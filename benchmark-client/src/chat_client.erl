-module(chat_client).
-behaviour(gen_server).

%% API
-export([
    start/2, %% Username, Password (called by the benchmark)
    start_link/1  %% Args = {Username Password} called by the supervisor
]).

-export([
    read/2
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    code_change/3,
    terminate/2
]).

-define(DEFAULT_PORT, 5455).
-define(DEFAULT_SERVER, localhost).

-record(state, {username, password, socket, bmList}).

%%%=================================================================================================
%%% API
%%%=================================================================================================
%% Called by the bm_generator.
start(Username, Password) ->
    logger:debug("chat_client:start(~p, ~p)", [Username, Password]),
    supervisor:start_child(chat_client_sup, [{Username, Password}]). % simple_one_for_one

%% Called by the chat_client_sup supervisor to start the gen_server.
start_link(Args) ->
    % {Username, Password} = Args,
    logger:debug("chat_client:start_link() Starting one chat_client server..."),
    gen_server:start_link(chat_client, Args, []).

%%%=================================================================================================
%%% Helper functions
%%%=================================================================================================
send_tcp_msg(Socket, Msg) ->
    RequestTime = now(),
    Id = generator:get_id(),
    case gen_tcp:send(Socket, Msg) of
        ok ->
            logger:debug("chat_client:send_tcp_msg() message ~p sent at ~p.", [Msg, RequestTime]),
            [{Id, "sent", RequestTime}];
        {error, Error} ->
            logger:debug(
                "chat_client:handle_call() auth message not sent due to ~p", [Error]),
            []
    end.

get_id(Packet) ->
    Message = chat_utils:trim_string(erlang:binary_to_list(Packet)),
    Tokens = string:tokens(Message, ","),
    lists:nth(1, Tokens).

read(Socket, ClientPid) ->
        logger:debug("chat_client:read() Ready to read."),
        case gen_tcp:recv(Socket, 0) of
            {ok, Packet} ->
                logger:debug("chat_client:read() Reading for PID ~p, message ~p",
                    [ClientPid, Packet]),
                Now = now(),
                MsgId = get_id(Packet),
                gen_server:cast(ClientPid, {add_bm_msg, [{MsgId, "recv", Now}]});
            {error, closed} ->
                logger:debug("chat_client:loop() Stopped reading for in PID ~p. Socket closed.",
                    [ClientPid]),
                unlink(ClientPid),
                gen_server:cast(ClientPid, socket_closed)
        end.

%%%=================================================================================================
%%% gen_server callbacks
%%%=================================================================================================
init({Username, Password}) ->
    %% create a Socket to listen on the chat server and save this in the state with the username
    %% and password
    SocketOpts = [{active, false}, binary, {packet, 0}],
    Port = ?DEFAULT_PORT,
    Host = ?DEFAULT_SERVER,
    case gen_tcp:connect(Host, Port, SocketOpts) of
        {ok, Socket} ->
            logger:info("chat_client:init() User ~p connected to ~p:~p", [Username, Host, Port]),
            bm_generator:add_pid(self()),
            spawn_link(chat_client, read, [Socket, self()]),
            {ok, #state{username = Username, password = Password, socket = Socket, bmList = []}};
        {error, Reason} ->
            {error, Reason}
    end.

handle_call(Request, From, State) ->
    logger:error("chat_client:handle_call(): Unknown request ~p from PID ~p", [Request, From]),
    {noreply, State}.

handle_cast(register, State) ->
    Username = State#state.username,
    Password = State#state.password,
    Socket = State#state.socket,
    BmList = State#state.bmList,
    SocketMsg = "register," ++ Username ++ "," ++ Password,
    RequestTime = now(),
    Id = generator:get_id(),
    logger:info("chat_client:handle_cast() register ~p RequestTime ~p", [Username, RequestTime]),
    case gen_tcp:send(Socket, SocketMsg) of
        ok ->
            logger:debug("chat_client:handle_cast() register message sent."),
            {noreply, State#state{bmList = lists:append(BmList, [{Id, "sent", RequestTime}])}};
        {error, Error} ->
            logger:debug(
                "chat_client:handle_cast() register message not sent due to ~p", [Error])
    end;


handle_cast(auth, State) ->
    Username = State#state.username,
    Password = State#state.password,
    Socket = State#state.socket,
    BmList = State#state.bmList,
    SocketMsg = "auth," ++ Username ++ "," ++ Password,
    logger:info("chat_client:handle_call() auth ~p", [Username]),
    case send_tcp_msg(Socket, SocketMsg) of
        [] ->
            {noreply, State};
        List ->
            {noreply, State#state{bmList = lists:append(BmList, List)}}
    end;

handle_cast({chat, Msg, ToUsername}, State) ->
    Username = State#state.username,
    BmList = State#state.bmList,
    Socket = State#state.socket,
    SocketMsg = "chat," ++ ToUsername ++ "," ++ Msg,
    logger:info("chat_client:handle_cast() chat ~p from ~p to ~p.", [Msg, Username, ToUsername]),
    case send_tcp_msg(Socket, SocketMsg) of
        [] ->
            {noreply, State};
        List ->
            {noreply, State#state{bmList = lists:append(BmList, List)}}
    end;

handle_cast({group, Msg}, State) ->
    Username = State#state.username,
    BmList = State#state.bmList,
    Socket = State#state.socket,
    SocketMsg = "group," ++ Msg,
    logger:info("chat_client:handle_cast() group ~p from ~p.", [Msg, Username]),
    case send_tcp_msg(Socket, SocketMsg) of
        [] ->
            {noreply, State};
        List ->
            {noreply, State#state{bmList = lists:append(BmList, List)}}
    end;

handle_cast({add_bm_msg, List}, State) ->
    logger:info("chat_client:handle_cast() add_bm_msg ~p.", [List]),
    case List of
        [] ->
            {noreply, State};
        _Other ->
            BmList = State#state.bmList,
            {noreply, State#state{bmList = lists:append(BmList, List)}}
    end;

handle_cast(OtherRequest, State) ->
    logger:error("chat_client:handle_cast() Unknown cast request <<~p>>", [OtherRequest]),
    {noreply, State}.

handle_info(Info, State) ->
    gen_server:cast(self(), Info),
	{noreply, State}.

% terminate is called if a handle_* call returns stop
% chat_client is brutally killed by chat_supervisor on shutdown
terminate(Reason, _State) ->
	logger:info("chat_client:terminate() Terminating for reason: ~p", [Reason]),
	ok.

code_change(_OldVsn, State, _Extra) ->
	NewState = State,
	{ok, NewState}.
