-module(chat_client).
-behaviour(gen_server).

%% API
-export([
    start/4, %% Username, Password (called by the benchmark)
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

-define(DEFAULT_PORT, 5400).
-define(DEFAULT_SERVER, localhost).

-define(MYSQL_ID, "1234").
-define(INSERT_BENCHMARK_INTO_MYSQL, "insert into benchmark values ").

%% Time expressed in ms.
-define(TIME_TO_DROP_BENCHMARK, 10 * 1000).

-record(state, {username, password, socket, bmList}).

%%%=================================================================================================
%%% API
%%%=================================================================================================
%% Called by the bm_generator.
start(Username, Password, Host, Port) ->
    logger:debug("chat_client:start(~p, ~p, ~p, ~p)", [Username, Password, Host, Port]),
    supervisor:start_child(chat_client_sup, [{Username, Password, Host, Port}]). % simple_one_for_one

%% Called by the chat_client_sup supervisor to start the gen_server.
start_link(Args) ->
    % {Username, Password} = Args,
    logger:debug("chat_client:start_link() Starting one chat_client server..."),
    gen_server:start_link(chat_client, Args, []).

%%%=================================================================================================
%%% Helper functions
%%%=================================================================================================
send_tcp_msg(Socket, Id, TypOfMsg, Msg) ->
    case gen_tcp:send(Socket, Msg) of
        ok ->
            RequestTime = now(),
            [{Id, "sent", TypOfMsg, RequestTime, chat_utils:trim_string(Msg)}];
        {error, _Error} ->
            []
    end.

get_id(Packet) ->
    Message = chat_utils:trim_string(Packet),
    Tokens = string:tokens(Message, ","),
    StringId = lists:nth(1, Tokens),
    case string:to_integer(StringId) of
        {error, _Reason} ->
            false;
        {_IntId, []} ->
            %% the correct one
            StringId;
        _Other ->
            false
    end.


handle_requests(Requests, Timestamp, ClientPid) ->
    lists:foreach(
        fun(Request) ->
            case get_id(Request) of
                false ->
                    ok;
                MsgId ->
                    logger:debug("chat_client:handle_requests ~p", [Request]),
                    gen_server:cast(ClientPid,
                        {add_bm_msg, [{MsgId, "recv", "n/a", Timestamp, Request}]})
            end
        end,
        Requests
    ).

read(Socket, ClientPid) ->
        logger:debug("chat_client:read() Ready to read."),
        case gen_tcp:recv(Socket, 0) of
            {ok, Packet} ->
                Now = now(),
                StringPacket = chat_utils:trim_string(binary_to_list(Packet)),
                Requests = string:tokens(StringPacket, "\n"),
                handle_requests(Requests, Now, ClientPid),
                read(Socket, ClientPid);
            {error, closed} ->
                logger:info("chat_client:loop() Stopped reading for in PID ~p. Socket closed.",
                    [ClientPid]),
                unlink(ClientPid),
                gen_server:cast(ClientPid, socket_closed)
        end.

add_timer() ->
    case timer:send_interval(?TIME_TO_DROP_BENCHMARK, drop_benchmark) of
        {ok, _Tref} ->
            logger:debug("chat_client:add_timer() Timer set for drop_benchmark");
        {error, Error} ->
            logger:error(
                "chat_client:add_timer() Timer was not set for drop_benchmark ~p", [Error])
    end.

get_string_timestamp(Timestamp) ->
    {Mega,Sec,Micro} = Timestamp,
    IntTimestamp = (Mega * 1000000 + Sec) * 1000000 + Micro,
    integer_to_list(IntTimestamp).

maybe_add_comma(CurrString) ->
    case CurrString of
        "" -> "";
        _Other -> CurrString ++ ","
    end.

get_elements_before([], Result, _Timestamp) ->
    Result;

get_elements_before(
    [{Id, Type, ReqType, CurrTimestamp, Msg} | RestOfList], {CurrString, CurrList}, Timestamp) ->
    case CurrTimestamp < Timestamp of
        true ->
            NewString = maybe_add_comma(CurrString) ++ " (\"" ++ Id ++ "\", \"" ++
                Type ++ "\", \"" ++ get_string_timestamp(CurrTimestamp) ++ "\", \"" ++ Msg ++
                    "\", \"" ++ ReqType ++ "\")",
            NewList = lists:append(CurrList, [{Id, Type, ReqType, CurrTimestamp, Msg}]),
            get_elements_before(RestOfList, {NewString, NewList}, Timestamp);
        false ->
            ok
    end.

%%%=================================================================================================
%%% gen_server callbacks
%%%=================================================================================================
init({Username, Password, Host, Port}) ->
    %% create a Socket to listen on the chat server and save this in the state with the username
    %% and password
    logger:info("chat_client:init() User ~p will connect to ~p:~p", [Username, Host, Port]),
    SocketOpts = [{active, false}, binary, {packet, 0}],
    case gen_tcp:connect(Host, Port, SocketOpts) of
        {ok, Socket} ->
            logger:info("chat_client:init() User ~p connected to ~p:~p", [Username, Host, Port]),
            bm_generator:add_pid(self()),
            spawn_link(chat_client, read, [Socket, self()]),
            add_timer(),
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
    Id = generator:get_id(),
    SocketMsg = Id ++ ",register," ++ Username ++ "," ++ Password ++ "\n",
    case gen_tcp:send(Socket, SocketMsg) of
        ok ->
            RequestTime = now(),
            Msg = chat_utils:trim_string(SocketMsg),
            {noreply, State#state{bmList =
                lists:append(BmList, [{Id, "sent", "register", RequestTime, Msg}])}};
        {error, Error} ->
            logger:error(
                "chat_client:handle_cast() register message not sent due to ~p", [Error])
    end;


handle_cast(auth, State) ->
    Username = State#state.username,
    Password = State#state.password,
    Socket = State#state.socket,
    BmList = State#state.bmList,
    Id = generator:get_id(),
    SocketMsg = Id ++ ",auth," ++ Username ++ "," ++ Password ++ "\n",
    logger:info("chat_client:handle_call() auth ~p", [Username]),
    case send_tcp_msg(Socket, Id, "auth", SocketMsg) of
        [] ->
            {noreply, State};
        List ->
            {noreply, State#state{bmList = lists:append(BmList, List)}}
    end;

handle_cast({chat, Msg, ToUsername}, State) ->
    Username = State#state.username,
    BmList = State#state.bmList,
    Socket = State#state.socket,
    Id = generator:get_id(),
    SocketMsg = Id ++ ",chat," ++ ToUsername ++ "," ++ Msg ++ "\n",
    logger:info("chat_client:handle_cast() chat from ~p to ~p.", [Username, ToUsername]),
    case send_tcp_msg(Socket, Id, "chat", SocketMsg) of
        [] ->
            {noreply, State};
        List ->
            {noreply, State#state{bmList = lists:append(BmList, List)}}
    end;

handle_cast({group, Msg}, State) ->
    Username = State#state.username,
    BmList = State#state.bmList,
    Socket = State#state.socket,
    Id = generator:get_id(),
    SocketMsg = Id ++ ",group," ++ Msg ++ "\n",
    logger:info("chat_client:handle_cast() group from ~p.", [Username]),
    case send_tcp_msg(Socket, Id,"group", SocketMsg) of
        [] ->
            {noreply, State};
        List ->
            {noreply, State#state{bmList = lists:append(BmList, List)}}
    end;

handle_cast({accept_friend_request, Friend}, State) ->
    Username = State#state.username,
    BmList = State#state.bmList,
    Socket = State#state.socket,
    Id = generator:get_id(),
    SocketMsg = Id ++ ",accept_friend_request," ++ Friend ++ "\n",
    logger:info("chat_client:handle_cast() accept_friend_request ~p for ~p.", [Friend, Username]),
    case send_tcp_msg(Socket, Id, "accept_friend_request", SocketMsg) of
        [] ->
            {noreply, State};
        List ->
            {noreply, State#state{bmList = lists:append(BmList, List)}}
    end;

handle_cast({add_bm_msg, List}, State) ->
    % logger:info("chat_client:handle_cast() add_bm_msg ~p.", [List]),
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

handle_info(drop_benchmark, State) ->
    BmList = State#state.bmList,
    Now = now(),
    Empty = {"", []},
    {StringBm, BmListBefore} = get_elements_before(BmList, Empty, Now),
    case BmListBefore of
        [] ->
            {noreply, State};
        _Other ->
            NewList = lists:foldl(
                fun(Element, BmListIn) ->
                    lists:delete(Element, BmListIn)
                end,
                BmList,
                BmListBefore
            ),
            MySqlInsertCommand = ?INSERT_BENCHMARK_INTO_MYSQL ++ StringBm,
            _Result = p1_mysql:fetch(?MYSQL_ID, MySqlInsertCommand, infinity),
            % logger:debug("chat_client:handle_info() drop_benchmark Command ~p",
            %     [MySqlInsertCommand]),
            {noreply, State#state{bmList = NewList}}
    end;


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
