-module(socket_handler).
-behaviour(gen_server).

-include_lib("kernel/include/inet.hrl"). % needed for hostent structure

%% API
-export([
    send_msg_to_pid/2,
    start/1,
    start_link/1
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

-export([
    read/3
]).

-define(GROUP_MESSAGE_TOKEN, "group").
-define(CHAT_TOKEN, "chat").
-define(GET_ONLINE_LIST_TOKEN, "get_online_list").

%%%===================================================================
%%% API
%%%===================================================================

%%  Sends a message to the user whose corresponding PID is Pid.
%% Called by the courier.
send_msg_to_pid(Msg, Pid) ->
    logger:debug("socket_handler:send_msg_to_pid() Message ~p and PID ~p", [Msg, Pid]),
    gen_server:cast(Pid, {message, Msg}).

%% Called by the socket acceptor.
start(Socket) ->
    logger:debug("socket_handler:start() Socket ~p", [Socket]),
    supervisor:start_child(socket_handler_sup, [{socket, Socket}]). % simple_one_for_one

%% Called by the supervisor to start the gen_server.
start_link(Args) ->
    logger:debug("socket_handler:start_link() Starting the socket_handler server..."),
    gen_server:start_link(socket_handler, Args, []).

%%%===================================================================
%%% Helper functions
%%%===================================================================

read(Socket, Name, Pid) ->
    logger:debug("socket_handler:read() Ready to read."),
    case gen_tcp:recv(Socket, 0) of
        {ok, Packet} ->
            logger:debug("socket_handler:loop() Reading for PID ~p, message ~p",
                [Pid, Packet]),
            %% Pid corresponds to the gen_server that handles Socket.
            case Name of
                undefined ->
                    %% In this case, the Name is undefined, so auth is mandatory.
                    case auth:handle_auth_packet(Packet, Pid) of
                        {user, User} ->
                            read(Socket, User, Pid);
                        _Other ->
                            read(Socket, undefined, Pid)
                    end;
                _Other ->
                    %% Here, the user is authenticated as Name.
                    handle_packet(Packet, Name, Pid),
                    read(Socket, Name, Pid)
            end;
        {error, closed} ->
            logger:debug(
                "socket_handler:loop() Stopped reading for user ~p in PID ~p. Socket closed.",
                    [Name, Pid]),
            unlink(Pid),
            gen_server:cast(Pid, socket_closed)
    end.

handle_packet(Packet, Name, _GenServerPid) ->
    case get_action_type(Packet) of
        {group_message, Message} ->
            logger:debug("socket_handler:handle_packet() Got action group_message."),
            courier:group_message(Name, chat_utils:format_message(Name, Message, ?GROUP_MESSAGE_TOKEN));
        {chat, To, Message} ->
            logger:debug("socket_handler:handle_packet() Got action chat."),
            courier:chat(Name, To, chat_utils:format_message(Name, Message, ?CHAT_TOKEN));
        _Other ->
            do_nothing
    end.

get_action_type(Packet) ->
    Request = chat_utils:trim_string(erlang:binary_to_list(Packet)),
    logger:debug("socket_handler:get_action_type() Request ~p", [Request]),
    Tokens = string:tokens(Request, ","),
    logger:debug("socket_handler:get_action_type() Tokens ~p", [Tokens]),
    %% TODO verify Tokens has at least 1 element
    Action = lists:nth(1, Tokens),
    case Action of
        ?GROUP_MESSAGE_TOKEN ->
            %% TODO verify Tokens has 2 elements
            Message = lists:nth(2, Tokens),
            logger:debug(
                "socket_handler:get_action_type() Action group_message and message ~p",
                    [Message]),
            {group_message, Message};
        ?CHAT_TOKEN ->
            %% TODO verify Tokens has 3 elements
            To = lists:nth(2, Tokens),
            Message = lists:nth(3, Tokens),
            logger:debug(
                "socket_handler:get_action_type() Action chat, for ~p and message ~p",
                    [To, Message]),
            {chat, To, Message};
        ?GET_ONLINE_LIST_TOKEN ->
            logger:debug("socket_handler:get_action_type() Action get_online_list"),
            get_online_list;
        Other ->
            logger:debug("socket_handler:get_action_type() Got unknown action: ~p", [Other])
    end.


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init(Args) ->
    {socket, Socket} = Args,
    logger:debug("socket_handler:init() Initializing socket_handler."),
    spawn_link(socket_handler, read, [Socket, undefined, self()]),
    %% We set the Name undefined for now and wait for it to be set properly when the
    %% authentication is successful.
    {ok, {Socket, undefined}}.

handle_call(Request, From, State) ->
    {_Socket, Name} = State,
    chat_utils:log_message(Name, "Unknown call request ~p, PID = ~p", [Request, From]).

handle_cast({set_user, User}, {Socket, _Name}) ->
    logger:debug("Setting User ~p for gen_server state.", [User]),
    {noreply, {Socket, User}};

%% Sends the received message through the socket corresponding to this process.
handle_cast({message, Msg}, {Socket, Name}=State) ->
    logger:debug(
        "socket_handler:handle_cast() Message ~p to ~p Socket ~p", [Msg, Name, Socket]),
	case gen_tcp:send(Socket, Msg ++ "\n") of
        ok ->
            logger:debug("Message ~p was sent to ~p.", [Msg, Name]);
        {error, Reason} ->
            logger:debug("Message ~p was not sent due to reason ~p", [Msg, Reason])
    end,
    {noreply, State};

handle_cast(socket_closed, {_Socket, Name}=State) ->
    chat_utils:log_message(Name, "socket closed"),
    {stop, normal, State};

handle_cast(OtherRequest, {_Socket, Name}=State) ->
    chat_utils:log_message(Name, "Unknown cast request ~w", [OtherRequest]),
    {noreply, State}.

handle_info(Info, State) ->
	{_Socket, Name} = State,
	chat_utils:log_message(Name, "got unknown message, info=~w", [Info]),
	{noreply, State}.

%% called when handle_cast returns stop.
%% when a shutdown occurs, all liasons are brutally killed by chat_liason_sup
terminate(Reason, State) ->
	{Socket, Name} = State,
	inet:close(Socket),
	chat_utils:log_message(Name, "terminating, pid=~w, reason=~w", [self(), Reason]),
	courier:group_message(chat_utils:format_notification(Name,"disconnected")), % inform
	courier:disconnected(),
	ok.

code_change(_OldVsn, State, _Extra) ->
	NewState = State,
	{ok, NewState}.
