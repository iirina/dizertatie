-module(socket_handler).
-behaviour(gen_server).

-include_lib("kernel/include/inet.hrl"). % needed for hostent structure

%% API
-export([
    send_msg/2,
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

%%%===================================================================
%%% API
%%%===================================================================

%% Called by the courier.
send_msg(Msg, Pid) ->
    logger:debug("socket_handler:send_msg() Message ~p and PID ~p", [Msg, Pid]),
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

deliver_message(Socket, String) ->
    logger:debug("socket_handler:deliver_message() Message ~s", [String]),
	case gen_tcp:send(Socket, String ++ "\n") of
        ok ->
            logger:debug("Message ~p was sent.", [String]);
        {error, Reason} ->
            logger:debug("Message ~p was not sent due to reason ~p", [String, Reason])
    end.


read(Socket, Name, Pid) ->
    logger:debug("socket_handler:read() Ready to read for user ~p", [Name]),
    case gen_tcp:recv(Socket, 0) of
        {ok, Packet} ->
            logger:debug("socket_handler:loop() Reading for user ~p and PID ~p, message ~p",
                [Name, Pid, Packet]),
            courier:message(chat_utils:format_message(Name, Packet)),
            read(Socket, Name, Pid);
        {error, closed} ->
            logger:debug(
                "socket_handler:loop() Stopped reading for user ~p in PID ~p. Socket closed.",
                    [Name, Pid]),
            unlink(Pid),
            gen_server:cast(Pid, socket_closed)
    end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(Args) ->
    {socket, Socket} = Args,
    logger:debug("socket_handler:init() Initializing socket_handler."),
    case auth:auth(Socket) of
        {user, User} ->
            logger:debug("socket_handler:init() Will try to connect user ~p.", [User]),
            case courier:connected(User) of
                ok ->
                    deliver_message(Socket, "Buna, " ++ User ++ "! Ai fost autentificat in chat." ++ "\n"),
                    %% Informs all chat users about the newly connected user.
                    courier:message(chat_utils:format_notification(User, "connected.")),
                    logger:debug("socket_handler:init() Identified user ~p.", [User]),
                    spawn_link(socket_handler, read, [Socket, User, self()]),
                    State = {Socket, User},
                    {ok, State};
                invalid ->
                    {stop, "Invalid user!"}
            end;
        {error, Error} ->
            {stop, Error}
    end.

handle_call(Request, From, State) ->
    {_Socket, Name} = State,
    chat_utils:log_message(Name, "Unknown call request ~p, PID = ~p", [Request, From]).

handle_cast({message, Msg}, {Socket, Name}=State) ->
    logger:debug("socket_handler:handle_cast() Message ~p from ~p Socket ~p", [Msg, Name, Socket]),
    deliver_message(Socket, Msg),
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
	courier:message(chat_utils:format_notification(Name,"disconnected")), % inform
	courier:disconnected(),
	ok.

code_change(_OldVsn, State, _Extra) ->
	NewState = State,
	{ok, NewState}.
