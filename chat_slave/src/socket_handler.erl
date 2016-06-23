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

-include("macros.hrl").

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
read(Socket, Name, SocketServerPid) ->
    logger:debug(
        "socket_handler:read() Ready to read for socket_handler server ~p", [SocketServerPid]),
    case gen_tcp:recv(Socket, 0) of
        {ok, Packet} ->
            %% Pid corresponds to the gen_server that handles Socket.
            StringPacket = chat_utils:trim_string(erlang:binary_to_list(Packet)),
            Requests = string:tokens(StringPacket, "\n"),
            NewName = handle_request(Requests, Name, SocketServerPid),
            read(Socket, NewName, SocketServerPid);
        {error, closed} ->
            logger:debug(
                "socket_handler:loop() Stopped reading for user ~p in PID ~p. Socket closed.",
                    [Name, SocketServerPid]),
            unlink(SocketServerPid),
            gen_server:cast(SocketServerPid, socket_closed)
    end.

handle_request([], Name, _Pid) ->
    Name;

handle_request([Request | Requests], Name, Pid) ->
    case Name of
        undefined ->
            %% In this case, the Name is undefined, so auth is mandatory.
            logger:debug(
                "socket_handler:read() undefined name: Packet ~p, Pid ~p", [Request, Pid]),
            case chat_auth:handle_auth_packet(Request, Pid) of
                {user, User} ->
                    handle_request(Requests, User, Pid);
                _Other ->
                    handle_request(Requests, undefined, Pid)
            end;
        _Other ->
            %% Here, the user is authenticated as Name.
            handle_packet(Request, Name, Pid),
            handle_request(Requests, Name, Pid)
    end.

%% User1: trimite la server add_friend,User2
%%          - Serveru; trimite mesajul <<user requests to be your friend.>> catre User2
%% User2 primeste de la server <<user requests to be your friend>>
%%            - User2 trimite la server respond_to_friend_request,yes or respond_to_friend_request,no
%% Serverul citeste de la User2 mesajul respond_to_friend_request,boolean
%%            - daca e pozitiv, updateaza ambele liste de prietenii
%%            - daca e negativ, doar trimite la User1 mesajul <<user rejected your friend request.>>
handle_packet(Packet, User, GenServerPid) ->
    case get_action_type(Packet) of
        {group_message, Id, Message} ->
            logger:debug("socket_handler:handle_packet() Got action group_message id ~p.", [Id]),
            courier:group_message(GenServerPid, Id, User,
                chat_utils:format_message(User, Message, ?GROUP_MESSAGE_TOKEN));
        {chat, Id, To, Message} ->
            logger:debug("socket_handler:handle_packet() Got action chat."),
            courier:chat(
                GenServerPid, Id, User, To, chat_utils:format_message(User, Message, ?CHAT_TOKEN));
        {get_friends, Id} ->
            logger:debug("socket_handler:handle_packet() Got action get_friends."),
            {friends_list, FriendsList} = roster_master:get_friends(User),
            StringFriendList = string:join(FriendsList, ","),
            %% We send the friend list to the user that requested it.
            gen_server:cast(GenServerPid, {message, Id ++ ",friends:" ++ StringFriendList});
        {add_friend, Id, NewFriend} ->
            logger:debug("socket_handler:handle_packet() Got action add_friend ~p", [NewFriend]),
            courier:server_msg(NewFriend, User ++ ?FRIEND_REQUEST),
            gen_server:cast(GenServerPid, {message, Id ++ "," ++ ?FRIENDSHIP_REQUEST_SENT});
        {accept_friend_request, Id, UserRequesting} ->
            logger:debug("socket_handler:handle_packet() Got action accept_friend_request ~p",
                [UserRequesting]),
            roster_master:add_friend(User, UserRequesting),
            %% Tell the friend that User accepted the friend request.
            courier:server_msg(UserRequesting, User ++ ?FRIEND_REQUEST_ACCEPTED),
            gen_server:cast(GenServerPid, {message, Id ++ "," ++ ?FRIENDSHIP_STATUS_SENT});
        {reject_friend_request, Id, UserRequesting} ->
            logger:debug("socket_handler:handle_packet() Got action reject_friend_request ~p",
                [UserRequesting]),
            %% Tell the friend that User rejected the friend request.
            courier:server_msg(UserRequesting, User ++ ?FRIEND_REQUEST_REJECTED),
            gen_server:cast(GenServerPid, {message, Id ++ "," ++ ?FRIENDSHIP_STATUS_SENT});
        {remove_friend, Id, Friend} ->
            logger:debug("socket_handler:handle_packet() Got action remove_friend ~p", [Friend]),
            roster_master:remove_friend(User, Friend),
            gen_server:cast(GenServerPid, {message, Id ++ "," ++ ?FRIENDSHIP_STATUS_SENT});
        _Other ->
            courier:server_msg(User, "bad_request")
    end.

get_action_type(Request) ->
    % logger:debug("socket_handler:get_action_type() Request ~p", [Request]),
    Tokens = string:tokens(Request, ","),
    % logger:debug("socket_handler:get_action_type() Tokens ~p", [Tokens]),
    %% TODO verify Tokens has at least 2 element
    Id = lists:nth(1, Tokens),
    Action = lists:nth(2, Tokens),
    case Action of
        ?GROUP_MESSAGE_TOKEN ->
            %% TODO verify Tokens has 2 elements
            Message = lists:nth(3, Tokens),
            % logger:debug(
            %     "socket_handler:get_action_type() Action ~p and message ~p",
            %         [?GROUP_MESSAGE_TOKEN, Message]),
            {group_message, Id, Message};
        ?CHAT_TOKEN ->
            %% TODO verify Tokens has 3 elements
            To = lists:nth(3, Tokens),
            Message = lists:nth(4, Tokens),
            logger:debug(
                "socket_handler:get_action_type() Action ~p, for ~p and message ~p",
                    [?CHAT_TOKEN, To, Message]),
            {chat, Id, To, Message};
        ?GET_FRIENDS_TOKEN ->
            logger:debug("socket_handler:get_action_type() Action ~p", [?GET_FRIENDS_TOKEN]),
            {get_friends, Id};
        ?ADD_FRIEND_TOKEN ->
            %% TODO verify Tokens has 3 elements
            NewFriend = lists:nth(3, Tokens),
            logger:debug("socket_handler:get_action_type() Action ~p new friend ~p",
                [?ADD_FRIEND_TOKEN, NewFriend]),
            {add_friend, Id, NewFriend};
        ?ACCEPT_FRIEND_REQUEST_TOKEN ->
            %% TODO verify Tokens has 3 elements
            Friend = lists:nth(3, Tokens),
            logger:debug("socket_handler:get_action_type() Action ~p friend ~p",
                [?ACCEPT_FRIEND_REQUEST_TOKEN, Friend]),
            {accept_friend_request, Id, Friend};
        ?REJECT_FRIEND_REQUEST_TOKEN ->
            %% TODO verify Tokens has 3 elements
            Friend = lists:nth(3, Tokens),
            logger:debug("socket_handler:get_action_type() Action ~p friend ~p",
                [?REJECT_FRIEND_REQUEST_TOKEN, Friend]),
            {reject_friend_request, Id, Friend};
        ?REMOVE_FRIEND_TOKEN ->
            %% TODO verify Tokens has 3 elements
            Friend = lists:nth(3, Tokens),
            logger:debug("socket_handler:get_action_type() Action ~p friend ~p",
                [?ADD_FRIEND_TOKEN, Friend]),
            {remove_friend, Id, Friend};
        Other ->
            logger:debug("socket_handler:get_action_type() Got unknown action: ~p Request",
                [Other, Request]),
            other
    end.


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init(Args) ->
    {socket, Socket} = Args,
    logger:debug("socket_handler:init() Initializing socket_handler."),
    SocketServerPid = self(),
    spawn_link(socket_handler, read, [Socket, undefined, SocketServerPid]),
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
	courier:group_message(self(), "server", "server", chat_utils:format_notification(Name,"disconnected")),
	courier:disconnected(Name),
	ok.

code_change(_OldVsn, State, _Extra) ->
	NewState = State,
	{ok, NewState}.
