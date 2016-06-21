%% Facilitates sending the messages and keeps track of the connected users.
%%
%% It implements the gen_server behaviour and keeps in the state a list of PIDs. Each PID
%% corresponds to the process that handles a socket. The list is updated when a new user connects
%% or disconnects. A message is sent to the process that handles a socket when an user message for
%% that socket is detected.

-module(master_courier).
-behaviour(gen_server).

%% API
-export([
    start_link/0,
    connected/2,
    disconnected/1,
    group_message/4,
    chat/5,
    server_msg/2,
    get_pid_of_user/1
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
    update_latest_ets_table/0,
    handle_send_group_msg/4,
    handle_chat/5,
    handle_server_msg/3
]).

% -include("mysql_utils.hrl").
-include("macros.hrl").

%% The state of this gen_server is relevant only for the connected users.
%% Registered users that are offline can be fetch from the master_registration gen_server.
% -record(state, {name_to_pid = dict:new()}).

%%%=================================================================================================
%%% API
%%%=================================================================================================

%% Starts the master_courier server.
start_link() ->
    gen_server:start_link({local, master_courier}, master_courier, noargs, []).

connected(Name, SocketServerPid) ->
    gen_server:cast(master_courier, {connected, Name, SocketServerPid}).

disconnected(User) ->
    gen_server:call(master_courier, {disconnected, User}).

group_message(FromUserPid, MsgId, From, Msg) ->
    gen_server:call(master_courier, {group_msg, FromUserPid, MsgId, From, lists:flatten(Msg)}).

chat(FromUserPid, MsgId, FromUser, ToUser, Msg) ->
    gen_server:cast(
        master_courier, {chat, FromUserPid, MsgId, FromUser, ToUser, lists:flatten(Msg)}).

%% Sends a message from the server.
server_msg(ToUser, Msg) ->
    gen_server:cast(master_courier, {server_msg, ToUser, lists:flatten(Msg)}).

get_pid_of_user(User) ->
    gen_server:call(master_courier, {get_pid_of_user, User}).

%%%=================================================================================================
%%% Helper functions
%%%=================================================================================================
get_obj_before_from_tabel(_Tab, '$end_of_table', _Time, CollectedObjects) ->
    CollectedObjects;

get_obj_before_from_tabel(Tab, Key, Time, CollectedObjects) ->
    [{User, Pid, Timestamp}] = ets:lookup(Tab, Key),
    case Timestamp < Time of
        true ->
            get_obj_before_from_tabel(Tab, ets:next(Tab, Key), Time,
                lists:append(CollectedObjects, [{User, Pid, Timestamp}]));
        false ->
            get_obj_before_from_tabel(Tab, ets:next(Tab, Key), Time, CollectedObjects)
    end.

remove_from_tabel(Tab, Objects) ->
    lists:foreach(
        fun(Object) ->
            ets:delete_object(Tab, Object)
        end,
        Objects).

seconds_ago({Macro, Sec, Micro}) ->
    {Macro, Sec - 10, Micro}.

update_latest_ets_table() ->
    Timestamp = seconds_ago(now()),
    LatestConnected = get_obj_before_from_tabel(
        ?LATEST_CONNECTED_USERS, ets:first(?LATEST_CONNECTED_USERS), Timestamp, []),
    LatestActive = get_obj_before_from_tabel(
        ?LATEST_ACTIVE_USERS, ets:first(?LATEST_ACTIVE_USERS), Timestamp, []),
    remove_from_tabel(?LATEST_CONNECTED_USERS, LatestConnected),
    remove_from_tabel(?LATEST_ACTIVE_USERS, LatestActive).


get_pid_on_current_node(User) ->
    Now = now(),
    logger:debug("master_courier:get_pid(~p)", [User]),
    Pid = case ets:lookup(?LATEST_ACTIVE_USERS, User) of
        [{_User, UserPid, _Timestamp}] ->
            UserPid;
        Other ->
            logger:debug("master_courier:get_pid(~p) Not latest active user, found ~p in ets, will search "
                ++ "all connected users", [User, Other]),
            case ets:lookup(?ALL_CONNECTED_USERS, User) of
                [{_User, UserPid}] ->
                    UserPid;
                _Other ->
                    no_pid
            end
    end,
    ResultPid = case Pid of
        no_pid ->
            logger:debug("master_courier:get_pid(~p) PID not found", [User]),
            no_pid;
        _Pid ->
            Object = {User, Pid, Now},
            logger:debug("master_courier:get_pid Pid ~p", [Pid]),
            ets:insert(?LATEST_ACTIVE_USERS, Object),
            case is_pid(Pid) of
                true ->
                    logger:debug("master_courier:get_pid ~p is pid", [Pid]),
                    Pid;
                false ->
                    logger:debug("master_courier:get_pid ~p is NOT pid", [Pid]),
                    list_to_pid(Pid)
            end
    end,
    logger:debug("master_courier:get_pid(~p) ~p", [User, ResultPid]),
    ResultPid.

get_pid_on_nodes([], _RequestNode, _User) ->
    no_pid;

get_pid_on_nodes([RequestNode | Nodes], RequestNode, User) ->
    get_pid_on_nodes(Nodes, RequestNode, User);

get_pid_on_nodes([?MASTER_NODE | Nodes], RequestNode, User) ->
    get_pid_on_nodes(Nodes, RequestNode, User);

get_pid_on_nodes([Node | Nodes], RequestNode, User) ->
    case gen_server:call({courier, Node}, {get_pid_of_user, User}) of
        no_pid ->
            get_pid_on_nodes(Nodes, RequestNode, User);
        Pid ->
            Pid
    end.

get_pid(User, RequestPid) ->
    case get_pid_on_current_node(User) of
        no_pid ->
            RequestNode = node(RequestPid),
            logger:debug("master_courier:get_pid(~p, ~p) Node ~p", [User, RequestPid, RequestNode]),
            Nodes = pool:get_nodes(),
            case get_pid_on_nodes(Nodes, RequestNode, User) of
                no_pid ->
                    no_pid;
                RemotePid ->
                    Now = now(),
                    StrPid = pid_to_list(RemotePid),
                    ets:insert(?LATEST_CONNECTED_USERS, {User, StrPid, Now}),
                    ets:insert(?LATEST_ACTIVE_USERS, {User, StrPid, Now}),
                    ets:insert(?ALL_CONNECTED_USERS, {User, StrPid}),
                    RemotePid
            end;
        Pid ->
            Pid
    end.

handle_send_group_msg(FromUser, FromUserPid, Msg, MsgId) ->
    logger:debug(
        "master_courier:handle_cast() group_msg New group message ~p from user ~p", [Msg, FromUser]),
    case master_roster_master:get_friends(FromUser) of
        {friends_list, []} ->
            logger:debug("master_courier:handle_cast() group_msg Did not send message ~p to anyone"
                ++ " because user ~p has no friends yet.", [Msg, FromUser]),
            socket_handler:send_msg_to_pid(MsgId ++ "," ++ ?NO_FRIENDS, FromUserPid);
        {friends_list, FriendsList} ->
            logger:debug("Found friend list: ~p for user ~p", [FriendsList, FromUser]),
            lists:foreach(
                fun(User) ->
                    case get_pid(User, FromUserPid) of
                        no_pid ->
                            logger:debug("master_courier:handle_cast() group_msg Could not send "
                                ++ "message to ~p because s/he is not registered.", [User]);
                        UserPid ->
                            socket_handler:send_msg_to_pid(Msg, UserPid)
                    end
                end,
                FriendsList
            ),
            socket_handler:send_msg_to_pid(MsgId ++ "," ++ ?GROUP_MESSAGE_SENT, FromUserPid)
    end.

handle_chat(FromUser, FromUserPid, ToUser, Msg, MsgId) ->
    logger:debug("master_courier:handle_cast() chat New message for user ~p", [ToUser]),
    case master_roster_master:are_friends(FromUser, ToUser) of
        true ->
            %% We need the pid of ToUser.
            case get_pid(ToUser, FromUserPid) of
                no_pid ->
                    logger:debug("master_courier:handle_cast() chat Could not send message ~p to user"
                        ++ "~p because (s)he is not regitered on this chat.", [Msg, ToUser]),
                    socket_handler:send_msg_to_pid(
                        MsgId ++ "," ++ ?FRIEND_UNAVAILABLE, FromUserPid);
                ToPid ->
                    logger:debug("master_courier:handle_cast() chat Found PID ~p for user ~p.",
                        [ToPid, ToUser]),
                    socket_handler:send_msg_to_pid(Msg, ToPid),
                    socket_handler:send_msg_to_pid(MsgId ++ "," ++ ?MESSAGE_SENT, FromUserPid)
             end;
         false ->
             logger:debug("master_courier:handle_cast() chat Could not send message to ~p because (s)he is"
                " not a friend of ~p.", [ToUser, FromUser]),
             socket_handler:send_msg_to_pid(MsgId ++ "," ++ ?NOT_FRIENDS, FromUserPid);
        Other ->
            socket_handler:send_msg_to_pid(MsgId ++ "," ++ Other, FromUserPid)
    end.

handle_server_msg(ToUser, FromUserPid, Msg) ->
    logger:debug("master_courier:handle_cast() server_msg New server message to user ~p", [ToUser]),
    case get_pid(ToUser, FromUserPid) of
        no_pid ->
            logger:debug("master_courier:handle_cast() server_msg User ~p is not registered and "
                "message will not be delievered.", [ToUser]);
        ToPid ->
            logger:debug(
               "master_courier:handle_cast() server_msg Found PID ~p for user ~p.", [ToPid, ToUser]),
            socket_handler:send_msg_to_pid(Msg, ToPid)

   end.

%%%=================================================================================================
%%% gen_server callbacks
%%%=================================================================================================
init(_Args) ->
    % {User, Pid, Timestamp}
    ets:new(?LATEST_CONNECTED_USERS, [set, public, named_table]),
    ets:new(?LATEST_ACTIVE_USERS, [set, public, named_table]),
    ets:new(?ALL_CONNECTED_USERS, [set, public, named_table]),
    case timer:send_interval(?TIME_TO_UPDATE_COURIER_ETS_TABLES, update_latest_ets_table) of
        {ok, _Tref} ->
            logger:debug("master_courier:init() Timer set for update_latest_ets_table");
        {error, Error} ->
            logger:error("master_courier:init() Timer was not set for update_latest_ets_table ~p",
                [Error])
    end,
	{ok, []}.

handle_call({get_pid_of_user, User}, {FromPid, _Tag}, State) ->
    {reply, get_pid(User, FromPid), State};

handle_call({disconnected, User}, {FromPid, _FromTag}, State) ->
    logger:info("master_courier:disconnected() User ~p disconnected (PID = ~p)", [User, FromPid]),
    ets:delete(?ALL_CONNECTED_USERS, User),
    ets:delete(?LATEST_CONNECTED_USERS, User),
    ets:delete(?LATEST_ACTIVE_USERS, User),
    {reply, ok, State};

handle_call({group_msg, FromUserPid, MsgId, FromUser, Msg}, _From, State) ->
    % handle_send_group_msg()
    spawn(master_courier, handle_send_group_msg, [FromUser, FromUserPid, Msg, MsgId]),
    {reply, ok, State};

handle_call({chat, FromUserPid, MsgId, FromUser, ToUser, Msg}, _From, State) ->
    % handle_chat()
    spawn(master_courier, handle_chat, [FromUser, FromUserPid, ToUser, Msg, MsgId]),
    {reply, ok, State};

handle_call({server_msg, ToUser, Msg}, {FromUserPid, _Tag}, State) ->
    % handle_server_msg()
    spawn(master_courier, handle_server_msg, [ToUser, FromUserPid, Msg]),
   {reply, ok, State};

handle_call(Request, From, State) ->
    logger:error("master_courier:handle_call(): Unknown request ~p from PID ~p", [Request, From]),
    {noreply, State}.

handle_cast({connected, User, SocketServerPid}, State) ->
    logger:debug("master_courier:connected() Trying to connect user ~p", [User]),
    Now = now(),
    StrPid = pid_to_list(SocketServerPid),
    ets:insert(?LATEST_CONNECTED_USERS, {User, StrPid, Now}),
    ets:insert(?LATEST_ACTIVE_USERS, {User, StrPid, Now}),
    ets:insert(?ALL_CONNECTED_USERS, {User, StrPid}),
    logger:info("master_courier:connected() User ~p is now connected.", [User]),
    {noreply, State};

handle_cast(OtherRequest, State) ->
    logger:error("master_courier:handle_cast() Unknown cast request <<~p>>", [OtherRequest]),
    {noreply, State}.

handle_info(update_latest_ets_table, State) ->
    update_latest_ets_table(),
    {noreply, State};

handle_info(Info, State) ->
	logger:error("master_courier:handle_info() Unknown info <<~p>>", [Info]),
	{noreply, State}.

% terminate is called if a handle_* call returns stop
% master_courier is brutally killed by master_chat_supervisor on shutdown
terminate(Reason, _State) ->
	logger:info("master_courier:terminate() Terminating for reason: ~p", [Reason]),
	ok.

code_change(_OldVsn, State, _Extra) ->
	NewState = State,
	{ok, NewState}.
