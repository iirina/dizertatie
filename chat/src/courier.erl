%% Facilitates sending the messages and keeps track of the connected users.
%%
%% It implements the gen_server behaviour and keeps in the state a list of PIDs. Each PID
%% corresponds to the process that handles a socket. The list is updated when a new user connects
%% or disconnects. A message is sent to the process that handles a socket when an user message for
%% that socket is detected.

-module(courier).
-behaviour(gen_server).

%% API
-export([
    start_link/0,
    connected/2,
    disconnected/1,
    group_message/3,
    chat/4,
    server_msg/2
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
    update_latest_ets_table/0
]).

% -include("mysql_utils.hrl").
-include("macros.hrl").

%% The state of this gen_server is relevant only for the connected users.
%% Registered users that are offline can be fetch from the registration gen_server.
% -record(state, {name_to_pid = dict:new()}).

%%%=================================================================================================
%%% API
%%%=================================================================================================

%% Starts the courier server.
start_link() ->
    gen_server:start_link({local, courier}, courier, noargs, []).

connected(Name, SocketServerPid) ->
    gen_server:cast(courier, {connected, Name, SocketServerPid}).

disconnected(User) ->
    gen_server:call(courier, {disconnected, User}).

group_message(MsgId, From, Msg) ->
    gen_server:cast(courier, {group_msg, MsgId, From, lists:flatten(Msg)}).

chat(MsgId, FromUser, ToUser, Msg) ->
    gen_server:cast(courier, {chat, MsgId, FromUser, ToUser, lists:flatten(Msg)}).

%% Sends a message from the server.
server_msg(ToUser, Msg) ->
    gen_server:cast(courier, {server_msg, ToUser, lists:flatten(Msg)}).

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

%%%=================================================================================================
%%% gen_server callbacks
%%%=================================================================================================
init(_Args) ->
    % {User, Pid, Timestamp}
    ets:new(?LATEST_CONNECTED_USERS, [set, private, named_table]),
    ets:new(?LATEST_ACTIVE_USERS, [set, private, named_table]),
    ets:new(?ALL_CONNECTED_USERS, [set, private, named_table]),
    case timer:send_interval(?TIME_TO_UPDATE_COURIER_ETS_TABLES, update_latest_ets_table) of
        {ok, _Tref} ->
            logger:debug("courier:init() Timer set for update_latest_ets_table");
        {error, Error} ->
            logger:error("courieri:init() Timer was not set for update_latest_ets_table ~p",
                [Error])
    end,
	{ok, []}.

handle_call({disconnected, User}, {FromPid, _FromTag}, State) ->
    logger:info("courier:disconnected() User ~p disconnected (PID = ~p)", [User, FromPid]),
    ets:delete(?ALL_CONNECTED_USERS, User),
    ets:delete(?LATEST_CONNECTED_USERS, User),
    ets:delete(?LATEST_ACTIVE_USERS, User),
    {reply, ok, State};

handle_call(Request, From, State) ->
    logger:error("courier:handle_call(): Unknown request ~p from PID ~p", [Request, From]),
    {noreply, State}.

get_pid(User) ->
    Now = now(),
    logger:debug("courier:get_pid(~p)", [User]),
    Pid = case ets:lookup(?LATEST_ACTIVE_USERS, User) of
        [{_User, UserPid, _Timestamp}] ->
            UserPid;
        Other ->
            logger:debug("courier:get_pid(~p) Not latest active user, found ~p in ets, will search "
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
            logger:debug("courier:get_pid(~p) PID not found", [User]),
            no_pid;
        _Pid ->
            Object = {User, Pid, Now},
            logger:debug("courier:get_pid Pid ~p", [Pid]),
            ets:insert(?LATEST_ACTIVE_USERS, Object),
            case is_pid(Pid) of
                true ->
                    logger:debug("courier:get_pid ~p is pid", [Pid]),
                    Pid;
                false ->
                    logger:debug("courier:get_pid ~p is NOT pid", [Pid]),
                    list_to_pid(Pid)
            end
    end,
    logger:debug("courier:get_pid(~p) ~p", [User, ResultPid]),
    ResultPid.

handle_cast({connected, User, SocketServerPid}, State) ->
    logger:debug("courier:connected() Trying to connect user ~p", [User]),
    Now = now(),
    StrPid = pid_to_list(SocketServerPid),
    ets:insert(?LATEST_CONNECTED_USERS, {User, StrPid, Now}),
    ets:insert(?LATEST_ACTIVE_USERS, {User, StrPid, Now}),
    ets:insert(?ALL_CONNECTED_USERS, {User, StrPid}),
    logger:info("courier:connected() User ~p is now connected.", [User]),
    {noreply, State};

handle_cast({group_msg, MsgId, FromUser, Msg}, State) ->
    logger:debug(
        "courier:handle_cast() group_msg New group message ~p from user ~p", [Msg, FromUser]),
    case get_pid(FromUser) of
        no_pid ->
            ok;
        FromUserPid ->
            case roster_master:get_friends(FromUser) of
                {friends_list, []} ->
                    logger:debug("courier:handle_cast() group_msg Did not send message ~p to anyone"
                        ++ " because user ~p has no friends yet.", [Msg, FromUser]),
                    socket_handler:send_msg_to_pid(MsgId ++ "," ++ ?NO_FRIENDS, FromUserPid);
                {friends_list, FriendsList} ->
                    logger:debug("Found friend list: ~p for user ~p", [FriendsList, FromUser]),
                    lists:foreach(
                        fun(User) ->
                            case get_pid(User) of
                                no_pid ->
                                    logger:debug("courier:handle_cast() group_msg Could not send "
                                        ++ "message to ~p because s/he is not registered.", [User]);
                                UserPid ->
                                    socket_handler:send_msg_to_pid(Msg, UserPid)
                            end
                        end,
                        FriendsList
                    ),
                    socket_handler:send_msg_to_pid(MsgId ++ "," ++ ?GROUP_MESSAGE_SENT, FromUserPid)
            end
    end,
    {noreply, State};

handle_cast({chat, MsgId, FromUser, ToUser, Msg}, State) ->
    logger:debug("courier:handle_cast() chat New message for user ~p", [ToUser]),
    FromUserPid = get_pid(FromUser),
    case roster_master:are_friends(FromUser, ToUser) of
        true ->
            %% We need the pid of ToUser.
            case get_pid(ToUser) of
                no_pid ->
                    logger:debug("courier:handle_cast() chat Could not send message ~p to user"
                        ++ "~p because (s)he is not regitered on this chat.", [Msg, ToUser]),
                    socket_handler:send_msg_to_pid(
                        MsgId ++ "," ++ ?FRIEND_UNAVAILABLE, FromUserPid);
                ToPid ->
                    logger:debug("courier:handle_cast() chat Found PID ~p for user ~p.",
                        [ToPid, ToUser]),
                    socket_handler:send_msg_to_pid(Msg, ToPid),
                    socket_handler:send_msg_to_pid(MsgId ++ "," ++ ?MESSAGE_SENT, FromUserPid)
             end;
         false ->
             logger:debug("courier:handle_cast() chat Could not send message to ~p because (s)he is"
                " not a friend of ~p.", [ToUser, FromUser]),
             socket_handler:send_msg_to_pid(MsgId ++ "," ++ ?NOT_FRIENDS, FromUserPid);
        Other ->
            socket_handler:send_msg_to_pid(MsgId ++ "," ++ Other, FromUserPid)
    end,
    {noreply, State};

 handle_cast({server_msg, ToUser, Msg}, State) ->
     logger:debug("courier:handle_cast() server_msg New server message to user ~p", [ToUser]),
     case get_pid(ToUser) of
         no_pid ->
             logger:debug("courier:handle_cast() server_msg User ~p is not registered and message "
                ++ "will not be delievered.", [ToUser]);
         ToPid ->
             logger:debug(
                "courier:handle_cast() server_msg Found PID ~p for user ~p.", [ToPid, ToUser]),
             socket_handler:send_msg_to_pid(Msg, ToPid)

    end,
    {noreply, State};

handle_cast(OtherRequest, State) ->
    logger:error("courier:handle_cast() Unknown cast request <<~p>>", [OtherRequest]),
    {noreply, State}.

handle_info(update_latest_ets_table, State) ->
    update_latest_ets_table(),
    {noreply, State};

handle_info(Info, State) ->
	logger:error("courier:handle_info() Unknown info <<~p>>", [Info]),
	{noreply, State}.

% terminate is called if a handle_* call returns stop
% courier is brutally killed by chat_supervisor on shutdown
terminate(Reason, _State) ->
	logger:info("courier:terminate() Terminating for reason: ~p", [Reason]),
	ok.

code_change(_OldVsn, State, _Extra) ->
	NewState = State,
	{ok, NewState}.
