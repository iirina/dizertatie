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
    is_valid/1,
    disconnected/0,
    group_message/2,
    chat/3,
    add_friend/2,
    remove_friend/2,
    get_friend_list/1
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


-record(state, {pid_to_name = dict:new(), name_to_pid = dict:new(), friends = dict:new()}).

%%%=================================================================================================
%%% API
%%%=================================================================================================

%% Starts the courier server.
start_link() ->
    gen_server:start_link({local, courier}, courier, noargs, []).

connected(Name, SocketServerPid) ->
    gen_server:cast(courier, {connected, Name, SocketServerPid}).

is_valid(Name) ->
    gen_server:call(courier, {is_valid, Name}).

disconnected() ->
    gen_server:call(courier, disconnected).

%% Sends a message to all chat users.
group_message(From, Msg) ->
    gen_server:cast(courier, {group_msg, From, lists:flatten(Msg)}).

chat(FromUser, ToUser, Msg) ->
    gen_server:cast(courier, {chat, FromUser, ToUser, lists:flatten(Msg)}).

add_friend(ForUser, NewFriend) ->
    gen_server:cast(courier, {add_friend, ForUser, NewFriend}).

remove_friend(ForUser, Friend) ->
    gen_server:cast(courier, {remove_friend, ForUser, Friend}).

get_friend_list(ForUser) ->
    gen_server:call(courier, {get_friend_list, ForUser}).

is_name_of(_User, []) ->
    false;

is_name_of(User, [{Name, Pid} | FriendList]) ->
    case User == Name of
        true ->
            {pid, Pid};
        false ->
            is_name_of(User, FriendList)
    end.


%%%=================================================================================================
%%% gen_server callbacks
%%%=================================================================================================
init(_Args) ->
    NewState = #state{
        pid_to_name = dict:new(), name_to_pid = dict:new(), friends = dict:new()},
	{ok, NewState}.

handle_call({is_valid, Name}, {_FromPid, _FromTag}, State) ->
    NameToPid = State#state.name_to_pid,
    case dict:is_key(Name, NameToPid) of
        true ->
            % the name is already in use.
            logger:info("courier:is_valid() Name ~p is already in use.", [Name]),
            {reply, invalid, State};
        false ->
            logger:info("courier:is_valid() Name ~p is ok to use.", [Name]),
            {reply, ok, State}
    end;

handle_call(disconnected, {FromPid, _FromTag}, State) ->
    PidToName = State#state.pid_to_name,
    NameToPid = State#state.name_to_pid,
    case dict:is_key(FromPid, PidToName) of
        true ->
            Name = dict:fetch(FromPid, PidToName),
            logger:info("courier:disconnected() User ~p disconnected (PID = ~p)", [Name, FromPid]),
            NewPidToName = dict:erase(FromPid, PidToName),
            NewNameToPid = dict:erase(Name, NameToPid),
            NewState = State#state{pid_to_name = NewPidToName, name_to_pid = NewNameToPid},
            {reply, ok, NewState};
        false ->
            {reply, ok, State}
    end;

handle_call({get_friend_list, ForUser}, _From, State) ->
    FriendsDict = State#state.friends,
    case dict:is_key(ForUser, FriendsDict) of
        true ->
            FriendsNamePidList = dict:fetch(ForUser, FriendsDict),
            FriendsList = lists:map( fun({Name, _Pid}) -> Name end, FriendsNamePidList),
            {reply, FriendsList, State};
        false ->
            {reply, [], State}
    end;

handle_call(Request, From, State) ->
    logger:error("courier:handle_call(): Unknown request ~p from PID ~p", [Request, From]),
    {noreply, State}.

handle_cast({connected, Name, SocketServerPid}, State) ->
    logger:debug("courier:connected() Trying to connect user ~p", [Name]),
    PidToName = State#state.pid_to_name,
    NameToPid = State#state.name_to_pid,
    case dict:is_key(Name, NameToPid) of
        true ->
            % the name is already in use.
            logger:info("courier:connected() Name ~p is already in use.", [Name]),
            {noreply, State};
        false ->
            NewPidToName = dict:append(SocketServerPid, Name, PidToName),
            NewNameToPid = dict:append(Name, SocketServerPid, NameToPid),
            logger:info("courier:connected() Name ~p is now connected.", [Name]),
            NewState = State#state{pid_to_name = NewPidToName, name_to_pid = NewNameToPid},
            {noreply, NewState}
    end;

handle_cast({group_msg, FromUser, Msg}, State) ->
    logger:debug("courier:handle_cast() group_msg New group message ~p", [Msg]),
    FriendsDict = State#state.friends,
    case dict:is_key(FromUser, FriendsDict) of
        true ->
        FriendsList = dict:fetch(FromUser, FriendsDict),
        lists:foreach(
            fun(Element) ->
                {_Name, Pid} = Element,
                socket_handler:send_msg_to_pid(Msg, Pid)
            end,
            FriendsList
        )
    end,
    {noreply, State};

handle_cast({chat, FromUser, ToUser, Msg}, State) ->
    logger:debug("courier:handle_cast() chat New message ~p to ~p", [Msg, ToUser]),
    FriendsDict = State#state.friends,
    case dict:is_key(FromUser, FriendsDict) of
        true ->
            %% Check if ToUser is a friend of FromUser.
            FriendsList = dict:fetch(FromUser, FriendsDict),
            case is_name_of(ToUser, FriendsList) of
                {pid, ToPid} ->
                    logger:debug("courier:handle_cast() chat Found PID ~p for user ~p.", [ToPid, ToUser]),
                    socket_handler:send_msg_to_pid(Msg, ToPid);
                false ->
                    logger:debug(
                        "courier:handle_cast() chat Could not send message to ~p because (s)he is not a friend.",
                        [ToUser])
                    %% TODO send message to fromuser that the message was not delivered.
            end;
        false ->
            logger:debug(
                "courier:handle_cast() chat There is no user registered with this name ~p.",
                    [ToUser])
            %% TODO send message to fromuser that the message was not delivered.
    end,
    {noreply, State};

handle_cast({add_friend, ForUser, NewFriend}, State) ->
    logger:debug("courier:handle_cast() add_friend Will add new friend ~p for user ~p",
        [NewFriend, ForUser]),
    FriendsDict = State#state.friends,
    NameToPidDict = State#state.name_to_pid,
    case dict:is_key(ForUser, NameToPidDict) of
        true ->
            [NewFriendPid] = dict:fetch(NewFriend, NameToPidDict),
            NewFriendsDict = dict:append(ForUser, {NewFriend, NewFriendPid}, FriendsDict),
            logger:debug(
                "courier:handle_cast() add_friend Added friend ~p for ~p", [NewFriend, ForUser]),
            {noreply, State#state{friends = NewFriendsDict}};
        false ->
            {noreply, State}
    end;

handle_cast({remove_friend, _ForUser, _NewFriend}, State) ->
    %% TODO
    {noreply, State};


handle_cast(OtherRequest, State) ->
    logger:error("courier:handle_cast() Unknown cast request <<~p>>", [OtherRequest]),
    {noreply, State}.

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
