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
    server_msg/2
]).

%% gen_servfi  er callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    code_change/3,
    terminate/2
]).

%% The state of this gen_server is relevant only for the connected users.
%% Registered users that are offline can be fetch from the registration gen_server.
-record(state, {pid_to_name = dict:new(), name_to_pid = dict:new()}).

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

group_message(From, Msg) ->
    gen_server:cast(courier, {group_msg, From, lists:flatten(Msg)}).

chat(FromUser, ToUser, Msg) ->
    gen_server:cast(courier, {chat, FromUser, ToUser, lists:flatten(Msg)}).

%% Sends a message from the server.
server_msg(ToUser, Msg) ->
    gen_server:cast(courier, {server_msg, ToUser, lists:flatten(Msg)}).

%%%=================================================================================================
%%% gen_server callbacks
%%%=================================================================================================
init(_Args) ->
    NewState = #state{
        pid_to_name = dict:new(), name_to_pid = dict:new()},
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
            logger:info("courier:connected() User ~p is now connected.", [Name]),
            NewState = State#state{pid_to_name = NewPidToName, name_to_pid = NewNameToPid},
            {noreply, NewState}
    end;

handle_cast({group_msg, FromUser, Msg}, State) ->
    logger:debug(
        "courier:handle_cast() group_msg New group message ~p from user ~p", [Msg, FromUser]),
    case roster:get_friends(FromUser) of
        {friends_list, []} ->
            logger:debug("courier:handle_cast() group_msg Did not send message ~p to anyone "
                ++ "because user ~p has no friends yet.", [Msg, FromUser]);
        {friends_list, FriendsList} ->
            logger:debug("Found friend list: ~p for user ~p", [FriendsList, FromUser]),
            NameToPidDict = State#state.name_to_pid,
            lists:foreach(
                fun(User) ->
                    case dict:is_key(User, NameToPidDict) of
                        true ->
                            [UserPid] = dict:fetch(User, NameToPidDict),
                            socket_handler:send_msg_to_pid(Msg, UserPid);
                        false ->
                            logger:debug("courier:handle_cast() group_msg Could not send message to ~p "
                            ++ "because (s)he is not regitered on this chat.", [User])
                    end
                end,
                FriendsList
            )
    end,
    {noreply, State};

handle_cast({chat, FromUser, ToUser, Msg}, State) ->
    logger:debug("courier:handle_cast() chat New message ~p to ~p", [Msg, ToUser]),
    case roster:are_friends(FromUser, ToUser) of
        true ->
            logger:debug(
                "courier:handle_cast() chat Users ~p and ~p are friends.", [FromUser, ToUser]),
                %% We need the pid of ToUser.
                NameToPidDict = State#state.name_to_pid,
                case dict:is_key(ToUser, NameToPidDict) of
                    true ->
                        [ToPid] = dict:fetch(ToUser, NameToPidDict),
                        logger:debug("courier:handle_cast() chat Found PID ~p for user ~p.",
                            [ToPid, ToUser]),
                        socket_handler:send_msg_to_pid(Msg, ToPid);
                    false ->
                        logger:debug("courier:handle_cast() chat Could not send message ~p to user"
                            ++ "~p because (s)he is not regitered on this chat.", [Msg, ToUser])
                         %% TODO send message to FromUser that the message was not delivered.
                 end;
         false ->
             logger:debug("courier:handle_cast() chat Could not send message to ~p because (s)he is"
                " not a friend of ~p.", [ToUser, FromUser])
            %% TODO send message to FromUser that the message was not delivered.
    end,
    {noreply, State};

 handle_cast({server_msg, ToUser, Msg}, State) ->
     logger:debug("courier:handle_cast() server_msg New server message ~p to ~p", [Msg, ToUser]),
     NameToPidDict = State#state.name_to_pid,
     case dict:is_key(ToUser, NameToPidDict) of
         true ->
             [ToPid] = dict:fetch(ToUser, NameToPidDict),
             logger:debug(
                "courier:handle_cast() server_msg Found PID ~p for user ~p.", [ToPid, ToUser]),
             socket_handler:send_msg_to_pid(Msg, ToPid);
         false ->
             logger:debug("courier:handle_cast() server_msg User ~p is not registered and message "
                ++ "~p will not be delievered.", [ToUser, Msg])
    end,
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
