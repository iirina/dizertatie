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
    connected/1,
    is_valid/1,
    disconnected/0,
    message/1
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


-record(state, {pid_to_name = dict:new(), name_to_pid = dict:new()}).

%%%===================================================================
%%% API
%%%===================================================================

%% Starts the courier server.
start_link() ->
    gen_server:start_link({local, courier}, courier, noargs, []).

connected(Name) ->
    gen_server:call(courier, {connected, Name}).

is_valid(Name) ->
    gen_server:call(courier, {is_valid, Name}).

disconnected() ->
    gen_server:call(courier, disconnected).

message(Msg) ->
    gen_server:cast(courier, {msg, lists:flatten(Msg)}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(_Args) ->
    NewState = #state{pid_to_name = dict:new(), name_to_pid = dict:new()},
	{ok, NewState}.

handle_call({connected, Name}, {FromPid, _FromTag}, State) ->
    logger:debug("courier:connected() Trying to connect user ~p", [Name]),
    PidToName = State#state.pid_to_name,
    NameToPid = State#state.name_to_pid,
    case dict:is_key(Name, NameToPid) of
        true ->
            % the name is already in use.
            logger:info("courier:connected() Name ~p is already in use.", [Name]),
            {reply, invalid, State};
        false ->
            NewPidToName = dict:append(FromPid, Name, PidToName),
            NewNameToPid = dict:append(Name, FromPid, NameToPid),
            NewState = #state{pid_to_name = NewPidToName, name_to_pid = NewNameToPid},
            logger:info("courier:connected() Name ~p is now connected.", [Name]),
            {reply, ok, NewState}
    end;

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
            NewState = #state{pid_to_name = NewPidToName, name_to_pid = NewNameToPid},
            {reply, ok, NewState};
        false ->
            {reply, ok, State}
    end;

handle_call(Request, From, State) ->
    logger:error("courier:handle_call(): Unknown request ~p from PID ~p", [Request, From]),
    {noreply, State}.

handle_cast({msg, Msg}, State) ->
    logger:debug("courier:msg() New message ~p", [Msg]),
    PidToNameList = dict:to_list(State#state.pid_to_name),
    lists:foreach(
        fun(Element) ->
            {Pid, _Name} = Element,
            socket_handler:send_msg(Msg, Pid)
        end,
        PidToNameList
    ),
    {noreply, State};

handle_cast(OtherRequest, State) ->
    logger:error("courier:handle_cast() Unknown cast request <<~p>>", [OtherRequest]),
    {noreply, State}.

handle_info(Info, State) ->
	logger:error("courier:handle_info() Unknown info <<~p>>", [Info]),
	{noreply, State}.

% terminate is called if a handle_* call returns stop
% dispatcher is brutally killed by chat_supervisor on shutdown
terminate(Reason, _State) ->
	logger:info("courier:terminate() Terminating for reason: ~p", [Reason]),
	ok.

code_change(_OldVsn, State, _Extra) ->
	NewState = State,
	{ok, NewState}.