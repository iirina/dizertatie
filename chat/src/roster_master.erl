%% Keeps track of all roster_master processes (children of roster_master_supervisor)
%% and redirects all requests to one of these processes (the one with the lowest load)

-module(roster_master).
-behaviour(gen_server).

%% API
-export([
    start_link/0,
    add_friend/2,
    remove_friend/2,
    get_friends/1,
    are_friends/2,
    add_worker_pid/1
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


%% Internally used. Exported to stop warnings.
-export([
    update_ets_tables/0,
    dump_mnesia_friends/0
]).

-include("macros.hrl").
-include("mnesia_utils.hrl").

-define(NR_WORKERS, 5).

-record(state, {worker_pids}).

%%%=================================================================================================
%%% API
%%%=================================================================================================
%% Starts the roster_master server.
start_link() ->
    gen_server:start_link({local, roster_master}, roster_master, noargs, []).

add_friend(User1, User2) ->
    logger:debug("roster_master:add_friend() Add friends ~p and ~p.", [User1, User2]),
    gen_server:cast(roster_master, {add_friend, User1, User2}).

remove_friend(User1, User2) ->
    logger:debug("roster_master:remove_friend() Remove friends ~p and ~p.", [User1, User2]).

%% Gets a list of friends usernames.
get_friends(User) ->
    logger:debug("roster_master:get_friends() for user ~p.", [User]),
    gen_server:call(roster_master, {get_friends, User}).

%% Returns boolean.
are_friends(User1, User2) ->
    logger:debug(
        "roster_master:are_friends() Is ~s a friend of ~s? That is the question.", [User2, User1]),
    gen_server:call(roster_master, {are_friends, User1, User2}).

add_worker_pid(Pid) ->
    gen_server:cast(roster_master, {add_worker_pid, Pid}).

%%%=================================================================================================
%%% helper functions
%%%=================================================================================================
start_workers(0) ->
    ok;

start_workers(NrWorkers) ->
    logger:debug("roster_master:start_workers() Starting new roster worker.."),
    roster:start(),
    start_workers(NrWorkers - 1).

get_first_element_of_queue_and_add_it_to_rear(Queue) ->
    case queue:is_empty(Queue) of
        true ->
            no_pid;
        false ->
            case queue:out(Queue) of
                {{value, Element}, ResultingQueue} ->
                    {Element, queue:in(Element, ResultingQueue)};
                _Other ->
                    no_pid
            end
    end.

has_timestamp_before(Object, Time, Tab) ->
    case Tab of
        ?LATEST_ADDED_TAB ->
            {{_User, _Friend}, Timestamp} = Object,
            Timestamp < Time;
        ?LATEST_USED_FRIENDS_TAB ->
            {{_User, _Friend}, _BooleanValue, Timestamp} = Object,
            Timestamp < Time
    end.

get_latest_before(_Tab, '$end_of_table', _Time, CollectedObjects) ->
    CollectedObjects;

get_latest_before(Tab, Key, Time, CollectedObjects) ->
    Objects = ets:lookup(Tab, Key),
    FilteredObjects = lists:filter(
        fun(Object) ->
            has_timestamp_before(Object, Time, Tab)
        end,
        Objects
    ),
    get_latest_before(Tab, Key, Time, lists:append(FilteredObjects, CollectedObjects)).

drop_friends_to_mnesia(FriendsList) ->
    lists:foreach(
        fun({{User, Friend}, _Timestamp}) ->
            insert_friendship(User, Friend)
        end,
        FriendsList).

remove_objects_from_ets(Objects, Tab) ->
    lists:foreach(
        fun(Object) ->
            ets:delete_object(Tab, Object)
        end,
        Objects).

get_seconds_ago({MacroSeconds, Seconds, MicroSeconds}) ->
    {MacroSeconds, Seconds - 10, MicroSeconds}.

update_ets_tables() ->
    %% We want to drop latest added friends before now() to mnesia and remove them from ets
    %% and remove from ets the latest used friends before now() - 10 seconds
    Now = now(),
    SecondsAgo = get_seconds_ago(Now),
    LatestAddedFriendsBefore =
        get_latest_before(?LATEST_ADDED_TAB, ets:first(?LATEST_ADDED_TAB), Now, []),
    LatestUsedFriendsBefore = get_latest_before(
        ?LATEST_USED_FRIENDS_TAB, ets:first(?LATEST_USED_FRIENDS_TAB), SecondsAgo, []),
    drop_friends_to_mnesia(LatestAddedFriendsBefore),
    remove_objects_from_ets(LatestAddedFriendsBefore, ?LATEST_ADDED_TAB),
    remove_objects_from_ets(LatestUsedFriendsBefore, ?LATEST_USED_FRIENDS_TAB).

dump_mnesia_friends() ->
    % mnesia:dump_tables([friends]).
    ok.

set_timers() ->
    case timer:send_interval(?TIME_TO_DROP_LATEST_ADDED_FRIENDS, update_ets_tables) of
        {ok, _DropTref} ->
            logger:debug("roster_master:init() Timer set for drop_latest_added_friends");
        {error, DropError} ->
            logger:error(
                "roster_master:init() Timer was not set for drop_latest_added_friends ~p", [DropError])
    end.
    % case timer:send_interval(?TIME_TO_DUMP_MNESIA_FRIENDS, dump_mnesia_friends) of
    %     {ok, _DumpTref} ->
    %         logger:debug("roster_master:init() Timer set for dump_mnesia_friends");
    %     {error, DumpError} ->
    %         logger:error("roster_master:init() Timer was not set for dump_mnesia_friends ~p", [DumpError])
    % end.

%%%=================================================================================================
%%% gen_server callbacks
%%%=================================================================================================

% The state is a queue of the PIDs of all roster workers.
init(_Args) ->
    logger:debug("roster_master:init()"),

    %% Create the ets tables that are used by the workers (notice the public property).
    ets:new(?LATEST_USED_FRIENDS_TAB,
        [set, public, named_table, {write_concurrency, true}, {read_concurrency, true}]),
    ets:new(?LATEST_ADDED_TAB,
        [set, public, named_table, {write_concurrency, true}, {read_concurrency, true}]),

    set_timers(),
    %% We start the workers now and wait for their PID to be sent async later.
    start_workers(?NR_WORKERS),
    {ok, #state{worker_pids = queue:new()}}.

handle_call(Request, _From, State) ->
    logger:debug("roster_master:handle_call() ~p", [Request]),
    case get_first_element_of_queue_and_add_it_to_rear(State#state.worker_pids) of
        {WorkerPid, UpdatedQueue} ->
            logger:debug("roster_master:handle_call() Redirecting Request ~p to PID ~p",
                [Request, WorkerPid]),
            WorkerResponse = gen_server:call(WorkerPid, Request),
            {reply, WorkerResponse, #state{worker_pids = UpdatedQueue}};
        _Other ->
            logger:debug("roster_master:handle_call() Could not find PID to redirect request ~p",
                [Request]),
            {noreply, State}
    end.

handle_cast({add_worker_pid, Pid}, State) ->
    logger:debug("roster_master:handle_cast() add_worker_pid ~p", [Pid]),
    {noreply, #state{worker_pids = queue:in(Pid, State#state.worker_pids)}};

handle_cast(Request, State) ->
    logger:debug("roster_master:handle_cast() ~p", [Request]),
    case get_first_element_of_queue_and_add_it_to_rear(State#state.worker_pids) of
        {WorkerPid, UpdatedQueue} ->
            logger:debug("roster_master:handle_cast() Redirecting Request ~p to PID ~p",
                [Request, WorkerPid]),
            gen_server:cast(WorkerPid, Request),
            {noreply, #state{worker_pids = UpdatedQueue}};
        _Other ->
            logger:debug("roster_master:handle_cast() Could not find PID to redirect request ~p",
                [Request]),
            {noreply, State}
    end.

handle_info(update_ets_tables, State) ->
    update_ets_tables(),
    {noreply, State};

handle_info(dump_mnesia_friends, State) ->
    dump_mnesia_friends(),
    {noreply, State};

handle_info(Request, State) ->
    logger:debug("roster_master:handle_info() ~p", [Request]),
    case get_first_element_of_queue_and_add_it_to_rear(State#state.worker_pids) of
        {WorkerPid, UpdatedQueue} ->
            logger:debug("roster_master:handle_info() Redirecting Request ~p to PID ~p",
                [Request, WorkerPid]),
            WorkerPid ! Request,
            {noreply, #state{worker_pids = UpdatedQueue}};
        _Other ->
            logger:debug("roster_master:handle_info() Could not find PID to redirect request ~p",
                [Request]),
            {noreply, State}
    end.

% terminate is called if a handle_* call returns stop
% roster_master is brutally killed by chat_supervisor on shutdown
terminate(Reason, _State) ->
	logger:info("roster_master:terminate() Terminating for reason ~p", [Reason]),
	ok.

code_change(_OldVsn, State, _Extra) ->
	NewState = State,
	{ok, NewState}.