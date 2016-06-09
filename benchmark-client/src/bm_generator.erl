%% Accepts socket connections and spawns a new proccess after each new connection established.

-module(bm_generator).
-behaviour(gen_server).

-export([
    start_link/0,
    add_pid/1
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

-define(DEFAULT_PORT, 5455).

-define(MYSQL_ID, "1234").
-define(MYSQL_HOST, "localhost").
-define(MYSQL_USER, "root").
-define(MYSQL_PASSWORD, "parola").
-define(MYSQL_DATABASE, "chat").

-define(NR_USERS, 5).
-define(NR_FRIENDSHIP, 5).
-define(NR_MESSAGES, 100).
-define(NR_GROUP_MESSAGES, 5).
-define(BATCH_SIZE, 2).

-record(state, {pids = [], usernames = []}).

%%%=================================================================================================
%%% API
%%%=================================================================================================
start_link() ->
    NrClients = 2,
    logger:info("bm_generator:start_link() ~p clients", [NrClients]),
    % Pid = spawn_link(bm_generator, init, [NrClients]),
    gen_server:start_link({local, bm_generator}, bm_generator, [NrClients], []).

add_pid(Pid) ->
    logger:debug("bm_generator:add_pid(~p)", [Pid]),
    gen_server:cast(bm_generator, {add_pid, Pid}).

%%%=================================================================================================
%%% Helper functions
%%%=================================================================================================
get_usernames(0, Usernames) ->
    Usernames;

get_usernames(NrUsers, Usernames) ->
    Name = generator:get_name(),
    NewUsernamesList = lists:append(Usernames, [Name]),
    get_usernames(NrUsers - 1, NewUsernamesList).

get_random_friendships(0, Friendships) ->
    Friendships;

get_random_friendships(NrFrienships, ExistingFriendships) ->
    Friendship = {random:uniform(?NR_USERS), random:uniform(?NR_USERS)},
    UpdatedFrienships = lists:append([Friendship], ExistingFriendships),
    get_random_friendships(NrFrienships - 1, UpdatedFrienships).

%%%=================================================================================================
%%% gen_server callbacks
%%%=================================================================================================
init(_Args) ->
    logger:debug("bm_generator:init()"),
    Usernames = get_usernames(?NR_USERS, []),
    logger:debug("bm_generator:init() Username List ~p", [Usernames]),
    lists:foreach(
        fun(Username) ->
            chat_client:start(Username, "parola" ++ Username)
        end,
        Usernames
    ),
    {ok, #state{pids = [], usernames = Usernames}}.

handle_call(OtherRequest, _From, State) ->
    logger:error("bm_generator:handle_cast() Unknown cast request <<~p>>", [OtherRequest]),
    {noreply, State}.

handle_cast({add_pid, Pid}, State) ->
    PidList = State#state.pids,
    NewPidList = lists:append(PidList, [Pid]),
    logger:debug("bm_generator:handle_cast() add_pid ~p to list ~p.", [Pid, PidList]),
    case  length(NewPidList) == ?NR_USERS of
        true ->
            gen_server:cast(bm_generator, register_users);
        false ->
            do_nothing
    end,
    {noreply, State#state{pids = NewPidList}};

handle_cast(register_users, State) ->
    PidList = State#state.pids,
    logger:debug("bm_generator:handle_cast() register_users ~p.", [PidList]),
    % This is seq
    lists:foreach(
        fun(Pid) ->
            Pid ! register
        end,
        PidList
    ),
    gen_server:cast(bm_generator, add_friends_to_mysql),
    {noreply, State};

handle_cast(add_friends_to_mysql, State) ->
    Pids = State#state.pids,
    Usernames = State#state.usernames,
    %% Add ?NR_FRIENDSHIP pairs (u1, u2) to mysql
    random:seed(erlang:now()),
    Friends = get_random_friendships(?NR_FRIENDSHIP, []),

    %% This is seq
    lists:foreach(
        fun({PidIndex, UserIndex}) ->
            Pid = lists:nth(PidIndex, Pids),
            User = lists:nth(UserIndex, Usernames),
            Pid ! {accept_friend_request, User}
        end,
        Friends
    ),
    {noreply, State};

handle_cast(Other, _State) ->
    logger:error("bm_generator:handle_cast() Unknown request ~p", [Other]).

handle_info(Info, State) ->
    gen_server:cast(self(), Info),
	{noreply, State}.

% terminate is called if a handle_* call returns stop
% bm_generator is brutally killed by chat_supervisor on shutdown
terminate(Reason, _State) ->
	logger:info("bm_generator:terminate() Terminating for reason: ~p", [Reason]),
	ok.

code_change(_OldVsn, State, _Extra) ->
	NewState = State,
	{ok, NewState}.
