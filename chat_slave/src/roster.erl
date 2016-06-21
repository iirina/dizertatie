%% Keeps track of the chat users friends.
%% The state of the gen_server is an identifier of an ets table that stores tuples:
%% {User, Friend}
%% where User will be the key used in the ets table.
-module(roster).
-behaviour(gen_server).

%% API
-export([
    start/0,
    start_link/0
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

-include("macros.hrl").

%%%=================================================================================================
%%% API
%%%=================================================================================================
%% Called by roster_master to start a new worker.
start() ->
    StartingResult = supervisor:start_child(roster_sup, []),
    logger:debug("roster:start() ~p", [StartingResult]).

start_link() ->
    logger:debug("roster:start_link/0"),
    gen_server:start_link(roster, no_args, []).

%%%=================================================================================================
%%% helper functions
%%%=================================================================================================
%% Returns a set of the usernames of all the User's friends.
get_friends_on_current_node(User) ->
    EtsList = ets:lookup(?ALLTIME_FRIENDS_TAB, User),
    % add_element(Element, Set1)
    FriendsSet = lists:foldl(
        fun({_User, Friend}, CurrSet) ->
            sets:add_element(Friend, CurrSet)
        end,
        sets:new(),
        EtsList),
    FriendsSet.

get_all_friends(User) ->
    FriendsSetFromCurrentNode = get_friends_on_current_node(User),
    FriendsSetFromMaster =
        gen_server:call({master_roster_master, ?MASTER_NODE}, {get_friends, User}),
    AllFriendsSet = sets:union(FriendsSetFromCurrentNode, FriendsSetFromMaster),
    sets:to_list(AllFriendsSet).

are_users_friends(User1, User2) ->
    Now = now(),
    case ets:lookup(?LATEST_USED_FRIENDS_TAB, tuple_to_list({User1, User2})) of
        [] ->
            % [[{rufsen,dog,7}],[{brunte,horse,5}],[{ludde,dog,5}]]
            % 7> ets:match(T, {'_',dog,'$1'}).
            AreFriends = case ets:match_object(?ALLTIME_FRIENDS_TAB, {User1, User2}) of
                            [_FoundElement] ->
                                true;
                            _Other ->
                                false
                          end,
            ets:insert(?LATEST_USED_FRIENDS_TAB, {tuple_to_list({User1, User2}), AreFriends, Now}),
            ets:insert(?LATEST_USED_FRIENDS_TAB, {tuple_to_list({User2, User1}), AreFriends, Now}),
            AreFriends;
        [{_AllegedFriendship, AreFriends, _Timestamp}] ->
            %% insert overrides old values for the same key.
            ets:insert(?LATEST_USED_FRIENDS_TAB, {tuple_to_list({User1, User2}), AreFriends, Now}),
            ets:insert(?LATEST_USED_FRIENDS_TAB, {tuple_to_list({User2, User1}), AreFriends, Now}),
            AreFriends
    end.

%%%=================================================================================================
%%% gen_server callbacks
%%%=================================================================================================
init(_Args) ->
    logger:debug("roster:init()"),
    roster_master:add_worker_pid(self()),
    {ok, []}.

handle_call({get_friends, User}, _From, State) ->
    {reply, {friends_list, get_all_friends(User)}, State};

handle_call({are_friends, User1, User2}, _From, State) ->
    {reply, are_users_friends(User1, User2), State};

handle_call(OtherRequest, _From, State) ->
    logger:error("roster:handle_call() Unknown cast request ~p", [OtherRequest]),
    {noreply, State}.

handle_cast({add_friend, User1, User2}, State) ->
    Now = now(),
    %% insert overrides old values for the same key.
    ets:insert(?LATEST_USED_FRIENDS_TAB, {tuple_to_list({User1, User2}), true, Now}),
    ets:insert(?LATEST_USED_FRIENDS_TAB, {tuple_to_list({User2, User1}), true, Now}),

    ets:insert(?LATEST_ADDED_TAB, {tuple_to_list({User1, User2}), Now}),
    ets:insert(?LATEST_ADDED_TAB, {tuple_to_list({User2, User1}), Now}),

    ets:insert(?ALLTIME_FRIENDS_TAB, {User1, User2}),
    ets:insert(?ALLTIME_FRIENDS_TAB, {User2, User1}),
    {noreply, State};

handle_cast(OtherRequest, State) ->
    logger:error("roster:handle_cast() Unknown cast request ~p", [OtherRequest]),
    {noreply, State}.

handle_info(Info, State) ->
	logger:error("roster:handle_info() Unknown info ~p", [Info]),
	{noreply, State}.

% terminate is called if a handle_* call returns stop
% roster is brutally killed by chat_supervisor on shutdown
terminate(Reason, _State) ->
	logger:info("roster:terminate() Terminating for reason ~p", [Reason]),
	ok.

code_change(_OldVsn, State, _Extra) ->
	NewState = State,
	{ok, NewState}.
