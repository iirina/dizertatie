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
-include("mnesia_utils.hrl").

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
%% Returns a list of the usernames of all the User's friends.
get_mnesia_friends(User) ->
    case get_friends_for_user(User) of
        {atomic, FriendshipList} ->
            lists:map(fun({friends, _User, Friend}) -> Friend end, FriendshipList);
        _Other ->
            []
    end.

%%%=================================================================================================
%%% gen_server callbacks
%%%=================================================================================================
init(_Args) ->
    logger:debug("roster:init()"),
    roster_master:add_worker_pid(self()),
    {ok, []}.

%% Gets a list of friends usernames. Queries mnesia since its the only place where all the friends
%% can be found.
handle_call({get_friends, User}, _From, State) ->
    {reply, {friends_list, get_mnesia_friends(User)}, State};

handle_call({are_friends, User1, User2}, _From, State) ->
    Now = now(),
    case ets:lookup(?LATEST_USED_FRIENDS_TAB, {User1, User2}) of
        [] ->
            %% TODO fetch data from mysql if the all tab is empty
            AreFriends = are_mnesia_friends(User1, User2),
            ets:insert(?LATEST_USED_FRIENDS_TAB, {{User1, User2}, AreFriends, Now}),
            ets:insert(?LATEST_USED_FRIENDS_TAB, {{User2, User1}, AreFriends, Now}),
            {reply, AreFriends, State};
        [{_AllegedFriendship, AreFriends, _Timestamp}] ->
            %% insert overrides old values for the same key.
            ets:insert(?LATEST_USED_FRIENDS_TAB, {{User1, User2}, AreFriends, Now}),
            ets:insert(?LATEST_USED_FRIENDS_TAB, {{User2, User1}, AreFriends, Now}),
            {reply, AreFriends, State}
    end;

handle_call(OtherRequest, _From, State) ->
    logger:error("roster:handle_call() Unknown cast request ~p", [OtherRequest]),
    {noreply, State}.

handle_cast({add_friend, User1, User2}, State) ->
    Now = now(),
    %% insert overrides old values for the same key.
    ets:insert(?LATEST_USED_FRIENDS_TAB, {{User1, User2}, true, Now}),
    ets:insert(?LATEST_USED_FRIENDS_TAB, {{User2, User1}, true, Now}),

    ets:insert(?LATEST_ADDED_TAB, {{User1, User2}, Now}),
    ets:insert(?LATEST_ADDED_TAB, {{User2, User1}, Now}),
    {noreply, State};

handle_cast(load_friends, State) ->
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
