%% Keeps track of the chat users friends.
%% The state of the gen_server is an identifier of an ets table that stores tuples:
%% {User, Friend}
%% where User will be the key used in the ets table.
-module(roster).
-behaviour(gen_server).

%% API
-export([
    start_link/0,
    add_friend/2,
    remove_friend/2,
    get_friends/1,
    are_friends/2
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

-define(LATEST_TAB, latest_friends_tab).
-define(LATEST_ADDED_TAB, latest_added_friends_tab).
-define(ALL_TAB, all_friends_tab).

%%%=================================================================================================
%%% API
%%%=================================================================================================
%% Starts the roster server.
start_link() ->
    gen_server:start_link({local, roster}, roster, noargs, []).

add_friend(User1, User2) ->
    logger:debug("roster:add_friend() Add friends ~p and ~p.", [User1, User2]),
    gen_server:cast(roster, {add_friend, User1, User2}).

remove_friend(User1, User2) ->
    logger:debug("roster:remove_friend() Remove friends ~p and ~p.", [User1, User2]).

%% Gets a list of friends usernames.
get_friends(User) ->
    logger:debug("roster:get_friends() for user ~p.", [User]),
    gen_server:call(roster, {get_friends, User}).

%% Returns boolean.
are_friends(User1, User2) ->
    logger:debug(
        "roster:are_friends() Is ~s a friend of ~s? That is the question.", [User2, User1]),
    gen_server:call(roster, {are_friends, User1, User2}).

%% TODO add function to frequently drop new entries to mysql

%%%=================================================================================================
%%% helper functions
%%%=================================================================================================
is_name_of(_User, []) ->
    false;

%% For Elements in all_tab
is_name_of(User, [{_Key, CurrUser} | List]) ->
    case User == CurrUser of
        true -> true;
        false ->
            is_name_of(User, List)
    end;

%% For Elements in latest_tab
is_name_of(User, [{_Key, CurrUser, BooleanValue, _Timestamp} | List]) ->
    case User == CurrUser of
        true -> BooleanValue;
        false ->
            is_name_of(User, List)
    end.

update_latest_tab(User1, User2, NewTimestamp) ->
    case ets:lookup(?LATEST_TAB, User1) of
        {User1, User2, Value, _OldTimestamp} = Object ->
            ets:delete_object(?LATEST_TAB, Object),
            ets:insert(?LATEST_TAB, {User1, User2, Value, NewTimestamp});
        _Other ->
            ok
    end.

%%%=================================================================================================
%%% gen_server callbacks
%%%=================================================================================================
init(_Args) ->
    logger:debug("roster:init()"),
    %% TODO see if needed to integrate the option {heir,Pid,HeirData} | {heir,none}
    ets:new(?LATEST_TAB, [bag, private, named_table]),
    ets:new(?LATEST_ADDED_TAB, [bag, private, named_table]),
    %% TabAllFriends should be populated from mysql when a get on the list is made
    ets:new(?ALL_TAB, [bag, private, named_table]),
    {ok, []}.

%% Gets a list of friends usernames.
handle_call({get_friends, User}, {_FromPid, _FromTag}, State) ->
    %% TODO fetch data from mysql if the tab is empty
    FriendsTupleList = ets:lookup(?ALL_TAB, User),
    FriendsList = lists:map(fun({_Key, Friend}) -> Friend end, FriendsTupleList),
    {reply, {friends_list, FriendsList}, State};

handle_call({are_friends, User1, User2}, _From, State) ->
    Now = now(),
    case ets:lookup(?LATEST_TAB, User1) of
        [] ->
            %% TODO fetch data from mysql if the all tab is empty
            FriendsTuples = ets:lookup(?ALL_TAB, User1),
            IsFriend = is_name_of(User2, FriendsTuples),
            logger:debug("roster:handle_call() are_friends(~s,~s) ~p [looked in tab ~p]",
                [User1, User2, IsFriend, ?ALL_TAB]),
            ets:insert(?LATEST_TAB, {User1, User2, IsFriend, Now}),
            ets:insert(?LATEST_TAB, {User2, User1, IsFriend, Now}),
            {reply, IsFriend, State};
        FriendsTuples ->
            IsFriend = is_name_of(User2, FriendsTuples),
            logger:debug("roster:handle_call() are_friends(~s,~s) ~p [looked in tab ~p]",
                [User1, User2, IsFriend, ?LATEST_TAB]),
            logger:debug("roster:handle_call() are_friends(~s,~s) FriendsTuples ~p",
                [User1, User2, FriendsTuples]),
            update_latest_tab(User1, User2, Now),
            update_latest_tab(User2, User1, Now),
            {reply, IsFriend, State}
    end;

handle_call(OtherRequest, _From, State) ->
    logger:error("roster:handle_call() Unknown cast request ~p", [OtherRequest]),
    {noreply, State}.

delete_friendship_from_latest_tab(User, Friend) ->
    UserFriends = ets:lookup(?LATEST_TAB, User),
    lists:foreach(
        fun({_User, UserFriend, IsFriend, Timestamp}) ->
            case User == Friend of
                true ->
                    ets:delete_object(?LATEST_TAB, {User, UserFriend, IsFriend, Timestamp});
                false ->
                    ok
            end
        end,
        UserFriends
    ).

handle_cast({add_friend, User1, User2}, State) ->
    ets:insert(?ALL_TAB, {User1, User2}),
    ets:insert(?ALL_TAB, {User2, User1}),

    Now = now(),
    delete_friendship_from_latest_tab(User1, User2),
    delete_friendship_from_latest_tab(User2, User1),
    ets:insert(?LATEST_TAB, {User1, User2, true, Now}),
    ets:insert(?LATEST_TAB, {User2, User1, true, Now}),

    ets:insert(?LATEST_ADDED_TAB, {User1, User2, Now}),
    ets:insert(?LATEST_ADDED_TAB, {User2, User1, Now}),
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
