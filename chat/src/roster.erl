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
    are_friends/2,
    load_friends/0
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

-define(LATEST_USED_FRIENDS_TAB, latest_friends_tab).
-define(LATEST_ADDED_TAB, latest_added_friends_tab).
-define(ALL_FRIENDS_TAB, all_friends_tab).

%% Time expressed in milliseconds.
-define(TIME_TO_DROP_LATEST_ADDED_FRIENDS, 10 * 1000). %% 1 * 60 * 1000
-define(TIME_TO_UPDATE_LATEST_USED_FRIENDS, 10 * 1000). %% 5 * 60 * 1000

-define(MYSQL_ID, "1234").
-define(FETCH_ALL_FRIENDS_MYSQL, "select * from friends").
-define(INSERT_FRIENDS_INTO_MYSQL, "insert into friends values ").

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

load_friends() ->
    logger:debug("registration:load_users()"),
    gen_server:cast(roster, load_friends).


%%%=================================================================================================
%%% helper functions
%%%=================================================================================================
fetch_friends_from_mysql() ->
    case p1_mysql:fetch(?MYSQL_ID, ?FETCH_ALL_FRIENDS_MYSQL) of
        {data, {p1_mysql_result, _FieldList, UsersRetrieved, _Number, _List}} ->
            logger:debug(
                "roster:fetch_friends_from_mysql() ~p", [UsersRetrieved]),
            UsersRetrieved;
        _Other ->
            []
    end.

% get_latest_added_friends_before
% get_latest_used_friends_before

get_latest_added_friends_before('$end_of_table', Entries, _Timestamp) ->
    % logger:debug(
    %     "roster:get_latest_added_friends_before $end_of_table entries: ~p", [Entries]),
    Entries;

get_latest_added_friends_before(Key, {StringEntries, ObjectList}, Timestamp) ->
    %% For each key we must fetch all objects and append them to CurrentStringEntries.
    %% Then continue going through ets with the next key.

    ObjectsForKey = ets:lookup(?LATEST_ADDED_TAB, Key),
    Entries = lists:foldl(
        fun({User, Friend, CurrTimestamp}, {CurrString, CurrObjectList}) ->
            if
                (CurrTimestamp < Timestamp) ->
                    case CurrString of
                        "" ->
                            {
                                "(\"" ++ User ++ "\", \"" ++ Friend ++ "\") ",
                                lists:append([{User, Friend, CurrTimestamp}], CurrObjectList)
                            };
                        _Other ->
                            {
                                CurrString ++ ", (\"" ++ User ++ "\", \"" ++ Friend ++ "\") ",
                                lists:append([{User, Friend, CurrTimestamp}], CurrObjectList)
                            }
                    end;
                (CurrTimestamp > Timestamp) ->
                    ""
            end

        end,
        {StringEntries, ObjectList},
        ObjectsForKey
    ),
    get_latest_added_friends_before(
        ets:next(?LATEST_ADDED_TAB, Key), Entries, Timestamp).

get_latest_used_friends_before('$end_of_table', ObjectList, _Timestamp) ->
    ObjectList;

get_latest_used_friends_before(Key, ObjectList, Timestamp) ->
    % {User, Friend, BooleanValue, CurrTimestamp}
    KeyObjectList = ets:lookup(?LATEST_USED_FRIENDS_TAB, Key),
    logger:debug("roster:get_latest_used_friends_before() KeyObjectList ~p", [KeyObjectList]),
    FilteredKeyObjectList = lists:filter(
        fun({_User, _Friend, _BooleanValue, CurrTimestamp}) ->
            logger:debug("roster:get_latest_used_friends_before() ~p",
                [CurrTimestamp < Timestamp]),
            CurrTimestamp < Timestamp
        end,
        KeyObjectList
    ),
    get_latest_used_friends_before(
        ets:next(?LATEST_USED_FRIENDS_TAB, Key),
        lists:append(ObjectList, FilteredKeyObjectList),
        Timestamp
    ).

is_name_of(_User, []) ->
    false;

%% For Elements in ALL_FRIENDS_TAB
is_name_of(User, [{_Key, CurrUser} | List]) ->
    case User == CurrUser of
        true -> true;
        false ->
            is_name_of(User, List)
    end;

%% For Elements in LATEST_USED_FRIENDS_TAB
is_name_of(User, [{_Key, CurrUser, BooleanValue, _Timestamp} | List]) ->
    case User == CurrUser of
        true -> BooleanValue;
        false ->
            is_name_of(User, List)
    end.

update_latest_used_friends_tab(User1, User2, NewTimestamp) ->
    case ets:lookup(?LATEST_USED_FRIENDS_TAB, User1) of
        {User1, User2, Value, _OldTimestamp} = Object ->
            ets:delete_object(?LATEST_USED_FRIENDS_TAB, Object),
            ets:insert(?LATEST_USED_FRIENDS_TAB, {User1, User2, Value, NewTimestamp});
        _Other ->
            ok
    end.

delete_friendship_from_latest_used_friends_tab(User, Friend) ->
    UserFriends = ets:lookup(?LATEST_USED_FRIENDS_TAB, User),
    lists:foreach(
        fun({_User, UserFriend, IsFriend, Timestamp}) ->
            case User == Friend of
                true ->
                    ets:delete_object(?LATEST_USED_FRIENDS_TAB, {User, UserFriend, IsFriend, Timestamp});
                false ->
                    ok
            end
        end,
        UserFriends
    ).

%%%=================================================================================================
%%% gen_server callbacks
%%%=================================================================================================
init(_Args) ->
    logger:debug("roster:init()"),
    %% TODO see if needed to integrate the option {heir,Pid,HeirData} | {heir,none}
    ets:new(?LATEST_USED_FRIENDS_TAB, [bag, private, named_table]),
    ets:new(?LATEST_ADDED_TAB, [bag, private, named_table]),
    %% TabAllFriends should be populated from mysql when a get on the list is made
    ets:new(?ALL_FRIENDS_TAB, [bag, private, named_table]),
    case timer:send_interval(?TIME_TO_DROP_LATEST_ADDED_FRIENDS, drop_latest_added_friends) of
        {ok, _DropTref} ->
            logger:debug("roster:init() Timer set for drop_latest_added_friends");
        {error, DropError} ->
            logger:error(
                "roster:init() Timer was not set for drop_latest_added_friends ~p", [DropError])
    end,
    case timer:send_interval(?TIME_TO_UPDATE_LATEST_USED_FRIENDS, update_latest_used_friends) of
        {ok, _UpdateTref} ->
            logger:debug("roster:init() Timer set for update_latest_used_friends");
        {error, UpdateError} ->
            logger:error("roster:init() Timer was not set for update_latest_used_friends ~p",
                [UpdateError])
    end,
    {ok, []}.

%% Gets a list of friends usernames.
handle_call({get_friends, User}, {_FromPid, _FromTag}, State) ->
    %% TODO fetch data from mysql if the tab is empty
    FriendsTupleList = ets:lookup(?ALL_FRIENDS_TAB, User),
    FriendsList = lists:map(fun({_Key, Friend}) -> Friend end, FriendsTupleList),
    {reply, {friends_list, FriendsList}, State};

handle_call({are_friends, User1, User2}, _From, State) ->
    Now = now(),
    case ets:lookup(?LATEST_USED_FRIENDS_TAB, User1) of
        [] ->
            %% TODO fetch data from mysql if the all tab is empty
            FriendsTuples = ets:lookup(?ALL_FRIENDS_TAB, User1),
            IsFriend = is_name_of(User2, FriendsTuples),
            logger:debug("roster:handle_call() are_friends(~s,~s) ~p [looked in tab ~p]",
                [User1, User2, IsFriend, ?ALL_FRIENDS_TAB]),
            ets:insert(?LATEST_USED_FRIENDS_TAB, {User1, User2, IsFriend, Now}),
            ets:insert(?LATEST_USED_FRIENDS_TAB, {User2, User1, IsFriend, Now}),
            {reply, IsFriend, State};
        FriendsTuples ->
            IsFriend = is_name_of(User2, FriendsTuples),
            logger:debug("roster:handle_call() are_friends(~s,~s) ~p [looked in tab ~p]",
                [User1, User2, IsFriend, ?LATEST_USED_FRIENDS_TAB]),
            logger:debug("roster:handle_call() are_friends(~s,~s) FriendsTuples ~p",
                [User1, User2, FriendsTuples]),
            update_latest_used_friends_tab(User1, User2, Now),
            update_latest_used_friends_tab(User2, User1, Now),
            {reply, IsFriend, State}
    end;

handle_call(OtherRequest, _From, State) ->
    logger:error("roster:handle_call() Unknown cast request ~p", [OtherRequest]),
    {noreply, State}.

handle_cast({add_friend, User1, User2}, State) ->
    ets:insert(?ALL_FRIENDS_TAB, {User1, User2}),
    ets:insert(?ALL_FRIENDS_TAB, {User2, User1}),

    Now = now(),
    delete_friendship_from_latest_used_friends_tab(User1, User2),
    delete_friendship_from_latest_used_friends_tab(User2, User1),
    ets:insert(?LATEST_USED_FRIENDS_TAB, {User1, User2, true, Now}),
    ets:insert(?LATEST_USED_FRIENDS_TAB, {User2, User1, true, Now}),

    ets:insert(?LATEST_ADDED_TAB, {User1, User2, Now}),
    ets:insert(?LATEST_ADDED_TAB, {User2, User1, Now}),
    {noreply, State};

handle_cast(load_friends, State) ->
    logger:error("registration:handle_cast() load_friends"),
    MySqlFriends = fetch_friends_from_mysql(),
    lists:foreach(
        fun([User, Friend]) ->
            ets:insert(?ALL_FRIENDS_TAB, {User, Friend}),
            logger:debug("roster:handle_cast() load_friends Added ~p to ets ~p",
                [{User, Friend}, ?ALL_FRIENDS_TAB])
        end,
        MySqlFriends
    ),
    logger:error("registration:handle_cast() load_friends ~p", [MySqlFriends]),
    {noreply, State};

handle_cast(OtherRequest, State) ->
    logger:error("roster:handle_cast() Unknown cast request ~p", [OtherRequest]),
    {noreply, State}.

handle_info(drop_latest_added_friends, State) ->
    %% Take all entries in LATEST_ADDED_TAB which are  {User, Friend}
    %% and bulk insert them in mysql.
    Now = now(),
    Empty = {"", []},
    case get_latest_added_friends_before(ets:first(?LATEST_ADDED_TAB), Empty, Now) of
        {"", []} ->
            % logger:debug("roster:handle_info() drop_latest_added_friends No entries found.");
            ok;
        {Entries, ObjectList} ->
            logger:debug("roster:handle_info() drop_latest_added_friends Entries ~p.", [Entries]),
            lists:foreach(
                fun(Object) ->
                    ets:delete_object(?LATEST_ADDED_TAB, Object)
                    % logger:debug("roster:handle_info() drop_latest_added_friends deleted object: "
                    %     ++ "~p from ets ~p", [Object, ?LATEST_ADDED_TAB])
                end,
                ObjectList
            ),
            MySqlInsertCommand = ?INSERT_FRIENDS_INTO_MYSQL ++ Entries,
            Result = p1_mysql:fetch(?MYSQL_ID, MySqlInsertCommand),
            logger:debug(
                "roster:handle_info() drop_latest_added_friends, MysqlCommand ~p and MySql result: "
                    ++ "~p", [MySqlInsertCommand, Result])
    end,
    {noreply, State};

handle_info(update_latest_used_friends, State) ->
    Now = now(),
    %% Go through all entries of LATEST_USED_FRIENDS_TAB and delete those with Timestamp <= Now.
    ObjToDelete = get_latest_used_friends_before(ets:first(?LATEST_USED_FRIENDS_TAB), [], Now),
    lists:foreach(
        fun(Object) ->
            ets:delete_object(?LATEST_USED_FRIENDS_TAB, Object)
            % logger:debug("registration:handle_info() update_latest_used_tab, Deleted object: ~p "
            %     ++ "from ets ~p", [Object, ?LATEST_USED_FRIENDS_TAB])
        end,
        ObjToDelete
    ),
    {noreply, State};

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
