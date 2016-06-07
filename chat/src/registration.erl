%% Keeps track of the registered users.

-module(registration).
-behaviour(gen_server).

%% API
-export([
    start_link/0,
    register/2,
    is_registered/1,
    load_users/0
]).

%% gen_serv er callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    code_change/3,
    terminate/2
]).

-define(LATEST_REGISTERED_USED_TAB, latest_registered_used_tab).
-define(LATEST_REGISTERED_ADDED_TAB, latest_registered_added_tab).
-define(USER_TAKEN, "User is already used by someone else.").
-define(REGISTRATION_COMPLETED, "Registration was completed.").

%% Time expressed in milliseconds.
-define(TIME_TO_DROP_REGISTERED_USERS, 10 * 1000). %% 1 * 60 * 1000
-define(TIME_TO_UPDATE_LATEST_USED_TAB, 10 * 1000). %% 5 * 60 * 1000

-define(MYSQL_ID, "1234").
-define(FETCH_ALL_USERS_MYSQL, "select * from user").

-record(state, {all_registered = sets:new()}).

%%%=================================================================================================
%%% API
%%%=================================================================================================
%% Starts the registration server. Called by its supervisor.
start_link() ->
    gen_server:start_link({local, registration}, registration, noargs, []).

register(User, Password) ->
    logger:debug("registration:register(~p, ~p)", [User, Password]),
    gen_server:call(registration, {register, User, Password}).

is_registered(User) ->
    logger:debug("registration:is_registered(~p)", [User]),
    gen_server:call(registration, {is_registered, User}).

load_users() ->
    logger:debug("registration:load_users()"),
    gen_server:cast(registration, load_users).

%%%=================================================================================================
%%% Helper functions
%%%=================================================================================================
fetch_users_from_mysql() ->
    case p1_mysql:fetch(?MYSQL_ID, ?FETCH_ALL_USERS_MYSQL) of
        {data, {p1_mysql_result, _FieldList, UsersRetrieved, _Number, _List}} ->
            Users = lists:map(
                fun([User, Pass]) ->
                    logger:debug(
                        "roster:fetch_users_from_mysql() User ~p password ~p", [User, Pass]),
                    User
                end,
                UsersRetrieved
            ),
            Users;
        _Other ->
            []
    end.

get_latest_registered_users_before('$end_of_table', Entries, _Timestamp) ->
    logger:debug(
        "registration:get_latest_registered_users_before $end_of_table entries: ~p", [Entries]),
    Entries;

get_latest_registered_users_before(Key, {StringEntries, ObjectList}, Timestamp) ->
    %% For each key we must fetch all objects and append them to CurrentStringEntries.
    %% Then continue going through ets with the next key.

    ObjectsForKey = ets:lookup(?LATEST_REGISTERED_ADDED_TAB, Key),
    Entries = lists:foldl(
        fun({User, Password, CurrTimestamp}, {CurrString, CurrObjectList}) ->
            if
                (CurrTimestamp < Timestamp) ->
                    {
                        CurrString ++ "\n(\"" ++ User ++ "\", \"" ++ Password ++ "\")",
                        lists:append([{User, Password, CurrTimestamp}], CurrObjectList)
                    };
                (CurrTimestamp > Timestamp) ->
                    ""
            end

        end,
        {StringEntries, ObjectList},
        ObjectsForKey
    ),
    get_latest_registered_users_before(
        ets:next(?LATEST_REGISTERED_ADDED_TAB, Key), Entries, Timestamp).

get_latest_used_before('$end_of_table', ObjectList, _Timestamp) ->
    ObjectList;

get_latest_used_before(Key, ObjectList, Timestamp) ->
    % {User, true, Now}
    KeyObjectList = ets:lookup(?LATEST_REGISTERED_USED_TAB, Key),
    logger:debug("registration:get_latest_used_before() KeyObjectList ~p", [KeyObjectList]),
    FilteredKeyObjectList = lists:filter(
        fun({_User, _Value, CurrTimestamp}) ->
            logger:debug("registration:get_latest_used_before() ~p",
                [CurrTimestamp < Timestamp]),
            CurrTimestamp < Timestamp
        end,
        KeyObjectList
    ),
    get_latest_used_before(
        ets:next(?LATEST_REGISTERED_USED_TAB, Key),
        lists:append(ObjectList, FilteredKeyObjectList),
        Timestamp
    ).

%%%=================================================================================================
%%% gen_server callbacks
%%%=================================================================================================
init(_Args) ->
    logger:debug("registration:init()"),
    %% TODO see if needed to integrate the option {heir,Pid,HeirData} | {heir,none}
    ets:new(?LATEST_REGISTERED_USED_TAB, [set, private, named_table]),
    ets:new(?LATEST_REGISTERED_ADDED_TAB, [set, private, named_table]),
    %% ALL_REGISTERED_TAB should be populated from mysql at the beginning
    NewState = #state{all_registered = sets:new()},
    case timer:send_interval(?TIME_TO_DROP_REGISTERED_USERS, drop_registered_users) of
        {ok, _DropTref} ->
            logger:debug("registration:init() Timer set for drop_registered_users");
        {error, DropError} ->
            logger:error(
                "registration:init() Timer was not set for drop_registered_users ~p", [DropError])
    end,
    case timer:send_interval(?TIME_TO_UPDATE_LATEST_USED_TAB, update_latest_used_tab) of
        {ok, _UpdateTref} ->
            logger:debug("registration:init() Timer set for update_latest_used_tab");
        {error, UpdateError} ->
            logger:error("registration:init() Timer was not set for update_latest_used_tab ~p",
                [UpdateError])
    end,
    {ok, NewState}.

handle_call({is_registered, User}, _From, State) ->
    Now = now(),
    case ets:lookup(?LATEST_REGISTERED_USED_TAB, User) of
        [] ->
            logger:debug("registration:handle_call() is_registered User ~p is not in ets table ~p",
                [User, ?LATEST_REGISTERED_USED_TAB]),
            %% We look for the user in the set that holds all registered users.
            %% TODO: fetch from mysql
            AllRegisteredSets = State#state.all_registered,
            case sets:is_element(User, AllRegisteredSets) of
                true ->
                    logger:debug("registration:handle_call() is_registered User ~p is registered.",
                        [User]),
                    ets:insert(?LATEST_REGISTERED_USED_TAB, {User, true, Now}),
                    {reply, true, State};
                false ->
                    logger:debug("registration:handle_call() is_registered User ~p is not "
                        ++ "registered.", [User]),
                    ets:insert(?LATEST_REGISTERED_USED_TAB, {User, false, Now}),
                    {reply, false, State}
            end;
        [{User, IsRegistered, Timestamp}] ->
            logger:debug(
                "registration:handle_call() is_registered User ~p ~p", [User, IsRegistered]),
            ets:delete_object(?LATEST_REGISTERED_USED_TAB, {User, IsRegistered, Timestamp}),
            ets:insert(?LATEST_REGISTERED_USED_TAB, {User, IsRegistered, Now}),
            {reply, IsRegistered, State}
    end;

handle_call({register, User, Password}, _From, State) ->
    Now = now(),
    logger:debug("registration:handle_cast() register User ~p Password ~p", [User, Password]),

    ExistingRegisteredUsers = State#state.all_registered,
    case sets:is_element(User, ExistingRegisteredUsers) of
        true->
            logger:debug("register:handle_cast() register User ~p already used.", [User]),
            {reply, {false, ?USER_TAKEN}, State};
        false ->
            UpdatedRegisteredUsers = sets:add_element(User, ExistingRegisteredUsers),
            ets:insert(?LATEST_REGISTERED_ADDED_TAB, {User, Password, Now}),
            % ets:insert(?LATEST_REGISTERED_USED_TAB, {User, true, Now}),
            case ets:lookup(?LATEST_REGISTERED_USED_TAB, User) of
                [] ->
                    ets:insert(?LATEST_REGISTERED_USED_TAB, {User, true, Now});
                [{User, false, Timestamp}] ->
                    ets:delete_object(?LATEST_REGISTERED_USED_TAB, {User, false, Timestamp}),
                    ets:insert(?LATEST_REGISTERED_USED_TAB, {User, true, Now});
                 _Other ->
                    ok
            end,
            NewState = #state{all_registered = UpdatedRegisteredUsers},
            {reply, {true, ?REGISTRATION_COMPLETED}, NewState}
    end;

handle_call(OtherRequest, _From, State) ->
    logger:error("registration:handle_call() Unknown cast request ~p", [OtherRequest]),
    {noreply, State}.

handle_cast(load_users, _State) ->
    logger:error("registration:handle_cast() load_users"),
    MySqlUsers = fetch_users_from_mysql(),
    RegisteredUsers = sets:from_list(MySqlUsers),
    NewState = #state{all_registered = RegisteredUsers},
    logger:error("registration:handle_cast() load_users ~p", [sets:to_list(RegisteredUsers)]),
    {noreply, NewState};

handle_cast(OtherRequest, State) ->
    logger:error("registration:handle_cast() Unknown cast request ~p", [OtherRequest]),
    {noreply, State}.

handle_info(drop_registered_users, State) ->
    %% Take all entries in LATEST_REGISTERED_ADDED_TAB which are {User, Password, Timestamp}
    %% and bulk insert them in mysql.
    Now = now(),
    Empty = {"", []},
    case get_latest_registered_users_before(ets:first(?LATEST_REGISTERED_ADDED_TAB), Empty, Now) of
        {"", []} ->
            logger:debug("registration:handle_info() drop_registered_users No entries found.");
        {Entries, ObjectList} ->
            logger:debug("registration:handle_info() drop_registered_users ~p Entries", [Entries]),
            lists:foreach(
                fun(Object) ->
                    ets:delete_object(?LATEST_REGISTERED_ADDED_TAB, Object),
                    logger:debug("registration:handle_info() drop_registered_users, Deleted object:"
                        ++ " ~p from ets ~p", [Object, ?LATEST_REGISTERED_ADDED_TAB])
                end,
                ObjectList
            ),
            MySqlInsertCommand = "insert into user values " ++ Entries,
            Result = p1_mysql:fetch(?MYSQL_ID, MySqlInsertCommand),
            logger:debug(
                "registration:handle_info() drop_registered_users, MySql result: ~p", [Result])
    end,
    {noreply, State};

handle_info(update_latest_used_tab, State) ->
    Now = now(),
    %% Go through all entries of LATEST_REGISTERED_USED_TAB and delete those with Timestamp <= Now.
    ObjToDelete = get_latest_used_before(ets:first(?LATEST_REGISTERED_USED_TAB), [], Now),
    lists:foreach(
        fun(Object) ->
            ets:delete_object(?LATEST_REGISTERED_USED_TAB, Object),
            logger:debug("registration:handle_info() update_latest_used_tab, Deleted object: ~p "
            ++ "from ets ~p", [Object, ?LATEST_REGISTERED_USED_TAB])
        end,
        ObjToDelete
    ),
    {noreply, State};

handle_info(Info, State) ->
	logger:error("registration:handle_info() Unknown info ~p", [Info]),
	{noreply, State}.

% terminate is called if a handle_* call returns stop
% registration is brutally killed by chat_supervisor on shutdown
terminate(Reason, _State) ->
	logger:info("registration:terminate() Terminating for reason ~p", [Reason]),
	ok.

code_change(_OldVsn, State, _Extra) ->
	NewState = State,
	{ok, NewState}.
