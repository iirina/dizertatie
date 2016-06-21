%% Keeps track of the registered users.

-module(registration).
-behaviour(gen_server).

%% API
-export([
    start_link/0,
    register/2,
    is_registered/1
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

-include("macros.hrl").

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

%%%=================================================================================================
%%% Helper functions
%%%=================================================================================================

get_latest_registered_users_before('$end_of_table', Entries, _Timestamp) ->
    % logger:debug(
    %     "registration:get_latest_registered_users_before $end_of_table entries: ~p", [Entries]),
    Entries;

get_latest_registered_users_before(Key, {StringEntries, ObjectList}, Timestamp) ->
    %% For each key we must fetch all objects and append them to CurrentStringEntries.
    %% Then continue going through ets with the next key.

    ObjectsForKey = ets:lookup(?LATEST_REGISTERED_ADDED_TAB, Key),
    Entries = lists:foldl(
        fun({User, Password, CurrTimestamp}, {CurrString, CurrObjectList}) ->
            if
                (CurrTimestamp < Timestamp) ->
                    case CurrString of
                        "" ->
                            {
                                "(\"" ++ User ++ "\", \"" ++ Password ++ "\") ",
                                lists:append([{User, Password, CurrTimestamp}], CurrObjectList)
                            };
                        _Other ->
                            {
                                CurrString ++ ", (\"" ++ User ++ "\", \"" ++ Password ++ "\") ",
                                lists:append([{User, Password, CurrTimestamp}], CurrObjectList)
                            }
                    end;
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
    FilteredKeyObjectList = lists:filter(
        fun({_User, _Value, CurrTimestamp}) ->
            CurrTimestamp < Timestamp
        end,
        KeyObjectList
    ),
    get_latest_used_before(
        ets:next(?LATEST_REGISTERED_USED_TAB, Key),
        lists:append(ObjectList, FilteredKeyObjectList),
        Timestamp
    ).

seconds_ago({Macro, Sec, Micro}) ->
    {Macro, Sec - 10, Micro}.

is_registered_on_node(User) ->
    Now = now(),
    case ets:lookup(?LATEST_REGISTERED_USED_TAB, User) of
        [] ->
            case ets:lookup(?ALL_REGISTERED_TAB, User) of
                [_Element] ->
                    ets:insert(?LATEST_REGISTERED_USED_TAB, {User, true, Now}),
                    true;
                _Other ->
                    ets:insert(?LATEST_REGISTERED_USED_TAB, {User, false, Now}),
                    false
            end;
        [{User, IsRegistered, _Timestamp}] ->
            logger:debug("registration:is_registered_on_node() is_registered User ~p ~p",
                    [User, IsRegistered]),
            ets:insert(?LATEST_REGISTERED_USED_TAB, {User, IsRegistered, Now}),
            IsRegistered
    end.

is_user_registered(User) ->
    case is_registered_on_node(User) of
        true ->
            logger:debug(
                "registration:is_user_registered(~p) is registered on current node", [User]),
            true;
        false ->
            ServerRef = {master_registration, ?MASTER_NODE},
            Request = {is_registered, User},
            IsRegistered = gen_server:call(ServerRef, Request),
            logger:debug("registration:is_user_registered(~p) is registered on MASTER node: ~p",
                [User, IsRegistered]),
            Now = now(),
            ets:insert(?LATEST_REGISTERED_USED_TAB, {User, IsRegistered, Now}),
            % logger:debug("registration:is_user_registered")
            case IsRegistered of
                false ->
                    ok;
                true ->
                    ets:insert(?LATEST_REGISTERED_ADDED_TAB, {User, "password", Now}),
                    ets:insert(?ALL_REGISTERED_TAB, {User, "password"})
            end,
            IsRegistered
    end.

%%%=================================================================================================
%%% gen_server callbacks
%%%=================================================================================================
init(_Args) ->
    logger:debug("registration:init()"),
    %% TODO see if needed to integrate the option {heir,Pid,HeirData} | {heir,none}
    ets:new(?LATEST_REGISTERED_USED_TAB, [set, public, named_table]),
    ets:new(?LATEST_REGISTERED_ADDED_TAB, [set, public, named_table]),
    ets:new(?ALL_REGISTERED_TAB, [set, public, named_table]),

    ExistingRegisteredUsers = mysql_utils:get_all_users(),
    lists:foreach(
        fun({User, Password}) ->
            % logger:debug
            ets:insert(?ALL_REGISTERED_TAB, {User, Password})
        end,
        ExistingRegisteredUsers),

    %% ALL_REGISTERED_TAB should be populated from mysql at the beginning
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
    {ok, []}.

handle_call({is_registered, User}, _From, State) ->
    {reply, is_user_registered(User), State};

handle_call({register, User, Password}, _From, State) ->
    Now = now(),
    logger:debug("registration:handle_cast() register User ~p Password ~p", [User, Password]),
    case is_user_registered(User) of
        true ->
            logger:debug("register:handle_cast() register User ~p already used.", [User]),
            {reply, {false, ?USER_TAKEN}, State};
        false ->
            ets:insert(?LATEST_REGISTERED_ADDED_TAB, {User, Password, Now}),
            ets:insert(?LATEST_REGISTERED_USED_TAB, {User, true, Now}),
            {reply, {true, ?REGISTRATION_COMPLETED}, State}
    end;

handle_call(OtherRequest, _From, State) ->
    logger:error("registration:handle_call() Unknown cast request ~p", [OtherRequest]),
    {noreply, State}.

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
            % logger:debug("registration:handle_info() drop_registered_users No entries found.");
            ok;
        {Entries, ObjectList} ->
            % logger:debug("registration:handle_info() drop_registered_users ~p Entries", [Entries]),
            lists:foreach(
                fun(Object) ->
                    ets:delete_object(?LATEST_REGISTERED_ADDED_TAB, Object),
                    mysql_utils:bulk_insert_into_user(Entries)
                    % logger:debug("registration:handle_info() drop_registered_users, Deleted object:"
                    %     ++ " ~p from ets ~p", [Object, ?LATEST_REGISTERED_ADDED_TAB])
                end,
                ObjectList
            ),
            logger:debug("registration:handle_info() drop_registered_users, Inserted into mysql")
    end,
    {noreply, State};

handle_info(update_latest_used_tab, State) ->
    Timestamp = seconds_ago(now()),
    %% Go through all entries of LATEST_REGISTERED_USED_TAB and delete those with Timestamp <= Now.
    ObjToDelete = get_latest_used_before(ets:first(?LATEST_REGISTERED_USED_TAB), [], Timestamp),
    lists:foreach(
        fun(Object) ->
            ets:delete_object(?LATEST_REGISTERED_USED_TAB, Object)
            % logger:debug("registration:handle_info() update_latest_used_tab, Deleted object: ~p "
            % ++ "from ets ~p", [Object, ?LATEST_REGISTERED_USED_TAB])
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
