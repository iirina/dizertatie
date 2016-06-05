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

-define(LATEST_REGISTERED_USED_TAB, latest_registered_used_tab).
-define(LATEST_REGISTERED_ADDED_TAB, latest_registered_added_tab).
-define(USER_TAKEN, "User is already used by someone else.").
-define(REGISTRATION_COMPLETED, "Registration was completed.").

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

handle_cast(OtherRequest, State) ->
    logger:error("registration:handle_cast() Unknown cast request ~p", [OtherRequest]),
    {noreply, State}.

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
