-module(generator).
-behaviour(gen_server).

%% API
-export([
    start_link/0,
    get_id/0,
    get_name/0
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

-define(FETCH_ALL_USERS_MYSQL, "select * from user").
-define(MYSQL_ID, "1234").

-record(state, {id, names}).

%%%=================================================================================================
%%% API callbacks
%%%=================================================================================================
start_link() ->
    logger:debug("generator:start_link()"),
    gen_server:start_link({local, generator}, generator, noargs, []).

get_id() ->
    gen_server:call(generator, get_id).

get_name() ->
    gen_server:call(generator, get_name).

%%%=================================================================================================
%%% Helper functions
%%%=================================================================================================
get_unique_name(UsedNamesSet) ->
    Name = "irina" ++ integer_to_list(random:uniform(100000)),
    case sets:is_element(Name, UsedNamesSet) of
        true ->
            get_unique_name(UsedNamesSet);
        false ->
            Name
    end.

fetch_users_from_mysql() ->
    case p1_mysql:fetch(?MYSQL_ID, ?FETCH_ALL_USERS_MYSQL) of
        {data, {p1_mysql_result, _FieldList, UsersRetrieved, _Number, _List}} ->
            Users = lists:map(
                fun([User, Pass]) ->
                    logger:debug(
                        "registration:fetch_users_from_mysql() User ~p password ~p", [User, Pass]),
                    User
                end,
                UsersRetrieved
            ),
            Users;
        _Other ->
            []
    end.

%%%=================================================================================================
%%% gen_server callbacks
%%%=================================================================================================
init(_Args) ->
    random:seed(erlang:now()),
    Names = sets:from_list(fetch_users_from_mysql()),
    {ok, #state{id = 1, names = Names}}.

handle_call(get_id, _From, State) ->
    Id = State#state.id,
    logger:debug("generator:handle_call() get_id ~p", [Id]),
    {reply, integer_to_list(Id), State#state{id = Id + 1}};

handle_call(get_name, _From, State) ->
    Names = State#state.names,
    Name = get_unique_name(Names),
    logger:debug("generator:handle_call() get_name ~p", [Name]),
    {reply, Name, State#state{names = sets:add_element(Name, Names)} };

handle_call(Request, From, State) ->
    logger:error("generator:handle_call(): Unknown request ~p from PID ~p", [Request, From]),
    {noreply, State}.

handle_cast(OtherRequest, State) ->
    logger:error("generator:handle_cast() Unknown cast request <<~p>>", [OtherRequest]),
    {noreply, State}.


handle_info(Info, State) ->
	logger:error("generator:handle_info() Unknown info <<~p>>", [Info]),
	{noreply, State}.

% terminate is called if a handle_* call returns stop
% generator is brutally killed by chat_supervisor on shutdown
terminate(Reason, _State) ->
	logger:info("generator:terminate() Terminating for reason: ~p", [Reason]),
	ok.

code_change(_OldVsn, State, _Extra) ->
	NewState = State,
	{ok, NewState}.
