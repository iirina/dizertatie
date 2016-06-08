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

-record(state, {nr_clients, pids = []}).

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


%%%=================================================================================================
%%% gen_server callbacks
%%%=================================================================================================
init([NrClients]) ->
    logger:debug("bm_generator:init(~p)", [NrClients]),
    Usernames = get_usernames(NrClients, []),
    logger:debug("bm_generator:init(~p) Username List ~p", [NrClients, Usernames]),
    lists:foreach(
        fun(Username) ->
            chat_client:start(Username, "parola" ++ Username)
        end,
        Usernames
    ),
    {ok, #state{nr_clients = NrClients, pids = []}}.

handle_call(OtherRequest, _From, State) ->
    logger:error("bm_generator:handle_cast() Unknown cast request <<~p>>", [OtherRequest]),
    {noreply, State}.

handle_cast({add_pid, Pid}, State) ->
    NrClients =  State#state.nr_clients,
    PidList = State#state.pids,
    NewPidList = lists:append(PidList, [Pid]),
    logger:debug("bm_generator:handle_cast() add_pid ~p to list ~p.", [Pid, PidList]),
    case  length(NewPidList) == NrClients of
        true ->
            gen_server:cast(bm_generator, register_users);
        false ->
            do_nothing
    end,
    {noreply, State#state{pids = NewPidList}};

handle_cast(register_users, State) ->
    PidList = State#state.pids,
    logger:debug("bm_generator:handle_cast() register_users ~p.", [PidList]),
    lists:foreach(
        fun(Pid) ->
            Pid ! register
        end,
        PidList
    ),
    % gen_server:cast(bm_generator, send_)
    {noreply, State}.

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
