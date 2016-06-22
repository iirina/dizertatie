%% Accepts socket connections and spawns a new proccess after each new connection established.

-module(bm_generator).
-behaviour(gen_server).

-export([
    start_link/0,
    add_pid/1
]).

-export([
    send_msg_to_pid/2,
    send_batch_messages/3,
    send_batch_group/2,
    send_batch_register/2,
    send_batch_friendship/3
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

-define(MYSQL_ID, "1234").
-define(MYSQL_HOST, "localhost").
-define(MYSQL_USER, "root").
-define(MYSQL_PASSWORD, "parola").
-define(MYSQL_DATABASE, "chat").

-define(GENERAL_NR, 500).

-define(NR_USERS, ?GENERAL_NR).
-define(NR_FRIENDSHIP, ?GENERAL_NR * 100).
-define(NR_MESSAGES, ?GENERAL_NR).
-define(NR_GROUP_MESSAGES, ?GENERAL_NR).
-define(BATCH_SIZE, 100).
-define(MAX_MESSAGE_LENGTH, 20).

-define(SLAVE1_HOST, '192.168.178.28').
-define(SLAVE2_HOST, '192.168.178.28').
-define(SLAVE_PORT, 5400).
-define(MASTER_PORT, 5400).

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

get_random_msg(MessageLength) ->
    Chrs = list_to_tuple("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"),
    ChrsSize = size(Chrs),
    F = fun(_, R) -> [element(random:uniform(ChrsSize), Chrs) | R] end,
    lists:foldl(F, "", lists:seq(1, MessageLength)).

get_random_messages(0, Messages) ->
    Messages;

get_random_messages(NrMessages, ExistingMessages) ->
    MessageLength = random:uniform(?MAX_MESSAGE_LENGTH),
    NewObject = {
        random:uniform(?NR_USERS),
        random:uniform(?NR_USERS),
        get_random_msg(MessageLength)
    },
    get_random_messages(NrMessages - 1, lists:append(ExistingMessages, [NewObject])).

get_random_group_messages(0, Messages) ->
    Messages;

get_random_group_messages(NrMessages, ExistingMessages) ->
    MessageLength = random:uniform(?MAX_MESSAGE_LENGTH),
    NewObject = {random:uniform(?NR_USERS), get_random_msg(MessageLength)},
    get_random_group_messages(NrMessages - 1, lists:append(ExistingMessages, [NewObject])).

send_msg_to_pid(Pid, Msg) ->
        Pid ! Msg.

send_batch_messages(NrBatches, Pids, Usernames) when NrBatches >= 1 ->
    Messages = get_random_messages(?BATCH_SIZE, []),
    lists:foreach(
        fun({PidIndex, UserIndex, Message}) ->
            Pid = lists:nth(PidIndex, Pids),
            From = lists:nth(UserIndex, Usernames),
            spawn(bm_generator, send_msg_to_pid, [Pid, {chat, Message, From}])
        end,
        Messages
    ),
    timer:apply_after(500, bm_generator, send_batch_messages, [NrBatches - 1, Pids, Usernames]);

send_batch_messages(NrBatches, _Pids, _Usernames) when NrBatches < 1 ->
    ok.

send_batch_group(NrBatches, Pids) when NrBatches >= 1 ->
    Messages = get_random_group_messages(?BATCH_SIZE, []),
    lists:foreach(
        fun({PidIndex, Message}) ->
            Pid = lists:nth(PidIndex, Pids),
            spawn(bm_generator, send_msg_to_pid, [Pid, {group, Message}])
        end,
        Messages
    ),
    timer:apply_after(500, bm_generator, send_batch_group, [NrBatches - 1, Pids]);

send_batch_group(NrBatches, _Pids) when NrBatches < 1 ->
    ok.

send_batch_register(NrBatches, Pids) when NrBatches >= 1 ->
    lists:foreach(
        fun(Pid) ->
            spawn(bm_generator, send_msg_to_pid, [Pid, register])
        end,
        Pids
    ),
    timer:apply_after(500, bm_generator, send_batch_register, [NrBatches - 1, Pids]);

send_batch_register(NrBatches, _Pids) when NrBatches < 1 ->
    ok.

send_batch_friendship(NrBatches, Pids, Usernames) when NrBatches >= 1 ->
    Friends = get_random_friendships(?BATCH_SIZE, []),
    lists:foreach(
        fun({PidIndex, UserIndex}) ->
            Pid = lists:nth(PidIndex, Pids),
            User = lists:nth(UserIndex, Usernames),
            spawn(bm_generator, send_msg_to_pid, [Pid, {accept_friend_request, User}])
        end,
        Friends
    );

send_batch_friendship(NrBatches, _Pids, _Usernames) when NrBatches >= 1 ->
    ok.

start_clients([], _Slave) ->
    ok;

start_clients([Username | Usernames], Slave) ->
    case Slave of
        slave1 ->
            chat_client:start(Username, "parola" ++ Username, ?SLAVE1_HOST, ?SLAVE_PORT),
            start_clients(Usernames, slave2);
        slave2 ->
            chat_client:start(Username, "parola" ++ Username, ?SLAVE2_HOST, ?SLAVE_PORT),
            start_clients(Usernames, slave1)
    end.

%%%=================================================================================================
%%% gen_server callbacks
%%%=================================================================================================
init(_Args) ->
    logger:debug("bm_generator:init()"),
    p1_mysql:start(?MYSQL_ID, ?MYSQL_HOST, ?MYSQL_USER, ?MYSQL_PASSWORD, ?MYSQL_DATABASE,
        fun(_Level, Format, Args) -> logger:debug(Format, Args) end),
    Usernames = get_usernames(?NR_USERS, []),
    start_clients(Usernames, slave1),
    {ok, #state{pids = [], usernames = Usernames}}.

handle_call(OtherRequest, _From, State) ->
    logger:error("bm_generator:handle_call() Unknown call request <<~p>>", [OtherRequest]),
    {noreply, State}.

handle_cast({add_pid, Pid}, State) ->
    PidList = State#state.pids,
    NewPidList = lists:append(PidList, [Pid]),
    % logger:debug("bm_generator:handle_cast() add_pid ~p to list ~p.", [Pid, PidList]),
    case length(NewPidList) == ?NR_USERS of
        true ->
            gen_server:cast(bm_generator, register_users);
        false ->
            do_nothing
    end,
    {noreply, State#state{pids = NewPidList}};

handle_cast(register_users, State) ->
    % logger:debug("bm_generator:handle_cast() register_users ~p.", [PidList]),
    NrBatches = ?NR_USERS / ?BATCH_SIZE,
    send_batch_register(NrBatches, State#state.pids),
    % apply_after(Time, Module, Function, Arguments)
    timer:apply_after(500, gen_server, cast, [bm_generator, make_friends]),
    % gen_server:cast(bm_generator, make_friends),
    {noreply, State};

handle_cast(make_friends, State) ->
    Pids = State#state.pids,
    Usernames = State#state.usernames,
    %% Add ?NR_FRIENDSHIP pairs (u1, u2) to mysql
    random:seed(erlang:now()),
    NrBatches = ?NR_FRIENDSHIP / ?BATCH_SIZE,
    send_batch_friendship(NrBatches, Pids, Usernames),
    gen_server:cast(bm_generator, send_messages),
    {noreply, State};

handle_cast(send_messages, State) ->
    %% Add ?NR_FRIENDSHIP pairs (u1, u2) to mysql
    random:seed(erlang:now()),
    NrBatches = ?NR_MESSAGES / ?BATCH_SIZE,
    send_batch_messages(NrBatches, State#state.pids, State#state.usernames),
    timer:apply_after(500, gen_server, cast, [bm_generator, send_group_messages]),
    {noreply, State};

handle_cast(send_group_messages, State) ->
    %% Add ?NR_FRIENDSHIP pairs (u1, u2) to mysql
    random:seed(erlang:now()),
    % logger:debug("bm_generator:handle_cast() send_group_messages ~p", [Messages]),
    NrBatches = ?NR_GROUP_MESSAGES / ?BATCH_SIZE,
    send_batch_group(NrBatches, State#state.pids),
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
