%% Module used for auhtentication.
%% The user must send a request [auth, {user Username}, {pass Password}]
% This is supervised by chat_supervisor.

-module(auth).
-behaviour(gen_server).


%% API
-export([
    auth/1,
    start_link/0,
    is_auth_request/1,
    verify_validity/1
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

-define(AUTH_TOKEN, "auth").
-define(AUTH_FAILED, "authentication_failed").
-define(AUTH_SUCCESSFUL, "authentication_successful").

%%%===================================================================
%%% API functions
%%%===================================================================

%% Starts the courier server.
start_link() ->
    gen_server:start_link({local, auth}, auth, noargs, []).

auth(Socket) ->
    % logger:debug("auth:auth(~p)", [Socket]),
    gen_server:call(auth, {socket, Socket}).

% random_name() ->
%     "irina" ++ integer_to_list(random:uniform(1000)).


%%%===================================================================
%%% Helper functions
%%%===================================================================
auth_loop(Socket, FromPid) ->
    logger:debug("auth:auth_loop() starting"),
    case gen_tcp:recv(Socket, 0) of
        {ok, RawRequest} ->
            StringRequest = erlang:binary_to_list(RawRequest),
            % StringRequest = "auth," ++ random_name() ++ ",parola1234\r\n",
            logger:debug("auth:auth_loop() StringRequest ~p", [StringRequest]),
            case is_auth_request(StringRequest) of
                {user, User} ->
                    % logger:info("socket_handler:auth() Identified user ~p with password ~p", [User, Password]),
                    case verify_validity(User) of
                        true ->
                            {user, User};
                        false ->
                            logger:info("auth:auth_loop() User ~p can not be authentified.", [User]),
                            socket_handler:send_msg(?AUTH_FAILED, FromPid),
                            auth_loop(Socket, FromPid)
                    end;
                false ->
                    logger:info("auth:auth_loop() Wrong auth message sent: ~p", [StringRequest]),
                    socket_handler:send_msg(?AUTH_FAILED, FromPid),
                    auth_loop(Socket, FromPid)
            end;
        {error, Reason} ->
            {error, Reason}
    end.


is_auth_request(StringRequest) ->
    Request = chat_utils:trim_string(StringRequest),
    Tokens = string:tokens(Request, ","),
    logger:debug("auth:is_auth_request() Request ~p", [Request]),
    AuthToken = lists:nth(1, Tokens),
    case string:equal(AuthToken, ?AUTH_TOKEN) of
        true ->
            User = lists:nth(2, Tokens),
            logger:debug("auth:is_auth_request() User ~p", [User]),
            {user, User};
        false ->
            false
    end.


verify_validity(User) ->
    case courier:is_valid(User) of
        ok -> true;
        invalid -> false
    end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init(_Args) ->
    logger:debug("auth:init() gen_server initializing."),
	{ok, []}.

handle_call({socket, Socket}, {FromPid, _FromTag}, State) ->
    % logger:debug("auth:handle_call() Starting auth_loop()."),
    case auth_loop(Socket, FromPid) of
        {user, User} ->
            {reply, {user, User}, State};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call(Request, From, State) ->
    logger:error("auth:handle_call(): Unknown request ~p from PID ~p", [Request, From]),
    {noreply, State}.

handle_cast(Request, State) ->
    logger:error("auth:handle_cast() Unknown cast request ~p", [Request]),
    {noreply, State}.

handle_info(Info, State) ->
	logger:error("auth:handle_info() Unknown info ~p", [Info]),
	{noreply, State}.

% terminate is called if a handle_* call returns stop
% auth is brutally killed by chat_supervisor on shutdown
terminate(Reason, _State) ->
	logger:info("auth:terminate() Terminating for reason: ~p", [Reason]),
	ok.

code_change(_OldVsn, State, _Extra) ->
	NewState = State,
	{ok, NewState}.
