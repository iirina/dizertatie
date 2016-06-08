%% Module used for auhtentication.
%% The user must send a request [auth, {user Username}, {pass Password}]
% This is supervised by chat_supervisor.

-module(chat_auth).

%% API
-export([
    handle_auth_packet/2
]).

-define(AUTH_TOKEN, "auth").
-define(AUTH_FAILED, "authentication_failed").
-define(AUTH_SUCCESSFUL, "authentication_successful").
-define(REGISTRATION_TOKEN, "register").
-define(BAD_REQUEST, "bad_request").
-define(NOT_REGISTERED, "not_registered").

%%%===================================================================
%%% API functions
%%%===================================================================
handle_auth_packet(Packet, GenServerPid) ->
    logger:debug("chat_auth:handle_auth_packet(~p, ~p)", [Packet, GenServerPid]),
    Request = erlang:binary_to_list(Packet),
    case get_type_of_action(Request) of
        {auth, Id, User} ->
            case registration:is_registered(User) of
                true ->
                    %% For now we don't verify user and password.
                    %% We have to mark this user as connected, let the user now the
                    %% authentication was successful and notify the other chat users about
                    %% his presence.
                    logger:debug("chat_auth:handle_auth() Identified user ~p.", [User]),
                    courier:connected(User, GenServerPid),
                    gen_server:cast(GenServerPid, {set_user, User}),
                    gen_server:cast(GenServerPid, {message, Id ++ "," ++ ?AUTH_SUCCESSFUL}),
                    %% Informs all chat users about the newly connected user.
                    courier:group_message(
                        "server", User, chat_utils:format_notification(User, "connected")),
                    {user, User};
                false ->
                    logger:debug("chat_auth:handle_auth() User ~p not registered.", [User]),
                    gen_server:cast(GenServerPid, {message, Id ++ "," ++ ?NOT_REGISTERED}),
                    authentication_failed
            end;
        {register, Id, User, Password} ->
            case registration:register(User, Password) of
                {true, Message} ->
                    courier:connected(User, GenServerPid),
                    gen_server:cast(GenServerPid, {set_user, User}),
                    gen_server:cast(GenServerPid, {message, Id ++ "," ++ Message}),
                    courier:group_message(
                        "server", User, chat_utils:format_notification(User, "connected")),
                    logger:debug("chat_auth:handle_auth() Registered user ~p.", [User]),
                    {user, User};
                {false, Message} ->
                    logger:debug("chat_auth:handle_auth() Could not register user ~p.", [User]),
                    gen_server:cast(GenServerPid, {message, Id ++ "," ++ Message}),
                    registration_faild
            end;
        _Other ->
            gen_server:cast(GenServerPid, {message, ?BAD_REQUEST})
    end.

get_type_of_action(StringRequest) ->
    logger:debug("chat_auth:get_type_of_action(~p)", [StringRequest]),
    Request = chat_utils:trim_string(StringRequest),
    case is_auth_request(StringRequest) of
        {user, Id, User} ->
            {auth, Id, User};
        false ->
            case is_registration_request(StringRequest) of
                {user, Id, User, Password} ->
                    {register, Id, User, Password};
                false ->
                    logger:debug("chat_auth:get_type_of_action(~p) unrecognized.", [Request]),
                    unknown
            end
    end.

is_auth_request(Request) ->
    Tokens = string:tokens(Request, ","),
    logger:debug("chat_auth:is_auth_request() Request ~p", [Request]),
    Id = lists:nth(1, Tokens),
    AuthToken = lists:nth(2, Tokens),
    case string:equal(AuthToken, ?AUTH_TOKEN) of
        true ->
            User = lists:nth(3, Tokens),
            % Password = chat_utils:trim_string(lists:nth(4, Tokens)),
            logger:debug("chat_auth:is_auth_request() User ~p", [User]),
            {user, Id, User};
        false ->
            false
    end.

is_registration_request(Request) ->
    Tokens = string:tokens(Request, ","),
    Id = lists:nth(1, Tokens),
    logger:debug("chat_auth:is_auth_request() Request ~p", [Request]),
    RegistrationToken = lists:nth(2, Tokens),
    case string:equal(RegistrationToken, ?REGISTRATION_TOKEN) of
        true ->
            User = lists:nth(3, Tokens),
            Password = chat_utils:trim_string(lists:nth(4, Tokens)),
            logger:debug("chat_auth:is_registration_request() User ~p Password ~p", [User, Password]),
            {user, Id, User, Password};
        false ->
            false
    end.
