%% Module used for auhtentication.
%% The user must send a request [auth, {user Username}, {pass Password}]
% This is supervised by chat_supervisor.

-module(auth).

%% API
-export([
    is_auth_request/1,
    verify_validity/1,
    authentication_failed/0,
    authentication_successful/0
]).

-define(AUTH_TOKEN, "auth").
-define(AUTH_FAILED, "authentication_failed").
-define(AUTH_SUCCESSFUL, "authentication_successful").

%%%===================================================================
%%% API functions
%%%===================================================================

%%%===================================================================
%%% Helper functions
%%%===================================================================
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

authentication_failed() ->
    ?AUTH_FAILED.

authentication_successful() ->
    ?AUTH_SUCCESSFUL.

verify_validity(User) ->
    case courier:is_valid(User) of
        ok -> true;
        invalid -> false
    end.
