%% Module used for auhtentication.
%% The user must send a request [auth, {user Username}, {pass Password}]
% This is supervised by chat_supervisor.

-module(auth).

%% API
-export([
    handle_auth_packet/2
]).

-define(AUTH_TOKEN, "auth").
-define(AUTH_FAILED, "authentication_failed").
-define(AUTH_SUCCESSFUL, "authentication_successful").

%%%===================================================================
%%% API functions
%%%===================================================================
handle_auth_packet(Packet, GenServerPid) ->
    Request = erlang:binary_to_list(Packet),
    case is_auth_request(Request) of
        {user, User} ->
            case verify_validity(User) of
                true ->
                    %% We have to mark this user as connected, let the user now the
                    %% authentication was successful and notify the other chat users about
                    %% his presence.
                    courier:connected(User, GenServerPid),
                    gen_server:cast(GenServerPid, {set_user, User}),
                    gen_server:cast(GenServerPid, {message, ?AUTH_SUCCESSFUL}),
                    %% Informs all chat users about the newly connected user.
                    courier:group_message(chat_utils:format_notification(User, "connected.")),
                    logger:debug("socket_handler:handle_auth() Identified user ~p.", [User]),
                    {user, User};
                false ->
                    %% Authentication has failed, therefor we will try reading
                    %% again from the socket.
                    logger:info("auth:auth_loop() User ~p can not be authenticated.", [User]),
                    %% Let the user know the authentication faild.
                    gen_server:cast(GenServerPid, {message, ?AUTH_FAILED}),
                    authentication_failed
            end;
        false ->
            logger:info("auth:auth_loop() Wrong auth message sent: ~p", [Request]),
            %% Let the user know the authentication faild.
            gen_server:cast(GenServerPid, {message, ?AUTH_FAILED}),
            authentication_failed
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
