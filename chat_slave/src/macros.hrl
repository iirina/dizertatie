-define(MYSQL, "mysql").
-define(MNESIA, "mnesia").

-define(STORAGE, ?MYSQL).

-define(DEFAULT_MASTER_PORT, 5455).
-define(DEFAULT_SLAVE_PORT, 5400).

-define(SLAVE_NODE_NAME, chat_slave).
-define(MASTER_NODE_NAME, chat_master).

-define(MASTER_HOST, "192.168.178.28").

-define(MASTER_NODE, 'chat_master@192.168.178.28').

%%%=================================================================================================
%%% roster related macros
%%%=================================================================================================
-define(LATEST_USED_FRIENDS_TAB, latest_friends_tab).
-define(LATEST_ADDED_TAB, latest_added_friends_tab).
-define(ALLTIME_FRIENDS_TAB, alltime_friends_tab).
-define(TIME_TO_DROP_LATEST_ADDED_FRIENDS, 5 * 1000).
-define(TIME_TO_UPDATE_LATEST_USED_FRIENDS, 3 * 1000).

%%%=================================================================================================
%%% MySQL related macros
%%%=================================================================================================
-define(MYSQL_ID, "1234").
-define(MYSQL_HOST, "localhost").
-define(MYSQL_USER, "root").
-define(MYSQL_PASSWORD, "parola").
-define(MYSQL_DATABASE, "chat").

-define(FETCH_ALL_FRIENDS_MYSQL, "select * from friends").
-define(INSERT_FRIENDS_INTO_MYSQL, "insert into friends values ").
-define(FETCH_ALL_USERS_MYSQL, "select * from user").
-define(INSERT_USERS_INTO_MYSQL, "insert into user values ").

%%%=================================================================================================
%%% master_courier related macros
%%%=================================================================================================
-define(MESSAGE_SENT, "message_sent").
-define(GROUP_MESSAGE_SENT, "group_message_sent").
-define(NO_FRIENDS, "no_friends").
-define(FRIEND_UNAVAILABLE, "friend_unavailable").
-define(NOT_FRIENDS, "not_friends").

-define(LATEST_CONNECTED_USERS, latest_connected_users).
-define(LATEST_ACTIVE_USERS, latest_active_users).
-define(ALL_CONNECTED_USERS, all_connected_users).
-define(TIME_TO_UPDATE_COURIER_ETS_TABLES, 5 * 1000).

%%%=================================================================================================
%%% master_registration related macros
%%%=================================================================================================
-define(LATEST_REGISTERED_USED_TAB, latest_registered_used_tab).
-define(LATEST_REGISTERED_ADDED_TAB, latest_registered_added_tab).
-define(ALL_REGISTERED_TAB, all_registered).
-define(USER_TAKEN, "user_taken").
-define(REGISTRATION_COMPLETED, "master_registration_completed").

%% Time expressed in milliseconds.
-define(TIME_TO_DROP_REGISTERED_USERS, 5 * 1000). %% 1 * 5 * 1000
-define(TIME_TO_UPDATE_LATEST_USED_TAB, 5 * 1000). %% 5 * 5 * 1000


%%%=================================================================================================
%%% socket_handler related macros
%%%=================================================================================================
-define(GROUP_MESSAGE_TOKEN, "group").
-define(CHAT_TOKEN, "chat").
-define(GET_FRIENDS_TOKEN, "get_friends").
-define(ADD_FRIEND_TOKEN, "add_friend").
-define(REMOVE_FRIEND_TOKEN, "remove_friend").
-define(ACCEPT_FRIEND_REQUEST_TOKEN, "accept_friend_request").
-define(REJECT_FRIEND_REQUEST_TOKEN, "reject_friend_request").
-define(FRIEND_REQUEST, " wants to be your friend.").
-define(FRIEND_REQUEST_ACCEPTED, " accepted your friend request.").
-define(FRIEND_REQUEST_REJECTED, " rejected your friend request.").

-define(FRIENDSHIP_REQUEST_SENT, "friendship_request_sent").
-define(FRIENDSHIP_STATUS_SENT, "friendship_status_sent").
