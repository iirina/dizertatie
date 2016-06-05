{application, chat, [
	{description, "Chat Server"},
	{vsn, "0.1"},
	{modules, [chat_auth, chat_app, chat_supervisor, chat_utils, courier, logger, registration, roster,
                socket_acceptor_sup, socket_acceptor, socket_handler_sup, socket_handler]},
	{registered, [socket_acceptor_sup, courier, socket_handler_sup
		, chat_supervisor, roster]},
	{applications, [kernel, stdlib, p1_mysql]},
	{mod, {chat_app, []}}
	]}.
