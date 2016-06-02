{application, chat, [
	{description, "Chat Server"},
	{vsn, "0.1"},
	{modules, [chat_app, chat_supervisor, chat_utils, courier, logger, socket_acceptor,
                socket_acceptor_sup, socket_handler, socket_handler_sup]},
	{registered, [socket_acceptor_sup, courier, socket_handler_sup
		, chat_supervisor]},
	{applications, [kernel, stdlib]},
	{mod, {chat_app, []}}
	]}.
