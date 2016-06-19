{application, chat_master, [
	{description, "Chat Server"},
	{vsn, "0.1"},
	{modules, [chat_auth, chat_app, chat_supervisor, chat_utils, courier, logger, registration,
				roster, conn_distr_sup, conn_distr]},
	{registered, [socket_acceptor_sup, courier, conn_distr_sup, chat_supervisor, roster]},
	{applications, [kernel, stdlib, p1_mysql]},
	{mod, {chat_app, []}}
	]}.
