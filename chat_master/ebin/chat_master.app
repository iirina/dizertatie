{application, chat_master, [
	{description, "Chat Server"},
	{vsn, "0.1"},
	{modules, [master_chat_auth, master_chat_app, master_chat_supervisor, chat_utils, master_courier, logger, master_registration,
				master_roster, conn_distr_sup, conn_distr]},
	{registered, [master_courier, conn_distr_sup, master_chat_supervisor, master_roster_master]},
	{applications, [kernel, stdlib, p1_mysql]},
	{mod, {master_chat_app, []}}
]}.
