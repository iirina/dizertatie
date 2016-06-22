{application, benchmark, [
	{description, "Benchmarking for Chat Server"},
	{vsn, "0.1"},
	{modules, [
                benchmark_app,
                benchmark_sup,
                bm_generator_sup,
                bm_generator,
                char_client_sup,
                chat_client,
                chat_utils,
                generator,
                logger
              ]},
	{registered, [
                    benchmark_sup,
                    bm_generator_sup,
                    bm_generator,
                    chat_client_sup,
                    generator
                ]},
	{applications, [kernel, stdlib, p1_mysql]},
	{mod, {benchmark_app, []}}
	]}.
