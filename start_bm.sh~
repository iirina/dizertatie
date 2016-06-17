#!/bin/bash
cd benchmark_client
erl -make
cd ..
erl -env ERL_LIBS "." -eval "application:start(p1_mysql), application:start(benchmark)"
