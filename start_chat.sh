#!/bin/bash
erl -mnesia dir '"/tmp/mnesia_db"' -env ERL_LIBS "." -eval "application:start(p1_mysql), application:start(chat)."
