#!/bin/bash
cd chat_master/
erl -make
cd ..
sudo erl -sname chat_master -env ERL_LIBS "." -eval "application:start(p1_mysql), application:start(chat_master)."
