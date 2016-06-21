#!/bin/bash
cd chat_master/
erl -make
cd ..
sudo erl -name 'chat_master@192.168.178.28' -setcookie chatcookie -env ERL_LIBS "." -eval "application:start(p1_mysql), application:start(chat_master)."
