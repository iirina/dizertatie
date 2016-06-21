#!/bin/bash
cd chat_slave/
erl -make
cd ..

sudo erl -name $1 -setcookie chatcookie -env ERL_LIBS "."
