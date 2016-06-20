#!/bin/bash
cd chat_slave/
erl -make
cd ..
sudo erl -name 'chat_slave@192.168.178.28' -env ERL_LIBS "."
