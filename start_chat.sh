#!/bin/bash
erl -env ERL_LIBS "." -eval "application:start(p1_mysql), application:start(chat)."
