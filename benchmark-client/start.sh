#!/bin/bash
erl -pa ebin/ -eval "application:start(benchmark)"
