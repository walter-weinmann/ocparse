@echo off
erl -noshell -pa ebin -s test_generator generate -s init stop
