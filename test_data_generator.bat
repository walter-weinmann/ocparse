@echo off
erl -noshell -pa ebin -s test_data_generator generate -s init stop
