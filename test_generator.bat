@echo off
erl -noshell -pa _build\default\lib\ocparse\ebin -s test_generator generate -s init stop
