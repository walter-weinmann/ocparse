@echo off
erl -noshell -pa _build\default\lib\ocparse\ebin -s ocparse_generator generate -s init stop
