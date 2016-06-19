%%-*- mode: erlang -*-
%%-*- coding: utf-8 -*-

% Test query options
[{tests, []}].

%% 
%% TESTS
%%

"CREATE INDEX ON :Actor(name)".
"create index on :Actor (name)".
"create index on :Actor ( name )".

"DROP INDEX ON :Person(name)".
"drop index on :Person (name)".
"drop index on :Person ( name )".
