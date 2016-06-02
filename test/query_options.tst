%%-*- mode: erlang -*-
%%-*- coding: utf-8 -*-

% Test query options
[{tests, []}].

%% 
%% TESTS
%%

"cypher create index on :Actor(name)".
"explain create index on :Actor(name)".
"profile create index on :Actor(name)".
"explain profile create index on :Actor(name)".
"EXPLAIN PROFILE create index on :Actor(name)".

"cypher 2.2 create index on :Actor(name)".

"cypher planner=cost create index on :Actor(name)".
"cypher planner=rule create index on :Actor(name)".
"cypher planner=cost planner=rule create index on :Actor(name)".

"explain cypher planner=cost create index on :Actor(name)".
"profile cypher planner=rule create index on :Actor(name)".
"explain profile cypher planner=cost planner=rule create index on :Actor(name)".

"cypher 2.2 planner=cost create index on :Actor(name)".

"explain cypher 2.2 planner=cost create index on :Actor(name)".
"profile cypher 2.2 planner=cost create index on :Actor(name)".
"explain profile cypher 2.2 planner=cost create index on :Actor(name)".
"explain profile cypher 2.2 planner=cost planner=rule create index on :Actor(name)".

"explain cypher 2.2 cypher planner=cost create index on :Actor(name)".
"explain profile cypher 2.2 cypher planner=cost create index on :Actor(name)".
"explain profile cypher 2.2 cypher planner=cost cypher planner=rule create index on :Actor(name)".
