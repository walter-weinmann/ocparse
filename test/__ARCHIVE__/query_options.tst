%%-*- mode: erlang -*-
%%-*- coding: utf-8 -*-

% Test query options
[{tests, []}].

%% 
%% TESTS
%%

"match".

"cypher match".
"explain match".
"profile match".
"explain profile match".
"EXPLAIN PROFILE MATCH".

"cypher 2.2 match".

"cypher planner=cost match".
"cypher planner=rule match".
"cypher planner=cost planner=rule match".

"explain cypher planner=cost match".
"profile cypher planner=rule match".
"explain profile cypher planner=cost planner=rule match".

"cypher 2.2 planner=cost match".

"explain cypher 2.2 planner=cost match".
"profile cypher 2.2 planner=cost match".
"explain profile cypher 2.2 planner=cost match".
"explain profile cypher 2.2 planner=cost planner=rule match".
