%%-*- mode: erlang -*-
%%-*- coding: utf-8 -*-

% Test query options
[{tests, []}].

%% 
%% TESTS
%%

"REMOVE 'Dies\" ist \"ein String' .Property_key_4711".
"REMOVE 'Dies\" ist \"ein String' .Property_key_1".
"REMOVE 'Dies\" ist \"ein String' .Property_key_1? .Property_key_1!".
"REMOVE variable_1 : node_1".
"REMOVE variable_1 : node_1 : node_2".

"REMOVE 'Dies\" ist \"ein String'  .Property_key_4711, variable_1 : node_1".
"REMOVE 'Dies\" ist \"ein String' .Property_key_1 , 'Dies\" ist \"ein String' .Property_key_1".
"REMOVE 'Dies\" ist \"ein String' .Property_key_1? .Property_key_1! , variable_1 : node_1 : node_2".
"REMOVE variable_1 : node_1 , 'Dies\" ist \"ein String' .Property_key_1? .Property_key_1!".
"REMOVE variable_1 : node_1 : node_2 , 'Dies\" ist \"ein String' .Property_key_4711".
