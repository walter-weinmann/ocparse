%%-*- mode: erlang -*-
%%-*- coding: utf-8 -*-

% Test query options
[{tests, []}].

%% 
%% TESTS
%%

"FOREACH ( variable_1 IN 'test' .property_1 .property2? :label_1 :label_2 .property_3 .property4? | WITH DISTINCT * WHERE nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null )".   
"FOREACH ( variable_1 IN - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 | WITH DISTINCT * WHERE nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null )".   
"FOREACH ( variable_1 IN + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 | WITH DISTINCT * WHERE nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null )".   
"FOREACH ( variable_1 IN + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 >= + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 | WITH DISTINCT * WHERE nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null )".   
"FOREACH ( variable_1 IN nOt + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 = + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 aNd nOt + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 = + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 xOr nOt + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 = + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 aNd nOt + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 = + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 oR nOt + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 = + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 aNd nOt + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 = + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 xOr nOt + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 = + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 aNd nOt + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 = + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 | WITH DISTINCT * WHERE nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null )".   
"FOREACH ( variable_1 IN 'test' .property_1 .property2? :label_1 :label_2 .property_3 .property4? | DETACH DELETE nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null )".   
"FOREACH ( variable_1 IN - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 | DETACH DELETE nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null )".   
"FOREACH ( variable_1 IN + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 | DETACH DELETE nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null )".   
"FOREACH ( variable_1 IN + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 >= + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 | DETACH DELETE nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null )".   
"FOREACH ( variable_1 IN nOt + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 = + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 aNd nOt + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 = + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 xOr nOt + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 = + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 aNd nOt + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 = + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 oR nOt + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 = + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 aNd nOt + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 = + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 xOr nOt + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 = + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 aNd nOt + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 = + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 | DETACH DELETE nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null )".   
"FOREACH ( variable_1 IN 'test' .property_1 .property2? :label_1 :label_2 .property_3 .property4? | DETACH DELETE nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null OPTIONAL MATCH variable_9 = ( variable_1 ) - -> ( ) , ( variable_1 { } ) <- [ : rel_type_1 | : rel_type_1 ] -> ( ) )".   
"FOREACH ( variable_1 IN - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 | DETACH DELETE nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null OPTIONAL MATCH variable_9 = ( variable_1 ) - -> ( ) , ( variable_1 { } ) <- [ : rel_type_1 | : rel_type_1 ] -> ( ) )".   
"FOREACH ( variable_1 IN + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 | DETACH DELETE nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null OPTIONAL MATCH variable_9 = ( variable_1 ) - -> ( ) , ( variable_1 { } ) <- [ : rel_type_1 | : rel_type_1 ] -> ( ) )".   
"FOREACH ( variable_1 IN + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 >= + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 | DETACH DELETE nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null OPTIONAL MATCH variable_9 = ( variable_1 ) - -> ( ) , ( variable_1 { } ) <- [ : rel_type_1 | : rel_type_1 ] -> ( ) )".   
"FOREACH ( variable_1 IN nOt + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 = + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 aNd nOt + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 = + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 xOr nOt + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 = + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 aNd nOt + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 = + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 oR nOt + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 = + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 aNd nOt + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 = + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 xOr nOt + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 = + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 aNd nOt + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 = + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 | DETACH DELETE nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null OPTIONAL MATCH variable_9 = ( variable_1 ) - -> ( ) , ( variable_1 { } ) <- [ : rel_type_1 | : rel_type_1 ] -> ( ) )".   
