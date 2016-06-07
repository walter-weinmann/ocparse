%%-*- mode: erlang -*-
%%-*- coding: utf-8 -*-

% Test query options
[{tests, []}].

%% 
%% TESTS
%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Expression2
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"4711 :label_1".
"4711 :label_1 :label_2 :label_3".
"4711 .property_1".
"4711 .property_1!".
"4711 .property_1?".
"4711 .property_1 .property2! .property3?".
"4711 :label_1 .property_1".
"4711 :label_1 .property_1 :label_2".
"4711 .property_1 :label_1".
"4711 :label_1 :label_2 .property_1 .property2? :label_3 :label_4".
"4711 .property_1 .property2? :label_1 :label_2 .property_3 .property4?".

"'test' :label_1".
"'test' :label_1 :label_2 :label_3".
"'test' .property_1".
"'test' .property_1!".
"'test' .property_1?".
"'test' .property_1 .property2! .property3?".
"'test' :label_1 .property_1".
"'test' :label_1 .property_1 :label_2".
"'test' .property_1 :label_1".
"'test' :label_1 :label_2 .property_1 .property2? :label_3 :label_4".
"'test' .property_1 .property2? :label_1 :label_2 .property_3 .property4?".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Expression3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Expression3 = Expression2, { (WS, '[', Expression, ']') 
%                           | (WS, '[', [Expression], '..', [Expression], ']') 

"'test_1' .property_1 :label_1".
"'test_1' .property_1 :label_1 is null".
"'test_1' .property_1 :label_1 is NOT null".
"'test_1' .property_1 :label_1 =~ 'test_2' .property_1 :label_1".
"'test_1' .property_1 :label_1 iN 'test_2' .property_1 :label_1".
"'test_1' .property_1 :label_1 STARTS with 'test_2' .property_1 :label_1".
"'test_1' .property_1 :label_1 ends WITH 'test_2' .property_1 :label_1".
"'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Expression4
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1".
"+ 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1".
"- 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1".
