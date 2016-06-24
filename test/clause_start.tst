%%-*- mode: erlang -*-
%%-*- coding: utf-8 -*-

% Test query options
[{tests, []}].

%% 
%% TESTS
%%

"START variable_1 = NODE ( * )".
"START variable_1 = NODE ( { 4711 } )".
"START variable_1 = NODE ( { parameter_1 } )".
"START variable_1 = NODE ( 4711 )".
"START variable_1 = NODE ( 4711 , 4712 )".
"START variable_1 = NODE (4711,4712 , 4713)".
"START variable_1 = NODE : symbolic_1 ( \"test string\" )".
"START variable_1 = NODE : symbolic_1 ( { 4711 } )".
"START variable_1 = NODE : symbolic_1 ( { parameter_1 } )".
"START variable_1 = NODE : symbolic_1 ( symbolic_2 = \"test string\" )".
"START variable_1 = NODE : symbolic_1 ( symbolic_2 = { 4711 } )".
"START variable_1 = NODE : symbolic_1 ( symbolic_2 = { parameter_1 } )".
"START variable_1 = NODE : symbolic_1 ( symbolic_2 = 'test \"string' )".
"START variable_1 = NODE : symbolic_1 ( symbolic_2 = 'test string' )".
"START variable_1 = NODE : symbolic_1 ( 'test \"string' )".
"START variable_1 = NODE : symbolic_1 ( 'test string' )".
"START variable_1 = REL : symbolic_1 ( \"test string\" )".
"START variable_1 = REL : symbolic_1 ( { 4711 } )".
"START variable_1 = REL : symbolic_1 ( { parameter_1 } )".
"START variable_1 = REL : symbolic_1 ( symbolic_2 = \"test string\" )".
"START variable_1 = REL : symbolic_1 ( symbolic_2 = { 4711 } )".
"START variable_1 = REL : symbolic_1 ( symbolic_2 = { parameter_1 } )".
"START variable_1 = REL : symbolic_1 ( symbolic_2 = 'test \"string' )".
"START variable_1 = REL : symbolic_1 ( symbolic_2 = 'test string' )".
"START variable_1 = REL : symbolic_1 ( 'test \"string' )".
"START variable_1 = REL : symbolic_1 ( 'test string' )".
"START variable_1 = RELATIONSHIP : symbolic_1 ( \"test string\" )".
"START variable_1 = RELATIONSHIP : symbolic_1 ( { 4711 } )".
"START variable_1 = RELATIONSHIP : symbolic_1 ( { parameter_1 } )".
"START variable_1 = RELATIONSHIP : symbolic_1 ( symbolic_2 = \"test string\" )".
"START variable_1 = RELATIONSHIP : symbolic_1 ( symbolic_2 = { 4711 } )".
"START variable_1 = RELATIONSHIP : symbolic_1 ( symbolic_2 = { parameter_1 } )".
"START variable_1 = RELATIONSHIP : symbolic_1 ( symbolic_2 = 'test \"string' )".
"START variable_1 = RELATIONSHIP : symbolic_1 ( symbolic_2 = 'test string' )".
"START variable_1 = RELATIONSHIP : symbolic_1 ( 'test \"string' )".
"START variable_1 = RELATIONSHIP : symbolic_1 ( 'test string' )".

"START variable_1 = NODE ( * ) , variable_2 = RELATIONSHIP : symbolic_1 ( { 4711 } )".
"START variable_1 = NODE (4711,4712 , 4713) , variable_2 = RELATIONSHIP : symbolic_1 ( { 4711 } )".
"START variable_1 = NODE : symbolic_1 ( \"test string\" ) , variable_2 = RELATIONSHIP : symbolic_1 ( { 4711 } )".
"START variable_1 = NODE : symbolic_1 ( symbolic_2 = { 4711 } ) , variable_2 = RELATIONSHIP : symbolic_1 ( { 4711 } )".
"START variable_1 = NODE : symbolic_1 ( symbolic_2 = 'test \"string' ) , variable_2 = RELATIONSHIP : symbolic_1 ( { 4711 } )".
"START variable_1 = REL : symbolic_1 ( symbolic_2 = 'test string' ) , variable_2 = RELATIONSHIP : symbolic_1 ( { 4711 } )".
"START variable_1 = RELATIONSHIP : symbolic_1 ( symbolic_2 = { parameter_1 } ) , variable_2 = RELATIONSHIP : symbolic_1 ( { 4711 } )".

"START variable_1 = NODE ( * ) WHERE nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null".
"START variable_1 = NODE ( { 4711 } ) WHERE nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null".
"START variable_1 = NODE ( { parameter_1 } ) WHERE nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null".
"START variable_1 = NODE ( 4711 ) WHERE nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null".
"START variable_1 = NODE ( 4711 , 4712 ) WHERE nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null".
"START variable_1 = NODE (4711,4712 , 4713) WHERE nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null".
"START variable_1 = NODE : symbolic_1 ( \"test string\" ) WHERE nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null".
"START variable_1 = NODE : symbolic_1 ( { 4711 } ) WHERE nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null".
"START variable_1 = NODE : symbolic_1 ( { parameter_1 } ) WHERE nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null".
"START variable_1 = NODE : symbolic_1 ( symbolic_2 = \"test string\" ) WHERE nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null".
"START variable_1 = NODE : symbolic_1 ( symbolic_2 = { 4711 } ) WHERE nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null".
"START variable_1 = NODE : symbolic_1 ( symbolic_2 = { parameter_1 } ) WHERE nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null".
"START variable_1 = NODE : symbolic_1 ( symbolic_2 = 'test \"string' ) WHERE nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null".
"START variable_1 = NODE : symbolic_1 ( symbolic_2 = 'test string' ) WHERE nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null".
"START variable_1 = NODE : symbolic_1 ( 'test \"string' ) WHERE nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null".
"START variable_1 = NODE : symbolic_1 ( 'test string' ) WHERE nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null".
"START variable_1 = REL : symbolic_1 ( \"test string\" ) WHERE nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null".
"START variable_1 = REL : symbolic_1 ( { 4711 } ) WHERE nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null".
"START variable_1 = REL : symbolic_1 ( { parameter_1 } ) WHERE nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null".
"START variable_1 = REL : symbolic_1 ( symbolic_2 = \"test string\" ) WHERE nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null".
"START variable_1 = REL : symbolic_1 ( symbolic_2 = { 4711 } ) WHERE nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null".
"START variable_1 = REL : symbolic_1 ( symbolic_2 = { parameter_1 } ) WHERE nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null".
"START variable_1 = REL : symbolic_1 ( symbolic_2 = 'test \"string' ) WHERE nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null".
"START variable_1 = REL : symbolic_1 ( symbolic_2 = 'test string' ) WHERE nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null".
"START variable_1 = REL : symbolic_1 ( 'test \"string' ) WHERE nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null".
"START variable_1 = REL : symbolic_1 ( 'test string' ) WHERE nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null".
"START variable_1 = RELATIONSHIP : symbolic_1 ( \"test string\" ) WHERE nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null".
"START variable_1 = RELATIONSHIP : symbolic_1 ( { 4711 } ) WHERE nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null".
"START variable_1 = RELATIONSHIP : symbolic_1 ( { parameter_1 } ) WHERE nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null".
"START variable_1 = RELATIONSHIP : symbolic_1 ( symbolic_2 = \"test string\" ) WHERE nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null".
"START variable_1 = RELATIONSHIP : symbolic_1 ( symbolic_2 = { 4711 } ) WHERE nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null".
"START variable_1 = RELATIONSHIP : symbolic_1 ( symbolic_2 = { parameter_1 } ) WHERE nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null".
"START variable_1 = RELATIONSHIP : symbolic_1 ( symbolic_2 = 'test \"string' ) WHERE nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null".
"START variable_1 = RELATIONSHIP : symbolic_1 ( symbolic_2 = 'test string' ) WHERE nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null".
"START variable_1 = RELATIONSHIP : symbolic_1 ( 'test \"string' ) WHERE nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null".
"START variable_1 = RELATIONSHIP : symbolic_1 ( 'test string' ) WHERE nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null".

"START variable_1 = NODE ( * ) , variable_2 = RELATIONSHIP : symbolic_1 ( { 4711 } ) WHERE nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null".
"START variable_1 = NODE (4711,4712 , 4713) , variable_2 = RELATIONSHIP : symbolic_1 ( { 4711 } ) WHERE nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null".
"START variable_1 = NODE : symbolic_1 ( \"test string\" ) , variable_2 = RELATIONSHIP : symbolic_1 ( { 4711 } ) WHERE nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null".
"START variable_1 = NODE : symbolic_1 ( symbolic_2 = { 4711 } ) , variable_2 = RELATIONSHIP : symbolic_1 ( { 4711 } ) WHERE nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null".
"START variable_1 = NODE : symbolic_1 ( symbolic_2 = 'test \"string' ) , variable_2 = RELATIONSHIP : symbolic_1 ( { 4711 } ) WHERE nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null".
"START variable_1 = REL : symbolic_1 ( symbolic_2 = 'test string' ) , variable_2 = RELATIONSHIP : symbolic_1 ( { 4711 } ) WHERE nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null".
"START variable_1 = RELATIONSHIP : symbolic_1 ( symbolic_2 = { parameter_1 } ) , variable_2 = RELATIONSHIP : symbolic_1 ( { 4711 } ) WHERE nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null".
