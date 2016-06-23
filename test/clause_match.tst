%%-*- mode: erlang -*-
%%-*- coding: utf-8 -*-

% Test query options
[{tests, []}].

%% 
%% TESTS
%%

"MATCH variable_9 = ( ) <-- --> ( )".
"MATCH variable_9 = ( variable_1 ) -- --> ( ) , ( variable_1 { } ) <-- [ : rel_type_1 | : rel_type_1 ] --> ( )".
"MATCH ( variable_1 { } ) <-- [ : rel_type_1 | : rel_type_1 ] --> ( ) , variable_9 = ( ) <-- --> ( ) USING INDEX variable_1 : node_1 ( property_1 )".
"MATCH variable_9 = ( variable_1 ) -- --> ( ) , ( variable_1 { } ) <-- [ : rel_type_1 | : rel_type_1 ] --> ( ) USING JOIN ON variable_1".
"MATCH ( ) <-- --> ( ) , variable_9 = ( : node_1 ) <-- -- ( ) USING JOIN ON variable_1 , variable_2".
"MATCH variable_9 = ( : node_1 : node_2 ) -- -- ( ) , ( ) <-- --> ( ) USING SCAN variable_1 : node_1".
"MATCH variable_9 = ALLSHORTESTPATHS ( ( ( { 4711 } ) <-- [ : rel_type_1 ] --> ( ) ) ) , variable_9 = ( variable_1 : node_1 ) <-- [ ] --> ( ) USING INDEX variable_1 : node_1 ( property_1 ) USING JOIN ON variable_1 , variable_2".
"MATCH ( { } ) <-- [ ] -- ( ) , variable_9 = ( variable_1 : node_1 : node_2 ) -- [ ] --> ( ) USING JOIN ON variable_1 USING JOIN ON variable_1 , variable_2".
"MATCH variable_9 = ( { } ) <-- [ ] -- ( ) , ( { } ) <-- [ ] -- ( ) USING JOIN ON variable_1 , variable_2 USING JOIN ON variable_1 , variable_2".
"MATCH ( variable_1 { } ) <-- [ : rel_type_1 | : rel_type_1 ] --> ( ) , variable_9 = ( { property_key_1 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null } ) -- [ ] -- ( ) USING SCAN variable_1 : node_1 USING JOIN ON variable_1 , variable_2".
"MATCH variable_9 = ( { property_key_1 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null , property_key_2 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null } ) <-- [ variable_1 ] --> ( ) , ( variable_1 { } ) <-- [ : rel_type_1 | : rel_type_1 ] --> ( ) WHERE nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null".
"MATCH variable_9 = ( { parameter_1 } ) <-- [ ? ] --> ( ) , ( { parameter_1 } ) <-- [ ? ] --> ( ) USING INDEX variable_1 : node_1 ( property_1 ) WHERE nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null".
"MATCH ( { parameter_1 } ) <-- [ ? ] --> ( ) , variable_9 = ( { 4711 } ) <-- [ : rel_type_1 ] --> ( ) USING JOIN ON variable_1 WHERE nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null".
"MATCH ( ) <-- --> ( ) , ( variable_1 ) -- --> ( ) USING JOIN ON variable_1 , variable_2 WHERE nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null".
"MATCH ( : node_1 ) <-- -- ( ) , ( : node_1 : node_2 ) -- -- ( ) USING SCAN variable_1 : node_1 WHERE nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null".
"MATCH ( variable_1 : node_1 ) <-- [ ] --> ( ) , ( variable_1 : node_1 : node_2 ) -- [ ] --> ( ) USING INDEX variable_1 : node_1 ( property_1 ) USING JOIN ON variable_1 , variable_2 WHERE nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null".
"MATCH ( { } ) <-- [ ] -- ( ) , ( { property_key_1 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null } ) -- [ ] -- ( ) USING JOIN ON variable_1 USING JOIN ON variable_1 , variable_2 WHERE nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null".
"MATCH ( { property_key_1 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null , property_key_2 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null } ) <-- [ variable_1 ] --> ( ) , ( { parameter_1 } ) <-- [ ? ] --> ( ) USING JOIN ON variable_1 , variable_2 USING JOIN ON variable_1 , variable_2 WHERE nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null".
"MATCH ( { 4711 } ) <-- [ : rel_type_1 ] --> ( ) , ( variable_1 { } ) <-- [ : rel_type_1 | : rel_type_1 ] --> ( ) USING SCAN variable_1 : node_1 USING JOIN ON variable_1 , variable_2 WHERE nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null".

"OPTIONAL MATCH variable_9 = ( ) <-- --> ( )".
"OPTIONAL MATCH variable_9 = ( variable_1 ) -- --> ( ) , ( variable_1 { } ) <-- [ : rel_type_1 | : rel_type_1 ] --> ( )".
"OPTIONAL MATCH ( variable_1 { } ) <-- [ : rel_type_1 | : rel_type_1 ] --> ( ) , variable_9 = ( ) <-- --> ( ) USING INDEX variable_1 : node_1 ( property_1 )".
"OPTIONAL MATCH variable_9 = ( variable_1 ) -- --> ( ) , ( variable_1 { } ) <-- [ : rel_type_1 | : rel_type_1 ] --> ( ) USING JOIN ON variable_1".
"OPTIONAL MATCH ( ) <-- --> ( ) , variable_9 = ( : node_1 ) <-- -- ( ) USING JOIN ON variable_1 , variable_2".
"OPTIONAL MATCH variable_9 = ( : node_1 : node_2 ) -- -- ( ) , ( ) <-- --> ( ) USING SCAN variable_1 : node_1".
"OPTIONAL MATCH variable_9 = ALLSHORTESTPATHS ( ( ( { 4711 } ) <-- [ : rel_type_1 ] --> ( ) ) ) , variable_9 = ( variable_1 : node_1 ) <-- [ ] --> ( ) USING INDEX variable_1 : node_1 ( property_1 ) USING JOIN ON variable_1 , variable_2".
"OPTIONAL MATCH ( { } ) <-- [ ] -- ( ) , variable_9 = ( variable_1 : node_1 : node_2 ) -- [ ] --> ( ) USING JOIN ON variable_1 USING JOIN ON variable_1 , variable_2".
"OPTIONAL MATCH variable_9 = ( { } ) <-- [ ] -- ( ) , ( { } ) <-- [ ] -- ( ) USING JOIN ON variable_1 , variable_2 USING JOIN ON variable_1 , variable_2".
"OPTIONAL MATCH ( variable_1 { } ) <-- [ : rel_type_1 | : rel_type_1 ] --> ( ) , variable_9 = ( { property_key_1 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null } ) -- [ ] -- ( ) USING SCAN variable_1 : node_1 USING JOIN ON variable_1 , variable_2".
"OPTIONAL MATCH variable_9 = ( { property_key_1 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null , property_key_2 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null } ) <-- [ variable_1 ] --> ( ) , ( variable_1 { } ) <-- [ : rel_type_1 | : rel_type_1 ] --> ( ) WHERE nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null".
"OPTIONAL MATCH variable_9 = ( { parameter_1 } ) <-- [ ? ] --> ( ) , ( { parameter_1 } ) <-- [ ? ] --> ( ) USING INDEX variable_1 : node_1 ( property_1 ) WHERE nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null".
"OPTIONAL MATCH ( { parameter_1 } ) <-- [ ? ] --> ( ) , variable_9 = ( { 4711 } ) <-- [ : rel_type_1 ] --> ( ) USING JOIN ON variable_1 WHERE nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null".
"OPTIONAL MATCH ( ) <-- --> ( ) , ( variable_1 ) -- --> ( ) USING JOIN ON variable_1 , variable_2 WHERE nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null".
"OPTIONAL MATCH ( : node_1 ) <-- -- ( ) , ( : node_1 : node_2 ) -- -- ( ) USING SCAN variable_1 : node_1 WHERE nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null".
"OPTIONAL MATCH ( variable_1 : node_1 ) <-- [ ] --> ( ) , ( variable_1 : node_1 : node_2 ) -- [ ] --> ( ) USING INDEX variable_1 : node_1 ( property_1 ) USING JOIN ON variable_1 , variable_2 WHERE nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null".
"OPTIONAL MATCH ( { } ) <-- [ ] -- ( ) , ( { property_key_1 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null } ) -- [ ] -- ( ) USING JOIN ON variable_1 USING JOIN ON variable_1 , variable_2 WHERE nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null".
"OPTIONAL MATCH ( { property_key_1 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null , property_key_2 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null } ) <-- [ variable_1 ] --> ( ) , ( { parameter_1 } ) <-- [ ? ] --> ( ) USING JOIN ON variable_1 , variable_2 USING JOIN ON variable_1 , variable_2 WHERE nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null".
"OPTIONAL MATCH ( { 4711 } ) <-- [ : rel_type_1 ] --> ( ) , ( variable_1 { } ) <-- [ : rel_type_1 | : rel_type_1 ] --> ( ) USING SCAN variable_1 : node_1 USING JOIN ON variable_1 , variable_2 WHERE nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null".
