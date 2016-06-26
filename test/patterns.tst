%%-*- mode: erlang -*-
%%-*- coding: utf-8 -*-

% Test query options
[{tests, []}].

%%
%% TESTS
%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% pattern = pattern_part_comma_list
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"MATCH ( variable_1 { } ) <-- [ : rel_type_1 | : rel_type_1 ] --> ( ) , variable_9 = SHORTESTPATH ( ( ) <-- --> ( ) )".
"MATCH variable_9 = SHORTESTPATH ( ( variable_1 ) -- --> ( ) ) , ( variable_1 { } ) <-- [ : rel_type_1 | : rel_type_1 ] --> ( )".
"MATCH ( ) <-- --> ( ), variable_9 = SHORTESTPATH ( ( : node_1 ) <-- -- ( ) )".
"MATCH variable_9 = SHORTESTPATH ( ( : node_1 : node_2 ) -- -- ( ) ) , ( ) <-- --> ( )".
"MATCH variable_9 = ALLSHORTESTPATHS ( ( ( { 4711 } ) <-- [ : rel_type_1 ] --> ( ) ) ) , variable_9 = SHORTESTPATH ( ( variable_1 : node_1 ) <-- [ ] --> ( ) )".
"MATCH ( { } ) <-- [ ] -- ( ) , variable_9 = SHORTESTPATH ( ( variable_1 : node_1 : node_2 ) -- [ ] --> ( ) )".
"MATCH variable_9 = SHORTESTPATH ( ( { } ) <-- [ ] -- ( ) ) , ( { } ) <-- [ ] -- ( )".
"MATCH ( variable_1 { } ) <-- [ : rel_type_1 | : rel_type_1 ] --> ( ) , variable_9 = SHORTESTPATH ( ( { property_key_1 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null } ) -- [ ] -- ( ) )".
"MATCH variable_9 = SHORTESTPATH ( ( { parameter_1 } ) <-- [ ? ] --> ( ) ) , ( variable_1 { } ) <-- [ : rel_type_1 | : rel_type_1 ] --> ( )". 
"MATCH ( variable_1 { } ) <-- [ : rel_type_1 | : rel_type_1 ] --> ( ) , variable_9 = ALLSHORTESTPATHS ( ( ( ) <-- --> ( ) ) )".
"MATCH variable_9 = ALLSHORTESTPATHS ( ( ( variable_1 ) -- --> ( ) ) ) , ( variable_1 { } ) <-- [ : rel_type_1 | : rel_type_1 ] --> ( )".
"MATCH ( ) <-- --> ( ) , variable_9 = ALLSHORTESTPATHS ( ( ( : node_1 ) <-- -- ( ) ) )".
"MATCH variable_9 = ALLSHORTESTPATHS ( ( ( : node_1 : node_2 ) -- -- ( ) ) ) , ( ) <-- --> ( )".
"MATCH variable_9 = ALLSHORTESTPATHS ( ( ( { 4711 } ) <-- [ : rel_type_1 ] --> ( ) ) ) , variable_9 = ALLSHORTESTPATHS ( ( ( variable_1 : node_1 ) <-- [ ] --> ( ) ) )".
"MATCH ( { } ) <-- [ ] -- ( ) , variable_9 = ALLSHORTESTPATHS ( ( ( variable_1 : node_1 : node_2 ) -- [ ] --> ( ) ) )".
"MATCH variable_9 = ALLSHORTESTPATHS ( ( ( { } ) <-- [ ] -- ( ) ) ) , ( { } ) <-- [ ] -- ( )".
"MATCH ( variable_1 { } ) <-- [ : rel_type_1 | : rel_type_1 ] --> ( ) , variable_9 = ALLSHORTESTPATHS ( ( ( { property_key_1 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null } ) -- [ ] -- ( ) ) )".
"MATCH ( variable_1 { } ) <-- [ : rel_type_1 | : rel_type_1 ] --> ( ) , variable_9 = ALLSHORTESTPATHS ( ( ( { parameter_1 } ) <-- [ ? ] --> ( ) ) ) , ( variable_1 { } ) <-- [ : rel_type_1 | : rel_type_1 ] --> ( )". 
"MATCH variable_9 = ALLSHORTESTPATHS ( ( ( { 4711 } ) <-- [ : rel_type_1 ] --> ( ) ) ) , ( variable_1 { } ) <-- [ : rel_type_1 | : rel_type_1 ] --> ( )".
"MATCH ( variable_1 { } ) <-- [ : rel_type_1 | : rel_type_1 ] --> ( ) , variable_9 = ( ) <-- --> ( )".
"MATCH variable_9 = ( variable_1 ) -- --> ( ) , ( variable_1 { } ) <-- [ : rel_type_1 | : rel_type_1 ] --> ( )".
"MATCH ( ) <-- --> ( ) , variable_9 = ( : node_1 ) <-- -- ( )".
"MATCH variable_9 = ALLSHORTESTPATHS ( ( ( { 4711 } ) <-- [ : rel_type_1 ] --> ( ) ) )variable_9 = ( variable_1 : node_1 ) <-- [ ] --> ( )".
"MATCH ( { } ) <-- [ ] -- ( ) , variable_9 = ( variable_1 : node_1 : node_2 ) -- [ ] --> ( )".
"MATCH variable_9 = ( { } ) <-- [ ] -- ( ) , ( { } ) <-- [ ] -- ( )".
"MATCH ( variable_1 { } ) <-- [ : rel_type_1 | : rel_type_1 ] --> ( ) , variable_9 = ( { property_key_1 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null } ) -- [ ] -- ( )".
"MATCH variable_9 = ( { property_key_1 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null , property_key_2 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null } ) <-- [ variable_1 ] --> ( ) , ( variable_1 { } ) <-- [ : rel_type_1 | : rel_type_1 ] --> ( )".
"MATCH variable_9 = ( { parameter_1 } ) <-- [ ? ] --> ( ) , ( { parameter_1 } ) <-- [ ? ] --> ( )".
"MATCH ( { parameter_1 } ) <-- [ ? ] --> ( ) , variable_9 = ( { 4711 } ) <-- [ : rel_type_1 ] --> ( )".
"MATCH ( ) <-- --> ( ) , ( variable_1 ) -- --> ( )".
"MATCH ( : node_1 ) <-- -- ( ) , ( : node_1 : node_2 ) -- -- ( )".
"MATCH ( variable_1 : node_1 ) <-- [ ] --> ( ) , ( variable_1 : node_1 : node_2 ) -- [ ] --> ( )".
"MATCH ( { } ) <-- [ ] -- ( ) , ( { property_key_1 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null } ) -- [ ] -- ( )".
"MATCH ( { property_key_1 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null , property_key_2 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null } ) <-- [ variable_1 ] --> ( ) , ( { parameter_1 } ) <-- [ ? ] --> ( )".
"MATCH ( { 4711 } ) <-- [ : rel_type_1 ] --> ( ) , ( variable_1 { } ) <-- [ : rel_type_1 | : rel_type_1 ] --> ( )".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% pattern_part = anonymous_pattern_part
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% pattern_part = variable = anonymous_pattern_part
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"MATCH variable_9 = SHORTESTPATH ( ( ) <-- --> ( ) )".
"MATCH variable_9 = SHORTESTPATH ( ( variable_1 ) -- --> ( ) )".
"MATCH variable_9 = SHORTESTPATH ( ( : node_1 ) <-- -- ( ) )".
"MATCH variable_9 = SHORTESTPATH ( ( : node_1 : node_2 ) -- -- ( ) )".
"MATCH variable_9 = SHORTESTPATH ( ( variable_1 : node_1 ) <-- [ ] --> ( ) )".
"MATCH variable_9 = SHORTESTPATH ( ( variable_1 : node_1 : node_2 ) -- [ ] --> ( ) )".
"MATCH variable_9 = SHORTESTPATH ( ( { } ) <-- [ ] -- ( ) )".
"MATCH variable_9 = SHORTESTPATH ( ( { property_key_1 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null } ) -- [ ] -- ( ) )".
"MATCH variable_9 = SHORTESTPATH ( ( { parameter_1 } ) <-- [ ? ] --> ( ) )". 
"MATCH variable_9 = SHORTESTPATH ( ( { 4711 } ) <-- [ : rel_type_1 ] --> ( ) )".
"MATCH variable_9 = SHORTESTPATH ( ( variable_1 { } ) <-- [ : rel_type_1 | : rel_type_1 ] --> ( ) )".
"MATCH variable_9 = SHORTESTPATH ( ( variable_1 { property_key_1 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null } ) <-- [ * ] --> ( ) )".
"MATCH variable_9 = SHORTESTPATH ( ( { property_key_1 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null , property_key_2 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null } ) <-- [ variable_1 ] --> ( ) )".
"MATCH variable_9 = SHORTESTPATH ( ( variable_1 { property_key_1 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null , property_key_2 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null } ) <-- [ * .. ] --> ( ) )".
"MATCH variable_9 = SHORTESTPATH ( ( variable_1 { parameter_1 } ) <-- [ * 12 .. ] --> ( ) )".
"MATCH variable_9 = SHORTESTPATH ( ( variable_1 { 4711 } ) <-- [ * .. 34 ] --> ( ) )".
"MATCH variable_9 = SHORTESTPATH ( ( : node_1 { } ) <-- [ * 12 .. 34 ] --> ( ) )".
"MATCH variable_9 = SHORTESTPATH ( ( variable_1 : node_1 : node_2 { property_key_1 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null , property_key_2 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null } ) <-- [ variable_1 ? : rel_type_1 | : rel_type_1 * 12 .. 34 { 4711 } ] --> ( ) )".

"MATCH variable_9 = ALLSHORTESTPATHS ( ( ( ) <-- --> ( ) ) )".
"MATCH variable_9 = ALLSHORTESTPATHS ( ( ( variable_1 ) -- --> ( ) ) )".
"MATCH variable_9 = ALLSHORTESTPATHS ( ( ( : node_1 ) <-- -- ( ) ) )".
"MATCH variable_9 = ALLSHORTESTPATHS ( ( ( : node_1 : node_2 ) -- -- ( ) ) )".
"MATCH variable_9 = ALLSHORTESTPATHS ( ( ( variable_1 : node_1 ) <-- [ ] --> ( ) ) )".
"MATCH variable_9 = ALLSHORTESTPATHS ( ( ( variable_1 : node_1 : node_2 ) -- [ ] --> ( ) ) )".
"MATCH variable_9 = ALLSHORTESTPATHS ( ( ( { } ) <-- [ ] -- ( ) ) )".
"MATCH variable_9 = ALLSHORTESTPATHS ( ( ( { property_key_1 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null } ) -- [ ] -- ( ) ) )".
"MATCH variable_9 = ALLSHORTESTPATHS ( ( ( { parameter_1 } ) <-- [ ? ] --> ( ) ) )". 
"MATCH variable_9 = ALLSHORTESTPATHS ( ( ( { 4711 } ) <-- [ : rel_type_1 ] --> ( ) ) )".
"MATCH variable_9 = ALLSHORTESTPATHS ( ( ( variable_1 { } ) <-- [ : rel_type_1 | : rel_type_1 ] --> ( ) ) )".
"MATCH variable_9 = ALLSHORTESTPATHS ( ( ( variable_1 { property_key_1 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null } ) <-- [ * ] --> ( ) ) )".
"MATCH variable_9 = ALLSHORTESTPATHS ( ( ( { property_key_1 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null , property_key_2 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null } ) <-- [ variable_1 ] --> ( ) ) )".
"MATCH variable_9 = ALLSHORTESTPATHS ( ( ( variable_1 { property_key_1 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null , property_key_2 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null } ) <-- [ * .. ] --> ( ) ) )".
"MATCH variable_9 = ALLSHORTESTPATHS ( ( ( variable_1 { parameter_1 } ) <-- [ * 12 .. ] --> ( ) ) )".
"MATCH variable_9 = ALLSHORTESTPATHS ( ( ( variable_1 { 4711 } ) <-- [ * .. 34 ] --> ( ) ) )".
"MATCH variable_9 = ALLSHORTESTPATHS ( ( ( : node_1 { } ) <-- [ * 12 .. 34 ] --> ( ) ) )".
"MATCH variable_9 = ALLSHORTESTPATHS ( ( ( variable_1 : node_1 : node_2 { property_key_1 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null , property_key_2 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null } ) <-- [ variable_1 ? : rel_type_1 | : rel_type_1 * 12 .. 34 { 4711 } ] --> ( ) ) )".

"MATCH variable_9 = ( ) <-- --> ( )".
"MATCH variable_9 = ( variable_1 ) -- --> ( )".
"MATCH variable_9 = ( : node_1 : node_2 ) -- -- ( )".
"MATCH variable_9 = ( variable_1 : node_1 ) <-- [ ] --> ( )".
"MATCH variable_9 = ( variable_1 : node_1 : node_2 ) -- [ ] --> ( )".
"MATCH variable_9 = ( { } ) <-- [ ] -- ( )".
"MATCH variable_9 = ( { property_key_1 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null } ) -- [ ] -- ( )".
"MATCH variable_9 = ( { property_key_1 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null , property_key_2 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null } ) <-- [ variable_1 ] --> ( )".
"MATCH variable_9 = ( { parameter_1 } ) <-- [ ? ] --> ( )".
"MATCH variable_9 = ( { 4711 } ) <-- [ : rel_type_1 ] --> ( )".
"MATCH variable_9 = ( variable_1 { } ) <-- [ : rel_type_1 | : rel_type_1 ] --> ( )".
"MATCH variable_9 = ( variable_1 { property_key_1 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null } ) <-- [ * ] --> ( )".
"MATCH variable_9 = ( variable_1 { property_key_1 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null , property_key_2 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null } ) <-- [ * .. ] --> ( )".
"MATCH variable_9 = ( variable_1 { parameter_1 } ) <-- [ * 12 .. ] --> ( )".
"MATCH variable_9 = ( variable_1 { 4711 } ) <-- [ * .. 34 ] --> ( )".
"MATCH variable_9 = ( : node_1 { } ) <-- [ * 12 .. 34 ] --> ( )".
"MATCH variable_9 = ( : node_1 { property_key_1 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null } ) <-- [ { } ] --> ( )".
"MATCH variable_9 = ( : node_1 { property_key_1 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null , property_key_2 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null } ) <-- [ { property_key_1 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null } ] --> ( )".
"MATCH variable_9 = ( : node_1 { parameter_1 } ) <-- [ { property_key_1 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null , property_key_2 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null } ] --> ( )".
"MATCH variable_9 = ( : node_1 { 4711 } ) <-- [ { parameter_1 } ] --> ( )".
"MATCH variable_9 = ( : node_1 : node_2 { } ) <-- [ { 4711 } ] --> ( )".
"MATCH variable_9 = ( : node_1 : node_2 { property_key_1 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null } ) <-- [ variable_1 ? ] --> ( )".
"MATCH variable_9 = ( : node_1 : node_2 { property_key_1 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null , property_key_2 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null } ) <-- [ ? ] --> ( )".
"MATCH variable_9 = ( : node_1 : node_2 { parameter_1 } ) <-- [ ? : rel_type_1 ] --> ( )".
"MATCH variable_9 = ( : node_1 : node_2 { 4711 } ) <-- [ variable_1 ? : rel_type_1 | : rel_type_1 ] --> ( )".
"MATCH variable_9 = ( variable_1 : node_1 { property_key_1 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null } ) <-- [ variable_1 ? : rel_type_1 | : rel_type_1 * ] --> ( )".
"MATCH variable_9 = ( variable_1 : node_1 { property_key_1 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null , property_key_2 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null } ) <-- [ variable_1 ? : rel_type_1 | : rel_type_1 * 12 .. 34 ] --> ( )".
"MATCH variable_9 = ( variable_1 : node_1 { parameter_1 } ) <-- [ variable_1 ? : rel_type_1 | : rel_type_1 * 12 .. 34 { property_key_1 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null } ] --> ( )".
"MATCH variable_9 = ( variable_1 : node_1 { 4711 } ) <-- [ variable_1 ? : rel_type_1 | : rel_type_1 * 12 .. 34 { property_key_1 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null , property_key_2 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null } ] --> ( )".
"MATCH variable_9 = ( variable_1 : node_1 : node_2 { } ) <-- [ variable_1 ? : rel_type_1 | : rel_type_1 * 12 .. 34 { parameter_1 } ] --> ( )".
"MATCH variable_9 = ( variable_1 : node_1 : node_2 { property_key_1 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null , property_key_2 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null } ) <-- [ variable_1 ? : rel_type_1 | : rel_type_1 * 12 .. 34 { 4711 } ] --> ( )".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% anonymous_pattern_part = shortest_path_pattern
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% anonymous_pattern_part = pattern_element
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% shortest_path_pattern = SHORTESTPATH ( pattern_element )
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"MATCH SHORTESTPATH ( ( ) <-- --> ( ) )".
"MATCH SHORTESTPATH ( ( variable_1 ) -- --> ( ) )".
"MATCH SHORTESTPATH ( ( : node_1 ) <-- -- ( ) )".
"MATCH SHORTESTPATH ( ( : node_1 : node_2 ) -- -- ( ) )".
"MATCH SHORTESTPATH ( ( variable_1 : node_1 ) <-- [ ] --> ( ) )".
"MATCH SHORTESTPATH ( ( variable_1 : node_1 : node_2 ) -- [ ] --> ( ) )".
"MATCH SHORTESTPATH ( ( { } ) <-- [ ] -- ( ) )".
"MATCH SHORTESTPATH ( ( { property_key_1 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null } ) -- [ ] -- ( ) )".
"MATCH SHORTESTPATH ( ( { parameter_1 } ) <-- [ ? ] --> ( ) )". 
"MATCH SHORTESTPATH ( ( { 4711 } ) <-- [ : rel_type_1 ] --> ( ) )".
"MATCH SHORTESTPATH ( ( variable_1 { } ) <-- [ : rel_type_1 | : rel_type_1 ] --> ( ) )".
"MATCH SHORTESTPATH ( ( variable_1 { property_key_1 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null } ) <-- [ * ] --> ( ) )".
"MATCH SHORTESTPATH ( ( { property_key_1 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null , property_key_2 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null } ) <-- [ variable_1 ] --> ( ) )".
"MATCH SHORTESTPATH ( ( variable_1 { property_key_1 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null , property_key_2 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null } ) <-- [ * .. ] --> ( ) )".
"MATCH SHORTESTPATH ( ( variable_1 { parameter_1 } ) <-- [ * 12 .. ] --> ( ) )".
"MATCH SHORTESTPATH ( ( variable_1 { 4711 } ) <-- [ * .. 34 ] --> ( ) )".
"MATCH SHORTESTPATH ( ( : node_1 { } ) <-- [ * 12 .. 34 ] --> ( ) )".
"MATCH SHORTESTPATH ( ( variable_1 : node_1 : node_2 { property_key_1 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null , property_key_2 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null } ) <-- [ variable_1 ? : rel_type_1 | : rel_type_1 * 12 .. 34 { 4711 } ] --> ( ) )".

"MATCH SHORTESTPATH ( ( ( ) <-- --> ( ) ) )".
"MATCH SHORTESTPATH ( ( ( variable_1 ) -- --> ( ) ) )".
"MATCH SHORTESTPATH ( ( ( : node_1 ) <-- -- ( ) ) )".
"MATCH SHORTESTPATH ( ( ( : node_1 : node_2 ) -- -- ( ) ) )".
"MATCH SHORTESTPATH ( ( ( variable_1 : node_1 ) <-- [ ] --> ( ) ) )".
"MATCH SHORTESTPATH ( ( ( variable_1 : node_1 : node_2 ) -- [ ] --> ( ) ) )".
"MATCH SHORTESTPATH ( ( ( { } ) <-- [ ] -- ( ) ) )".
"MATCH SHORTESTPATH ( ( ( { property_key_1 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null } ) -- [ ] -- ( ) ) )".
"MATCH SHORTESTPATH ( ( ( { parameter_1 } ) <-- [ ? ] --> ( ) ) )". 
"MATCH SHORTESTPATH ( ( ( { 4711 } ) <-- [ : rel_type_1 ] --> ( ) ) )".
"MATCH SHORTESTPATH ( ( ( variable_1 { } ) <-- [ : rel_type_1 | : rel_type_1 ] --> ( ) ) )".
"MATCH SHORTESTPATH ( ( ( variable_1 { property_key_1 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null } ) <-- [ * ] --> ( ) ) )".
"MATCH SHORTESTPATH ( ( ( { property_key_1 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null , property_key_2 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null } ) <-- [ variable_1 ] --> ( ) ) )".
"MATCH SHORTESTPATH ( ( ( variable_1 { property_key_1 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null , property_key_2 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null } ) <-- [ * .. ] --> ( ) ) )".
"MATCH SHORTESTPATH ( ( ( variable_1 { parameter_1 } ) <-- [ * 12 .. ] --> ( ) ) )".
"MATCH SHORTESTPATH ( ( ( variable_1 { 4711 } ) <-- [ * .. 34 ] --> ( ) ) )".
"MATCH SHORTESTPATH ( ( ( : node_1 { } ) <-- [ * 12 .. 34 ] --> ( ) ) )".
"MATCH SHORTESTPATH ( ( ( variable_1 : node_1 : node_2 { property_key_1 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null , property_key_2 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null } ) <-- [ variable_1 ? : rel_type_1 | : rel_type_1 * 12 .. 34 { 4711 } ] --> ( ) ) )".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% shortest_path_pattern = ALLSHORTESTPATH ( pattern_element )
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"MATCH ALLSHORTESTPATHS ( ( ) <-- --> ( ) )".
"MATCH ALLSHORTESTPATHS ( ( variable_1 ) -- --> ( ) )".
"MATCH ALLSHORTESTPATHS ( ( : node_1 ) <-- -- ( ) )".
"MATCH ALLSHORTESTPATHS ( ( : node_1 : node_2 ) -- -- ( ) )".
"MATCH ALLSHORTESTPATHS ( ( variable_1 : node_1 ) <-- [ ] --> ( ) )".
"MATCH ALLSHORTESTPATHS ( ( variable_1 : node_1 : node_2 ) -- [ ] --> ( ) )".
"MATCH ALLSHORTESTPATHS ( ( { } ) <-- [ ] -- ( ) )".
"MATCH ALLSHORTESTPATHS ( ( { property_key_1 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null } ) -- [ ] -- ( ) )".
"MATCH ALLSHORTESTPATHS ( ( { parameter_1 } ) <-- [ ? ] --> ( ) )". 
"MATCH ALLSHORTESTPATHS ( ( { 4711 } ) <-- [ : rel_type_1 ] --> ( ) )".
"MATCH ALLSHORTESTPATHS ( ( variable_1 { } ) <-- [ : rel_type_1 | : rel_type_1 ] --> ( ) )".
"MATCH ALLSHORTESTPATHS ( ( variable_1 { property_key_1 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null } ) <-- [ * ] --> ( ) )".
"MATCH ALLSHORTESTPATHS ( ( { property_key_1 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null , property_key_2 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null } ) <-- [ variable_1 ] --> ( ) )".
"MATCH ALLSHORTESTPATHS ( ( variable_1 { property_key_1 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null , property_key_2 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null } ) <-- [ * .. ] --> ( ) )".
"MATCH ALLSHORTESTPATHS ( ( variable_1 { parameter_1 } ) <-- [ * 12 .. ] --> ( ) )".
"MATCH ALLSHORTESTPATHS ( ( variable_1 { 4711 } ) <-- [ * .. 34 ] --> ( ) )".
"MATCH ALLSHORTESTPATHS ( ( : node_1 { } ) <-- [ * 12 .. 34 ] --> ( ) )".
"MATCH ALLSHORTESTPATHS ( ( variable_1 : node_1 : node_2 { property_key_1 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null , property_key_2 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null } ) <-- [ variable_1 ? : rel_type_1 | : rel_type_1 * 12 .. 34 { 4711 } ] --> ( ) )".

"MATCH ALLSHORTESTPATHS ( ( ( ) <-- --> ( ) ) )".
"MATCH ALLSHORTESTPATHS ( ( ( variable_1 ) -- --> ( ) ) )".
"MATCH ALLSHORTESTPATHS ( ( ( : node_1 ) <-- -- ( ) ) )".
"MATCH ALLSHORTESTPATHS ( ( ( : node_1 : node_2 ) -- -- ( ) ) )".
"MATCH ALLSHORTESTPATHS ( ( ( variable_1 : node_1 ) <-- [ ] --> ( ) ) )".
"MATCH ALLSHORTESTPATHS ( ( ( variable_1 : node_1 : node_2 ) -- [ ] --> ( ) ) )".
"MATCH ALLSHORTESTPATHS ( ( ( { } ) <-- [ ] -- ( ) ) )".
"MATCH ALLSHORTESTPATHS ( ( ( { property_key_1 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null } ) -- [ ] -- ( ) ) )".
"MATCH ALLSHORTESTPATHS ( ( ( { parameter_1 } ) <-- [ ? ] --> ( ) ) )". 
"MATCH ALLSHORTESTPATHS ( ( ( { 4711 } ) <-- [ : rel_type_1 ] --> ( ) ) )".
"MATCH ALLSHORTESTPATHS ( ( ( variable_1 { } ) <-- [ : rel_type_1 | : rel_type_1 ] --> ( ) ) )".
"MATCH ALLSHORTESTPATHS ( ( ( variable_1 { property_key_1 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null } ) <-- [ * ] --> ( ) ) )".
"MATCH ALLSHORTESTPATHS ( ( ( { property_key_1 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null , property_key_2 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null } ) <-- [ variable_1 ] --> ( ) ) )".
"MATCH ALLSHORTESTPATHS ( ( ( variable_1 { property_key_1 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null , property_key_2 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null } ) <-- [ * .. ] --> ( ) ) )".
"MATCH ALLSHORTESTPATHS ( ( ( variable_1 { parameter_1 } ) <-- [ * 12 .. ] --> ( ) ) )".
"MATCH ALLSHORTESTPATHS ( ( ( variable_1 { 4711 } ) <-- [ * .. 34 ] --> ( ) ) )".
"MATCH ALLSHORTESTPATHS ( ( ( : node_1 { } ) <-- [ * 12 .. 34 ] --> ( ) ) )".
"MATCH ALLSHORTESTPATHS ( ( ( variable_1 : node_1 : node_2 { property_key_1 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null , property_key_2 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null } ) <-- [ variable_1 ? : rel_type_1 | : rel_type_1 * 12 .. 34 { 4711 } ] --> ( ) ) )".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% (2) pattern_element = node_pattern pattern_element_chain
%% -----------------------------------------------------------------------------
%% (1) pattern_element_chain = relationship_pattern node_pattern 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"MATCH ( ) -- --> ( )".
"MATCH ( ) <-- -- ( )".
"MATCH ( ) -- -- ( )".
"MATCH ( ) <-- [ ] --> ( )".
"MATCH ( ) -- [ ] --> ( )".
"MATCH ( ) <-- [ ] -- ( )".
"MATCH ( ) -- [ ] -- ( )".
"MATCH ( ) <-- [ variable_1 ] --> ( )".
"MATCH ( ) <-- [ ? ] --> ( )".
"MATCH ( ) <-- [ : rel_type_1 ] --> ( )".
"MATCH ( ) <-- [ : rel_type_1 | : rel_type_1 ] --> ( )".
"MATCH ( ) <-- [ * ] --> ( )".
"MATCH ( ) <-- [ * .. ] --> ( )".
"MATCH ( ) <-- [ * 12 .. ] --> ( )".
"MATCH ( ) <-- [ * .. 34 ] --> ( )".
"MATCH ( ) <-- [ * 12 .. 34 ] --> ( )".
"MATCH ( ) <-- [ { } ] --> ( )".
"MATCH ( ) <-- [ { property_key_1 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null } ] --> ( )".
"MATCH ( ) <-- [ { property_key_1 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null , property_key_2 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null } ] --> ( )".
"MATCH ( ) <-- [ { parameter_1 } ] --> ( )".
"MATCH ( ) <-- [ { 4711 } ] --> ( )".
"MATCH ( ) <-- [ variable_1 ? ] --> ( )".
"MATCH ( ) <-- [ ? : rel_type_1 ] --> ( )".
"MATCH ( ) <-- [ variable_1 ? : rel_type_1 | : rel_type_1 ] --> ( )".
"MATCH ( ) <-- [ variable_1 ? : rel_type_1 | : rel_type_1 * ] --> ( )".
"MATCH ( ) <-- [ variable_1 ? : rel_type_1 | : rel_type_1 * 12 .. 34 ] --> ( )".
"MATCH ( ) <-- [ variable_1 ? : rel_type_1 | : rel_type_1 * 12 .. 34 { property_key_1 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null } ] --> ( )".
"MATCH ( ) <-- [ variable_1 ? : rel_type_1 | : rel_type_1 * 12 .. 34 { property_key_1 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null , property_key_2 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null } ] --> ( )".
"MATCH ( ) <-- [ variable_1 ? : rel_type_1 | : rel_type_1 * 12 .. 34 { parameter_1 } ] --> ( )".
"MATCH ( ) <-- [ variable_1 ? : rel_type_1 | : rel_type_1 * 12 .. 34 { 4711 } ] --> ( )".

"MATCH ( ) <-- --> ( )".
"MATCH ( variable_1 ) -- --> ( )".
"MATCH ( : node_1 ) <-- -- ( )".
"MATCH ( : node_1 : node_2 ) -- -- ( )".
"MATCH ( variable_1 : node_1 ) <-- [ ] --> ( )".
"MATCH ( variable_1 : node_1 : node_2 ) -- [ ] --> ( )".
"MATCH ( { } ) <-- [ ] -- ( )".
"MATCH ( { property_key_1 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null } ) -- [ ] -- ( )".
"MATCH ( { property_key_1 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null , property_key_2 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null } ) <-- [ variable_1 ] --> ( )".
"MATCH ( { parameter_1 } ) <-- [ ? ] --> ( )".
"MATCH ( { 4711 } ) <-- [ : rel_type_1 ] --> ( )".
"MATCH ( variable_1 { } ) <-- [ : rel_type_1 | : rel_type_1 ] --> ( )".
"MATCH ( variable_1 { property_key_1 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null } ) <-- [ * ] --> ( )".
"MATCH ( variable_1 { property_key_1 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null , property_key_2 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null } ) <-- [ * .. ] --> ( )".
"MATCH ( variable_1 { parameter_1 } ) <-- [ * 12 .. ] --> ( )".
"MATCH ( variable_1 { 4711 } ) <-- [ * .. 34 ] --> ( )".
"MATCH ( : node_1 { } ) <-- [ * 12 .. 34 ] --> ( )".
"MATCH ( : node_1 { property_key_1 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null } ) <-- [ { } ] --> ( )".
"MATCH ( : node_1 { property_key_1 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null , property_key_2 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null } ) <-- [ { property_key_1 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null } ] --> ( )".
"MATCH ( : node_1 { parameter_1 } ) <-- [ { property_key_1 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null , property_key_2 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null } ] --> ( )".
"MATCH ( : node_1 { 4711 } ) <-- [ { parameter_1 } ] --> ( )".
"MATCH ( : node_1 : node_2 { } ) <-- [ { 4711 } ] --> ( )".
"MATCH ( : node_1 : node_2 { property_key_1 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null } ) <-- [ variable_1 ? ] --> ( )".
"MATCH ( : node_1 : node_2 { property_key_1 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null , property_key_2 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null } ) <-- [ ? ] --> ( )".
"MATCH ( : node_1 : node_2 { parameter_1 } ) <-- [ ? : rel_type_1 ] --> ( )".
"MATCH ( : node_1 : node_2 { 4711 } ) <-- [ variable_1 ? : rel_type_1 | : rel_type_1 ] --> ( )".
"MATCH ( variable_1 : node_1 { property_key_1 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null } ) <-- [ variable_1 ? : rel_type_1 | : rel_type_1 * ] --> ( )".
"MATCH ( variable_1 : node_1 { property_key_1 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null , property_key_2 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null } ) <-- [ variable_1 ? : rel_type_1 | : rel_type_1 * 12 .. 34 ] --> ( )".
"MATCH ( variable_1 : node_1 { parameter_1 } ) <-- [ variable_1 ? : rel_type_1 | : rel_type_1 * 12 .. 34 { property_key_1 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null } ] --> ( )".
"MATCH ( variable_1 : node_1 { 4711 } ) <-- [ variable_1 ? : rel_type_1 | : rel_type_1 * 12 .. 34 { property_key_1 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null , property_key_2 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null } ] --> ( )".
"MATCH ( variable_1 : node_1 : node_2 { } ) <-- [ variable_1 ? : rel_type_1 | : rel_type_1 * 12 .. 34 { parameter_1 } ] --> ( )".
"MATCH ( variable_1 : node_1 : node_2 { property_key_1 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null , property_key_2 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null } ) <-- [ variable_1 ? : rel_type_1 | : rel_type_1 * 12 .. 34 { 4711 } ] --> ( )".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% pattern_element = ( pattern_element )
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"MATCH ( ( ) <-- --> ( ) )".
"MATCH ( ( variable_1 ) -- --> ( ) )".
"MATCH ( ( : node_1 ) <-- -- ( ) )".
"MATCH ( ( : node_1 : node_2 ) -- -- ( ) )".
"MATCH ( ( variable_1 : node_1 ) <-- [ ] --> ( ) )".
"MATCH ( ( variable_1 : node_1 : node_2 ) -- [ ] --> ( ) )".
"MATCH ( ( { } ) <-- [ ] -- ( ) )".
"MATCH ( ( { property_key_1 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null } ) -- [ ] -- ( ) )".
"MATCH ( ( { property_key_1 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null , property_key_2 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null } ) <-- [ variable_1 ] --> ( ) )".
"MATCH ( ( { parameter_1 } ) <-- [ ? ] --> ( ) )".
"MATCH ( ( { 4711 } ) <-- [ : rel_type_1 ] --> ( ) )".
"MATCH ( ( variable_1 { } ) <-- [ : rel_type_1 | : rel_type_1 ] --> ( ) )".
"MATCH ( ( variable_1 { property_key_1 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null } ) <-- [ * ] --> ( ) )".
"MATCH ( ( variable_1 { property_key_1 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null , property_key_2 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null } ) <-- [ * .. ] --> ( ) )".
"MATCH ( ( variable_1 { parameter_1 } ) <-- [ * 12 .. ] --> ( ) )".
"MATCH ( ( variable_1 { 4711 } ) <-- [ * .. 34 ] --> ( ) )".
"MATCH ( ( : node_1 { } ) <-- [ * 12 .. 34 ] --> ( ) )".
"MATCH ( ( : node_1 { property_key_1 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null } ) <-- [ { } ] --> ( ) )".
"MATCH ( ( : node_1 { property_key_1 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null , property_key_2 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null } ) <-- [ { property_key_1 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null } ] --> ( ) )".
"MATCH ( ( : node_1 { parameter_1 } ) <-- [ { property_key_1 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null , property_key_2 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null } ] --> ( ) )".
"MATCH ( ( : node_1 { 4711 } ) <-- [ { parameter_1 } ] --> ( ) )".
"MATCH ( ( : node_1 : node_2 { } ) <-- [ { 4711 } ] --> ( ) )".
"MATCH ( ( : node_1 : node_2 { property_key_1 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null } ) <-- [ variable_1 ? ] --> ( ) )".
"MATCH ( ( : node_1 : node_2 { property_key_1 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null , property_key_2 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null } ) <-- [ ? ] --> ( ) )".
"MATCH ( ( : node_1 : node_2 { parameter_1 } ) <-- [ ? : rel_type_1 ] --> ( ) )".
"MATCH ( ( : node_1 : node_2 { 4711 } ) <-- [ variable_1 ? : rel_type_1 | : rel_type_1 ] --> ( ) )".
"MATCH ( ( variable_1 : node_1 { property_key_1 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null } ) <-- [ variable_1 ? : rel_type_1 | : rel_type_1 * ] --> ( ) )".
"MATCH ( ( variable_1 : node_1 { property_key_1 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null , property_key_2 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null } ) <-- [ variable_1 ? : rel_type_1 | : rel_type_1 * 12 .. 34 ] --> ( ) )".
"MATCH ( ( variable_1 : node_1 { parameter_1 } ) <-- [ variable_1 ? : rel_type_1 | : rel_type_1 * 12 .. 34 { property_key_1 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null } ] --> ( ) )".
"MATCH ( ( variable_1 : node_1 { 4711 } ) <-- [ variable_1 ? : rel_type_1 | : rel_type_1 * 12 .. 34 { property_key_1 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null , property_key_2 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null } ] --> ( ) )".
"MATCH ( ( variable_1 : node_1 : node_2 { } ) <-- [ variable_1 ? : rel_type_1 | : rel_type_1 * 12 .. 34 { parameter_1 } ] --> ( ) )".
"MATCH ( ( variable_1 : node_1 : node_2 { property_key_1 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null , property_key_2 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null } ) <-- [ variable_1 ? : rel_type_1 | : rel_type_1 * 12 .. 34 { 4711 } ] --> ( ) )".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% pattern_element = node_pattern
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% node_pattern = ( variable_opt node_labels_opt properties_opt )
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"MATCH ( )".
"MATCH ( variable_1 )".
"MATCH ( : node_1 )".
"MATCH ( : node_1 : node_2 )".
"MATCH ( variable_1 : node_1 )".
"MATCH ( variable_1 : node_1 : node_2 )".
"MATCH ( { } )".
"MATCH ( { property_key_1 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null } )".
"MATCH ( { property_key_1 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null , property_key_2 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null } )".
"MATCH ( { parameter_1 } )".
"MATCH ( { 4711 } )".
"MATCH ( variable_1 { } )".
"MATCH ( variable_1 { property_key_1 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null } )".
"MATCH ( variable_1 { property_key_1 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null , property_key_2 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null } )".
"MATCH ( variable_1 { parameter_1 } )".
"MATCH ( variable_1 { 4711 } )".
"MATCH ( : node_1 { } )".
"MATCH ( : node_1 { property_key_1 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null } )".
"MATCH ( : node_1 { property_key_1 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null , property_key_2 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null } )".
"MATCH ( : node_1 { parameter_1 } )".
"MATCH ( : node_1 { 4711 } )".
"MATCH ( : node_1 : node_2 { } )".
"MATCH ( : node_1 : node_2 { property_key_1 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null } )".
"MATCH ( : node_1 : node_2 { property_key_1 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null , property_key_2 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null } )".
"MATCH ( : node_1 : node_2 { parameter_1 } )".
"MATCH ( : node_1 : node_2 { 4711 } )".
"MATCH ( variable_1 : node_1 { } )".
"MATCH ( variable_1 : node_1 { property_key_1 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null } )".
"MATCH ( variable_1 : node_1 { property_key_1 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null , property_key_2 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null } )".
"MATCH ( variable_1 : node_1 { parameter_1 } )".
"MATCH ( variable_1 : node_1 { 4711 } )".
"MATCH ( variable_1 : node_1 : node_2 { } )".
"MATCH ( variable_1 : node_1 : node_2 { property_key_1 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null } )".
"MATCH ( variable_1 : node_1 : node_2 { property_key_1 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null , property_key_2 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null } )".
"MATCH ( variable_1 : node_1 : node_2 { parameter_1 } )".
"MATCH ( variable_1 : node_1 : node_2 { 4711 } )".
