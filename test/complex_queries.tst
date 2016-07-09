%%-*- mode: erlang -*-
%%-*- coding: utf-8 -*-

% Test query options
[{tests, []}].

%% 
%% TESTS
%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Comment
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"UNWIND Name1 .property_4711 as variable_1 // This is a comment".
"UNWIND /* comment 1 */ Name1 .property_4711 as variable_1".
"UNWIND /* comment 2 */ Name1 .property_4711 as variable_1".
"UNWIND /* comment 2 */ Name1 .property_4711 as variable_1 // This is a line comment".
"UNWIND /* comment line 1
                   line 2
                   line 3 */ Name1 .property_4711 as variable_1".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% RegularQuery
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"MATCH variable_9 = ( variable_1 ) - -> ( ) , ( variable_1 { } ) <- [ : rel_type_1 | : rel_type_1 ] -> ( ) UNION MATCH variable_9 = ( variable_1 ) - -> ( ) , ( variable_1 { } ) <- [ : rel_type_1 | : rel_type_1 ] -> ( )".
"MATCH variable_9 = ( variable_1 ) - -> ( ) , ( variable_1 { } ) <- [ : rel_type_1 | : rel_type_1 ] -> ( ) UNION MATCH variable_9 = ( variable_1 ) - -> ( ) , ( variable_1 { } ) <- [ : rel_type_1 | : rel_type_1 ] -> ( ) UNION MATCH variable_9 = ( variable_1 ) - -> ( ) , ( variable_1 { } ) <- [ : rel_type_1 | : rel_type_1 ] -> ( )".
"MATCH variable_9 = ( variable_1 ) - -> ( ) , ( variable_1 { } ) <- [ : rel_type_1 | : rel_type_1 ] -> ( ) RETURN -'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 UNION MATCH variable_9 = ( variable_1 ) - -> ( ) , ( variable_1 { } ) <- [ : rel_type_1 | : rel_type_1 ] -> ( ) WITH -'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1".
"MATCH variable_9 = ( variable_1 ) - -> ( ) , ( variable_1 { } ) <- [ : rel_type_1 | : rel_type_1 ] -> ( ) WITH -'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 UNION MATCH variable_9 = ( variable_1 ) - -> ( ) , ( variable_1 { } ) <- [ : rel_type_1 | : rel_type_1 ] -> ( ) WITH -'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 RETURN -'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1".
"MATCH variable_9 = ( variable_1 ) - -> ( ) , ( variable_1 { } ) <- [ : rel_type_1 | : rel_type_1 ] -> ( ) UNION ALL MATCH variable_9 = ( variable_1 ) - -> ( ) , ( variable_1 { } ) <- [ : rel_type_1 | : rel_type_1 ] -> ( )".
"MATCH variable_9 = ( variable_1 ) - -> ( ) , ( variable_1 { } ) <- [ : rel_type_1 | : rel_type_1 ] -> ( ) UNION ALL MATCH variable_9 = ( variable_1 ) - -> ( ) , ( variable_1 { } ) <- [ : rel_type_1 | : rel_type_1 ] -> ( ) UNION ALL MATCH variable_9 = ( variable_1 ) - -> ( ) , ( variable_1 { } ) <- [ : rel_type_1 | : rel_type_1 ] -> ( )".
"MATCH variable_9 = ( variable_1 ) - -> ( ) , ( variable_1 { } ) <- [ : rel_type_1 | : rel_type_1 ] -> ( ) RETURN -'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 UNION ALL MATCH variable_9 = ( variable_1 ) - -> ( ) , ( variable_1 { } ) <- [ : rel_type_1 | : rel_type_1 ] -> ( ) WITH -'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1".
"MATCH variable_9 = ( variable_1 ) - -> ( ) , ( variable_1 { } ) <- [ : rel_type_1 | : rel_type_1 ] -> ( ) WITH -'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 UNION ALL MATCH variable_9 = ( variable_1 ) - -> ( ) , ( variable_1 { } ) <- [ : rel_type_1 | : rel_type_1 ] -> ( ) WITH -'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 RETURN -'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% SingleQuery
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"MATCH variable_9 = ( variable_1 ) - -> ( ) , ( variable_1 { } ) <- [ : rel_type_1 | : rel_type_1 ] -> ( )".
"MATCH variable_9 = ( variable_1 ) - -> ( ) , ( variable_1 { } ) <- [ : rel_type_1 | : rel_type_1 ] -> ( ) RETURN -'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1".
"MATCH variable_9 = ( variable_1 ) - -> ( ) , ( variable_1 { } ) <- [ : rel_type_1 | : rel_type_1 ] -> ( ) WITH -'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1".
"MATCH variable_9 = ( variable_1 ) - -> ( ) , ( variable_1 { } ) <- [ : rel_type_1 | : rel_type_1 ] -> ( ) WITH -'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 RETURN -'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1".
