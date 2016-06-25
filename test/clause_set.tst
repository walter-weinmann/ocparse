%%-*- mode: erlang -*-
%%-*- coding: utf-8 -*-

% Test query options
[{tests, []}].

%% 
%% TESTS
%%

"SET 'Dies\" ist \"ein String' .Property_key_1 = - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1".
"SET 'Dies\" ist \"ein String' .Property_key_1? .Property_key_1! = + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1".
"SET 'Dies\" ist \"ein String' .Property_key_1? .Property_key_1! = 'test' .property_1 .property2? :label_1 :label_2 .property_3 .property4? , variable_1 : node_1 : node_2".
"SET 'Dies\" ist \"ein String' = 'test' .property_1 .property2? :label_1 :label_2 .property_3 .property4?".
"SET variable_1 : node_1".
"SET variable_1 : node_1 : node_2".
"SET variable_1 : node_1 : node_2 , 'Dies\" ist \"ein String' = 'test' .property_1 .property2? :label_1 :label_2 .property_3 .property4?".
"SET variable_1 += + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1".
"SET variable_1 = + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1".
