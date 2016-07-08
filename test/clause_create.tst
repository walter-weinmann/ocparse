%%-*- mode: erlang -*-
%%-*- coding: utf-8 -*-

% Test query options
[{tests, []}].

%% 
%% TESTS
%%

"CREATE  ( ( variable_1 :node_1 { parameter_1 } ) <- [ variable_1 ?  : rel_type_1 | : rel_type_2 * 12 .. 34 { parameter_1 } ] -> ( variable_1 :node_1 { parameter_1 } ) )".
"CREATE variable_1 =  ( ( variable_1 :node_1 { parameter_1 } ) <- [ variable_1 ?  : rel_type_1 | : rel_type_2 * 12 .. 34 { parameter_1 } ] -> ( variable_1 :node_1 { parameter_1 } ) )".
"CrEATE variable_1 =  ( ( variable_1 :node_1 { parameter_1 } ) <- [ variable_1 ?  : rel_type_1 | : rel_type_2 * 12 .. 34 { parameter_1 } ] -> ( variable_1 :node_1 { parameter_1 } ) )".
"CREATE variable_1 =  ( ( variable_1 :node_1 { parameter_1 } ) <- [ variable_1 ?  : rel_type_1 | : rel_type_2 * 12 .. 34 { parameter_1 } ] -> ( variable_1 :node_1 { parameter_1 } ) ) , variable_1 =  ( ( variable_1 :node_1 { parameter_1 } ) <- [ variable_1 ?  : rel_type_1 | : rel_type_2 * 12 .. 34 { parameter_1 } ] -> ( variable_1 :node_1 { parameter_1 } ) )".
