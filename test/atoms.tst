%%-*- mode: erlang -*-
%%-*- coding: utf-8 -*-

% Test query options
[{tests, []}].

%%
%% TESTS
%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ALL, ANY, EXTRACT, FILTER, NONE, SINGLE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"UNWIND fIlter ( variable_1 iN nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null wHere nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null ) .property_4711 as variable_1".
"UNWIND fIlter ( variable_1 iN 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null wHere nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null ) .property_4711 as variable_1".
"UNWIND eXtract ( variable_1 iN nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null wHere nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null | nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null ) .property_4711 as variable_1".
"UNWIND eXtract ( variable_1 iN nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null wHere nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null ) .property_4711 as variable_1".
"UNWIND aLl ( variable_1 iN nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null wHere nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null ) .property_4711 as variable_1".
"UNWIND aNy ( variable_1 iN nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null wHere nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null ) .property_4711 as variable_1".
"UNWIND nOne ( variable_1 iN nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null wHere nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null ) .property_4711 as variable_1".
"UNWIND sIngle ( variable_1 iN nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null wHere nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null ) .property_4711 as variable_1".
"UNWIND fIlter(variable_1 iN nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null wHere nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null) .property_4711 as variable_1".
"UNWIND eXtract(variable_1 iN nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null wHere nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null|nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null) .property_4711 as variable_1".
"UNWIND eXtract(variable_1 iN nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null wHere nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null) .property_4711 as variable_1".
"UNWIND aLl(variable_1 iN nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null wHere nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null) .property_4711 as variable_1".
"UNWIND aNy(variable_1 iN nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null wHere nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null) .property_4711 as variable_1".
"UNWIND nOne(variable_1 iN nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null wHere nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null) .property_4711 as variable_1".
"UNWIND sIngle(variable_1 iN nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null wHere nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null) .property_4711 as variable_1".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Constant
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"UNWIND TRUE .property_4711 as variable_1".
"UNWIND FALSE .property_4711 as variable_1".
"UNWIND NULL .property_4711 as variable_1".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% COUNT(*)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"UNWIND count(*) .property_4711 as variable_1".
"UNWIND count ( * ) .property_4711 as variable_1".
"UNWIND COUNT(*) .property_4711 as variable_1".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Expression List
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"UNWIND [ nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null ] .property_4711 as variable_1".
"UNWIND [nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null] .property_4711 as variable_1".
"UNWIND [ nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null , nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null ] .property_4711 as variable_1".
"UNWIND [nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null,nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null] .property_4711 as variable_1".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function Invocation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"UNWIND function_1 ( ) .property_4711 as variable_1".
"UNWIND function_1 ( dIstinct ) .property_4711 as variable_1".
"UNWIND function_1 ( nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null ) .property_4711 as variable_1".
"UNWIND function_1 ( dIstinct nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null ) .property_4711 as variable_1".
"UNWIND function_1 ( nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null , nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null ) .property_4711 as variable_1".
"UNWIND function_1 ( dIstinct nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null , nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null ) .property_4711 as variable_1".
"UNWIND function_1() .property_4711 as variable_1".
"UNWIND function_1(dIstinct) .property_4711 as variable_1".
"UNWIND function_1(nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null) .property_4711 as variable_1".
"UNWIND function_1(dIstinct nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null) .property_4711 as variable_1".
"UNWIND function_1(dIstinct 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null) .property_4711 as variable_1".
"UNWIND function_1(nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null , nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null) .property_4711 as variable_1".
"UNWIND function_1(dIstinct nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null , nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null) .property_4711 as variable_1".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% List Comprehension
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"UNWIND [ variable_1 iN nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null ] .property_4711 as variable_1".
"UNWIND [ variable_1 iN nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null wHere nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null ] .property_4711 as variable_1".
"UNWIND [ variable_1 iN nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null wHere 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null ] .property_4711 as variable_1".
"UNWIND [ variable_1 iN nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null | nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null ] .property_4711 as variable_1".
"UNWIND [ variable_1 iN nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null wHere nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null | nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null ] .property_4711 as variable_1".
"UNWIND [variable_1 iN nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null] .property_4711 as variable_1".
"UNWIND [variable_1 iN nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null wHere nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null] .property_4711 as variable_1".
"UNWIND [variable_1 iN nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null|nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null] .property_4711 as variable_1".
"UNWIND [variable_1 iN nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null wHere nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null|nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null] .property_4711 as variable_1".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Map Literal
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"UNWIND { } .property_4711 as variable_1".
"UNWIND { property_1 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null } .property_4711 as variable_1".
"UNWIND { property_1 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null , property_2 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null } .property_4711 as variable_1".
"UNWIND {} .property_4711 as variable_1".
"UNWIND {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null} .property_4711 as variable_1".
"UNWIND {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null,property_2:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null} .property_4711 as variable_1".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Number Literal
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Decimal Integer
"UNWIND 1 .property_4711 as variable_1".
"UNWIND 4711 .property_4711 as variable_1".
"UNWIND 8 .property_4711 as variable_1".
"UNWIND -1 .property_4711 as variable_1".
"UNWIND -4711 .property_4711 as variable_1".
"UNWIND -8 .property_4711 as variable_1".

%% Hexadecimal Integer
"UNWIND 0X0 .property_4711 as variable_1".
"UNWIND 0XA .property_4711 as variable_1".
"UNWIND 0X0123456789ABCDEF .property_4711 as variable_1".
"UNWIND -0X0 .property_4711 as variable_1".
"UNWIND -0XA .property_4711 as variable_1".
"UNWIND -0X0123456789ABCDEF .property_4711 as variable_1".

%% Octal Integer
"UNWIND 00 .property_4711 as variable_1".
"UNWIND 001234567 .property_4711 as variable_1".
"UNWIND -00 .property_4711 as variable_1".
"UNWIND -001234567 .property_4711 as variable_1".

%% Exponential Decimal Real
"UNWIND .E0 .property_4711 as variable_1".
"UNWIND -.E0 .property_4711 as variable_1".
"UNWIND 0.E0 .property_4711 as variable_1".
"UNWIND -0.E0 .property_4711 as variable_1".
"UNWIND 0E0 .property_4711 as variable_1".
"UNWIND 0E789 .property_4711 as variable_1".
"UNWIND 9E0 .property_4711 as variable_1".
"UNWIND 9E789 .property_4711 as variable_1".
"UNWIND 0E-0 .property_4711 as variable_1".
"UNWIND 0E-789 .property_4711 as variable_1".
"UNWIND 9E-0 .property_4711 as variable_1".
"UNWIND 9E-789 .property_4711 as variable_1".
"UNWIND -0E0 .property_4711 as variable_1".
"UNWIND -0E789 .property_4711 as variable_1".
"UNWIND -9E0 .property_4711 as variable_1".
"UNWIND -9E789 .property_4711 as variable_1".
"UNWIND -0E-0 .property_4711 as variable_1".
"UNWIND -0E-789 .property_4711 as variable_1".
"UNWIND -9E-0 .property_4711 as variable_1".
"UNWIND -9E-789 .property_4711 as variable_1".

%% Regular Decimal Real
"UNWIND .0 .property_4711 as variable_1".
"UNWIND .00 .property_4711 as variable_1".
"UNWIND .01 .property_4711 as variable_1".
"UNWIND .08 .property_4711 as variable_1".
"UNWIND .1 .property_4711 as variable_1".
"UNWIND .12345 .property_4711 as variable_1".
"UNWIND .8 .property_4711 as variable_1".
"UNWIND 0.0 .property_4711 as variable_1".
"UNWIND 0.1 .property_4711 as variable_1".
"UNWIND 0.12345 .property_4711 as variable_1".
"UNWIND 1.0 .property_4711 as variable_1".
"UNWIND 8.0 .property_4711 as variable_1".
"UNWIND 00.0 .property_4711 as variable_1".
"UNWIND 01.0 .property_4711 as variable_1".
"UNWIND 08.0 .property_4711 as variable_1".
"UNWIND 10.0 .property_4711 as variable_1".
"UNWIND 11.0 .property_4711 as variable_1".
"UNWIND 18.0 .property_4711 as variable_1".
"UNWIND 3210.0 .property_4711 as variable_1".
"UNWIND 3210.1 .property_4711 as variable_1".
"UNWIND 3210.12345 .property_4711 as variable_1".
"UNWIND 80.0 .property_4711 as variable_1".
"UNWIND 81.0 .property_4711 as variable_1".
"UNWIND 88.0 .property_4711 as variable_1".
"UNWIND -.0 .property_4711 as variable_1".
"UNWIND -.00 .property_4711 as variable_1".
"UNWIND -.01 .property_4711 as variable_1".
"UNWIND -.08 .property_4711 as variable_1".
"UNWIND -.1 .property_4711 as variable_1".
"UNWIND -.12345 .property_4711 as variable_1".
"UNWIND -.8 .property_4711 as variable_1".
"UNWIND -0.0 .property_4711 as variable_1".
"UNWIND -0.1 .property_4711 as variable_1".
"UNWIND -0.12345 .property_4711 as variable_1".
"UNWIND -1.0 .property_4711 as variable_1".
"UNWIND -8.0 .property_4711 as variable_1".
"UNWIND -00.0 .property_4711 as variable_1".
"UNWIND -01.0 .property_4711 as variable_1".
"UNWIND -08.0 .property_4711 as variable_1".
"UNWIND -10.0 .property_4711 as variable_1".
"UNWIND -11.0 .property_4711 as variable_1".
"UNWIND -18.0 .property_4711 as variable_1".
"UNWIND -3210.0 .property_4711 as variable_1".
"UNWIND -3210.1 .property_4711 as variable_1".
"UNWIND -3210.12345 .property_4711 as variable_1".
"UNWIND -80.0 .property_4711 as variable_1".
"UNWIND -81.0 .property_4711 as variable_1".
"UNWIND -88.0 .property_4711 as variable_1".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Parameter
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"UNWIND {test} .property_4711 as variable_1".
"UNWIND {Test} .property_4711 as variable_1".
"UNWIND {0} .property_4711 as variable_1".
"UNWIND {1} .property_4711 as variable_1".
"UNWIND {10} .property_4711 as variable_1".
"UNWIND {19} .property_4711 as variable_1".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Parenthesized Expression
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"UNWIND (nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null) .property_4711 as variable_1".
"UNWIND ( nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null) .property_4711 as variable_1".
"UNWIND (nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null ) .property_4711 as variable_1".
"UNWIND ( nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null ) .property_4711 as variable_1".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Relationships Pattern
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"UNWIND ( ) <- [ variable_1 ? * 1 ..99 ] -> (variable_1 :node_1 :node_2 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null}) .property_4711 as variable_1".
"UNWIND ( ) <- [ variable_1 ? :rel_1 | :rel_2 * 1 ..99 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null} ] -> (variable_1 :node_1 :node_2 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null}) .property_4711 as variable_1".
"UNWIND ( ) - [ variable_1 ? :rel_1 | :rel_2 * 1 ..99 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null} ] -> ( ) .property_4711 as variable_1".
"UNWIND ( ) <- [ variable_1 ? :rel_1 | :rel_2 * 1 ..99 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null} ] - (variable_1 :node_1 :node_2 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null}) .property_4711 as variable_1".
"UNWIND ( ) - [ variable_1 ? :rel_1 | :rel_2 * 1 ..99 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null} ] - ( ) .property_4711 as variable_1".
"UNWIND ( ) <- -> (variable_1 :node_1 :node_2 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null}) .property_4711 as variable_1".
"UNWIND ( ) - -> ( ) .property_4711 as variable_1".
"UNWIND ( ) <- - (variable_1 :node_1 :node_2 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null}) .property_4711 as variable_1".
"UNWIND ( ) - - ( ) .property_4711 as variable_1".
"UNWIND ( ) <- [ ] -> (variable_1 :node_1 :node_2 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null}) .property_4711 as variable_1".
"UNWIND ( ) <- [ variable_1 ] -> ( ) .property_4711 as variable_1".
"UNWIND ( ) <- [ variable_1 ? ] -> (variable_1 :node_1 :node_2 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null}) .property_4711 as variable_1".
"UNWIND ( ) <- [ variable_1 ? :rel_1 ] -> ( ) .property_4711 as variable_1".
"UNWIND ( ) <- [ variable_1 ? :rel_1 | :rel_2 *..99 ] -> (variable_1 :node_1 :node_2 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null}) .property_4711 as variable_1".
"UNWIND ( ) <- [ variable_1 ? :rel_1 * 1 ..] -> ( ) .property_4711 as variable_1".
"UNWIND ( ) <- [ variable_1 ? :rel_1 | :rel_2 * ..99]-> (variable_1 :node_1 :node_2 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null}) .property_4711 as variable_1".
"UNWIND ( ) <- [ variable_1 ? :rel_1 *..] -> ( ) .property_4711 as variable_1".
"UNWIND ( ) <- [ variable_1 ? :rel_1 * 1 ..99] -> ( ) .property_4711 as variable_1".
"UNWIND ( ) <- [ variable_1 ? :rel_1 | :rel_2 * 1 ..99 { name_1 }] -> (variable_1 :node_1 :node_2 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null}) .property_4711 as variable_1".
"UNWIND ( ) <- [ variable_1 ? :rel_1 * 1 ..99 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null}] -> ( ) .property_4711 as variable_1".
"UNWIND ( ) <- [ ? ] -> (variable_1 :node_1 :node_2 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null}) .property_4711 as variable_1".
"UNWIND ( ) <- [ ? :rel_1 ] -> ( ) .property_4711 as variable_1".
"UNWIND ( ) <- [ ? :rel_1 | :rel_2 *..99 ] -> (variable_1 :node_1 :node_2 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null}) .property_4711 as variable_1".
"UNWIND ( ) <- [ ? :rel_1 * 1 ..] -> ( ) .property_4711 as variable_1".
"UNWIND ( ) <- [ ? :rel_1 | :rel_2 * ..99]-> (variable_1 :node_1 :node_2 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null}) .property_4711 as variable_1".
"UNWIND ( ) <- [ ? :rel_1 *..99 ] -> ( ) .property_4711 as variable_1".
"UNWIND ( ) <- [ ? :rel_1 * 1 ..99] -> ( ) .property_4711 as variable_1".
"UNWIND ( ) <- [ ? :rel_1 * ..] -> ( ) .property_4711 as variable_1".
"UNWIND ( ) <- [ ? * ..] -> ( ) .property_4711 as variable_1".
"UNWIND ( ) <- [ * ..] -> ( ) .property_4711 as variable_1".
"UNWIND ( ) <- [ ? :rel_1 *.. {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null} ] -> ( ) .property_4711 as variable_1".
"UNWIND ( ) <- [ ? :rel_1 * 1 ..99 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null} ] -> ( ) .property_4711 as variable_1".
"UNWIND ( ) <- [ ? :rel_1 * .. {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null} ] -> ( ) .property_4711 as variable_1".
"UNWIND ( ) <- [ ? * .. {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null} ] -> ( ) .property_4711 as variable_1".
"UNWIND ( ) <- [ * .. {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null} ] -> ( ) .property_4711 as variable_1".
"UNWIND ( ) <- [ ? :rel_1 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null} ] -> ( ) .property_4711 as variable_1".
"UNWIND ( ) <- [ ? {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null} ] -> ( ) .property_4711 as variable_1".
"UNWIND ( ) <- [ {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null} ] -> ( ) .property_4711 as variable_1".
"UNWIND ( ) <- [ ? :rel_1 | :rel_2 * 1 ..99 { name_1 }] -> (variable_1 :node_1 :node_2 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null}) .property_4711 as variable_1".
"UNWIND ( ) <- [ ? :rel_1 * 1 ..99 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null}] -> ( ) .property_4711 as variable_1".
"UNWIND ( ) <- [ variable_1 ? {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null} ] -> (variable_1 :node_1 :node_2 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null}) .property_4711 as variable_1".
"UNWIND ( ) <- [ variable_1 ? * 1 ..99 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null} ] -> (variable_1 :node_1 :node_2 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null}) .property_4711 as variable_1".
"UNWIND ( ) <- [ variable_1 ? :rel_1 | :rel_2 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null} ] -> (variable_1 :node_1 :node_2 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null}) .property_4711 as variable_1".
"UNWIND ( ) <- [ ? { name_1 }] -> (variable_1 :node_1 :node_2 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null}) .property_4711 as variable_1".
"UNWIND ( variable_1 : node_1 : node_2 { name_1 } ) <- [ variable_1 ? :rel_1 | :rel_2 * 1 ..99 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null} ] -> (variable_1 :node_1 :node_2 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null}) .property_4711 as variable_1".
"UNWIND ( variable_1 : node_1 : node_2 { name_1 } ) <- [ variable_1 ? :rel_1 | :rel_2 * 1 ..99 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null} ] -> (variable_1 :node_1 :node_2 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null}) <- [ variable_1 ? :rel_1 | :rel_2 * 1 ..99 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null} ] -> (variable_1 :node_1 :node_2 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null}) .property_4711 as variable_1".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% String Literal
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"UNWIND \"Dies ist ein String\" .property_4711 as variable_1".
"UNWIND \"Dies' ist 'ein String\" .property_4711 as variable_1".
"UNWIND 'Dies ist ein String' .property_4711 as variable_1".
"UNWIND 'Dies\" ist \"ein String' .property_4711 as variable_1".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Variable
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"UNWIND Name1 .property_4711 as variable_1".
"UNWIND name2 .property_4711 as variable_1".
"UNWIND NAME3 .property_4711 as variable_1".
