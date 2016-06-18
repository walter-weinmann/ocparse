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

"fIlter ( variable_1 iN nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null wHere nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null )".
"eXtract ( variable_1 iN nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null wHere nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null | nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null )".
"eXtract ( variable_1 iN nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null wHere nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null )".
"aLl ( variable_1 iN nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null wHere nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null )".
"aNy ( variable_1 iN nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null wHere nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null )".
"nOne ( variable_1 iN nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null wHere nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null )".
"sIngle ( variable_1 iN nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null wHere nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null )".
"fIlter(variable_1 iN nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null wHere nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null)".
"eXtract(variable_1 iN nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null wHere nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null|nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null)".
"eXtract(variable_1 iN nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null wHere nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null)".
"aLl(variable_1 iN nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null wHere nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null)".
"aNy(variable_1 iN nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null wHere nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null)".
"nOne(variable_1 iN nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null wHere nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null)".
"sIngle(variable_1 iN nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null wHere nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null)".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Constant
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"TRUE".
"FALSE".
"NULL".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Case Expression
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"cAse wHen nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null tHen nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null eNd".
"cAse wHen nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null tHen nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null eLse nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null eNd".
"cAse wHen nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null tHen nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null wHen nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null tHen nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null wHen nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null tHen nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null eNd".
"cAse wHen nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null tHen nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null wHen nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null tHen nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null wHen nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null tHen nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null eLse nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null eNd".

"cAse nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null wHen nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null tHen nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null eNd".
"cAse nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null wHen nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null tHen nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null eLse nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null eNd".
"cAse nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null wHen nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null tHen nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null wHen nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null tHen nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null wHen nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null tHen nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null eNd".
"cAse nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null wHen nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null tHen nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null wHen nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null tHen nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null wHen nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null tHen nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null eLse nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null eNd".


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% COUNT(*)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"count(*)".
"count ( * )".
"COUNT(*)".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Expression List
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"[ nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null ]".
"[nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null]".
"[ nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null , nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null ]".
"[nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null,nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null]".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function Invocation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"function_1 ( )".
"function_1 ( dIstinct )".
"function_1 ( nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null )".
"function_1 ( dIstinct nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null )".
"function_1 ( nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null , nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null )".
"function_1 ( dIstinct nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null , nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null )".
"function_1()".
"function_1(dIstinct)".
"function_1(nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null)".
"function_1(dIstinct nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null)".
"function_1(nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null , nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null)".
"function_1(dIstinct nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null , nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null)".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% List Comprehension
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"[ variable_1 iN nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null ]".
"[ variable_1 iN nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null wHere nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null ]".
"[ variable_1 iN nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null | nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null ]".
"[ variable_1 iN nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null wHere nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null | nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null ]".
"[variable_1 iN nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null]".
"[variable_1 iN nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null wHere nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null]".
"[variable_1 iN nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null|nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null]".
"[variable_1 iN nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null wHere nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null|nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null]".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Map Literal
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"{ }".
"{ property_1 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null }".
"{ property_1 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null , property_2 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null }".
"{}".
"{property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null}".
"{property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null,property_2:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null}".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Number Literal
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Decimal Integer
"1".
"4711".
"8".
"-1".
"-4711".
"-8".

%% Hexadecimal Integer
"0X0".
"0XA".
"0X0123456789ABCDEF".
"-0X0".
"-0XA".
"-0X0123456789ABCDEF".

%% Octal Integer
"00".
"001234567".
"-00".
"-001234567".

%% Exponential Decimal Real
"0E0".
"0E789".
"9E0".
"9E789".
"0E-0".
"0E-789".
"9E-0".
"9E-789".
"-0E0".
"-0E789".
"-9E0".
"-9E789".
"-0E-0".
"-0E-789".
"-9E-0".
"-9E-789".

%% Regular Decimal Real
".0".
".00".
".01".
".08".
".1".
".12345".
".8".
"0.0".
"0.1".
"0.12345".
"1.0".
"8.0".
"00.0".
"01.0".
"08.0".
"10.0".
"11.0".
"18.0".
"3210.0".
"3210.1".
"3210.12345".
"80.0".
"81.0".
"88.0".
"-.0".
"-.00".
"-.01".
"-.08".
"-.1".
"-.12345".
"-.8".
"-0.0".
"-0.1".
"-0.12345".
"-1.0".
"-8.0".
"-00.0".
"-01.0".
"-08.0".
"-10.0".
"-11.0".
"-18.0".
"-3210.0".
"-3210.1".
"-3210.12345".
"-80.0".
"-81.0".
"-88.0".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Parameter
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"{test}".
"{Test}".
"{0}".
"{1}".
"{10}".
"{19}".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Parenthesized Expression
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"(nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null)".
"( nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null)".
"(nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null )".
"( nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null )".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Reduce
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"rEduce ( variable_1 = nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null , variable_2 iN nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null | nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null )".
"rEduce(variable_1=nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null,variable_2 iN nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null|nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null)".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Relationships Pattern
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"(variable_1 :node_1 :node_2 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null})".
"( variable_1 : node_1 : node_2 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null} )".
"( variable_1 : node_1 : node_2 { name_1 } )".
"( variable_1 : node_1 : node_2 { 4711 } )".
"( variable_1 : node_1 { name_1 } )".
"( variable_1 : node_1 { 4711 } )".
"( variable_1 : node_1 : node_2 )".
"( variable_1 : node_1 )".
"( variable_1 { name_1 } )".
"( variable_1 { 4711 } )".
"( variable_1 )".
"( : node_1 : node_2 { name_1 } )".
"( : node_1 : node_2 { 4711 } )".
"( : node_1 { name_1 } )".
"( : node_1 { 4711 } )".
"( {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null})".
"( { name_1 } )".
"( { 4711 } )".
"( : node_1 : node_2 )".
"( : node_1 )".
"(  )".
"( ) <-- [ variable_1 ? :rel_1 | :rel_2 * 1 ..99 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null} ] --> (variable_1 :node_1 :node_2 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null})".
"( ) -- [ variable_1 ? :rel_1 | :rel_2 * 1 ..99 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null} ] --> ( )".
"( ) <-- [ variable_1 ? :rel_1 | :rel_2 * 1 ..99 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null} ] -- (variable_1 :node_1 :node_2 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null})".
"( ) -- [ variable_1 ? :rel_1 | :rel_2 * 1 ..99 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null} ] -- ( )".
"( ) <-- --> (variable_1 :node_1 :node_2 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null})".
"( ) -- --> ( )".
"( ) <-- -- (variable_1 :node_1 :node_2 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null})".
"( ) -- -- ( )".
"( ) <-- [ ] --> (variable_1 :node_1 :node_2 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null})".
"( ) <-- [ variable_1 ] --> ( )".
"( ) <-- [ variable_1 ? ] --> (variable_1 :node_1 :node_2 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null})".
"( ) <-- [ variable_1 ? :rel_1 ] --> ( )".
"( ) <-- [ variable_1 ? :rel_1 | :rel_2 * ] --> (variable_1 :node_1 :node_2 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null})".
"( ) <-- [ variable_1 ? :rel_1 * 1 ..] --> ( )".
"( ) <-- [ variable_1 ? :rel_1 | :rel_2 * ..99]--> (variable_1 :node_1 :node_2 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null})".
"( ) <-- [ variable_1 ? :rel_1 *] --> ( )".
"( ) <-- [ variable_1 ? :rel_1 * 1 ..99] --> ( )".
"( ) <-- [ variable_1 ? :rel_1 | :rel_2 * 1 ..99 { name_1 }] --> (variable_1 :node_1 :node_2 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null})".
"( ) <-- [ variable_1 ? :rel_1 * 1 ..99 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null}] --> ( )".
"( ) <-- [ ? ] --> (variable_1 :node_1 :node_2 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null})".
"( ) <-- [ ? :rel_1 ] --> ( )".
"( ) <-- [ ? :rel_1 | :rel_2 * ] --> (variable_1 :node_1 :node_2 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null})".
"( ) <-- [ ? :rel_1 * 1 ..] --> ( )".
"( ) <-- [ ? :rel_1 | :rel_2 * ..99]--> (variable_1 :node_1 :node_2 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null})".
"( ) <-- [ ? :rel_1 * ] --> ( )".
"( ) <-- [ ? :rel_1 * 1 ..99] --> ( )".
"( ) <-- [ ? :rel_1 * ..] --> ( )".
"( ) <-- [ ? * ..] --> ( )".
"( ) <-- [ * ..] --> ( )".
"( ) <-- [ ? :rel_1 * {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null} ] --> ( )".
"( ) <-- [ ? :rel_1 * 1 ..99 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null} ] --> ( )".
"( ) <-- [ ? :rel_1 * .. {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null} ] --> ( )".
"( ) <-- [ ? * .. {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null} ] --> ( )".
"( ) <-- [ * .. {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null} ] --> ( )".
"( ) <-- [ ? :rel_1 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null} ] --> ( )".
"( ) <-- [ ? {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null} ] --> ( )".
"( ) <-- [ {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null} ] --> ( )".
"( ) <-- [ ? :rel_1 | :rel_2 * 1 ..99 { name_1 }] --> (variable_1 :node_1 :node_2 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null})".
"( ) <-- [ ? :rel_1 * 1 ..99 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null}] --> ( )".
"( ) <-- [ variable_1 ? {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null} ] --> (variable_1 :node_1 :node_2 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null})".
"( ) <-- [ variable_1 ? * 1 ..99 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null} ] --> (variable_1 :node_1 :node_2 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null})".
"( ) <-- [ variable_1 ? :rel_1 | :rel_2 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null} ] --> (variable_1 :node_1 :node_2 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null})".
"( ) <-- [ ? { name_1 }] --> (variable_1 :node_1 :node_2 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null})".
"( variable_1 : node_1 : node_2 { name_1 } ) <-- [ variable_1 ? :rel_1 | :rel_2 * 1 ..99 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null} ] --> (variable_1 :node_1 :node_2 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null})".
"( variable_1 : node_1 : node_2 { name_1 } ) <-- [ variable_1 ? :rel_1 | :rel_2 * 1 ..99 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null} ] --> (variable_1 :node_1 :node_2 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null}) <-- [ variable_1 ? :rel_1 | :rel_2 * 1 ..99 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null} ] --> (variable_1 :node_1 :node_2 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null})".


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% String Literal
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"\"Dies ist ein String\"".
"\"Dies' ist 'ein String\"".
"'Dies ist ein String'".
"'Dies\" ist \"ein String'".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Variable
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"Name1".
"name2".
"NAME3".
