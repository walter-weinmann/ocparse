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

"CREATE CONSTRAINT ON (book:Book) ASSERT fIlter ( variable_1 iN nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null wHere nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null ) is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT eXtract ( variable_1 iN nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null wHere nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null | nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null ) is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT eXtract ( variable_1 iN nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null wHere nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null ) is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT aLl ( variable_1 iN nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null wHere nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null ) is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT aNy ( variable_1 iN nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null wHere nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null ) is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT nOne ( variable_1 iN nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null wHere nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null ) is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT sIngle ( variable_1 iN nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null wHere nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null ) is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT fIlter(variable_1 iN nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null wHere nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null) is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT eXtract(variable_1 iN nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null wHere nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null|nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null) is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT eXtract(variable_1 iN nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null wHere nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null) is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT aLl(variable_1 iN nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null wHere nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null) is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT aNy(variable_1 iN nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null wHere nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null) is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT nOne(variable_1 iN nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null wHere nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null) is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT sIngle(variable_1 iN nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null wHere nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null) is unique".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Constant
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"CREATE CONSTRAINT ON (book:Book) ASSERT TRUE is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT FALSE is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT NULL is unique".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Case Expression
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"CREATE CONSTRAINT ON (book:Book) ASSERT cAse wHen nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null tHen nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null eNd is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT cAse wHen nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null tHen nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null eLse nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null eNd is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT cAse wHen nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null tHen nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null wHen nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null tHen nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null wHen nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null tHen nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null eNd is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT cAse wHen nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null tHen nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null wHen nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null tHen nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null wHen nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null tHen nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null eLse nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null eNd is unique".

"CREATE CONSTRAINT ON (book:Book) ASSERT cAse nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null wHen nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null tHen nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null eNd is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT cAse nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null wHen nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null tHen nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null eLse nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null eNd is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT cAse nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null wHen nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null tHen nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null wHen nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null tHen nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null wHen nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null tHen nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null eNd is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT cAse nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null wHen nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null tHen nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null wHen nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null tHen nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null wHen nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null tHen nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null eLse nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null eNd is unique".


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% COUNT(*)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"CREATE CONSTRAINT ON (book:Book) ASSERT count(*) is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT count ( * ) is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT COUNT(*) is unique".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Expression List
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"CREATE CONSTRAINT ON (book:Book) ASSERT [ nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null ] is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT [nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null] is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT [ nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null , nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null ] is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT [nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null,nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null] is unique".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function Invocation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"CREATE CONSTRAINT ON (book:Book) ASSERT function_1 ( ) is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT function_1 ( dIstinct ) is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT function_1 ( nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null ) is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT function_1 ( dIstinct nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null ) is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT function_1 ( nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null , nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null ) is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT function_1 ( dIstinct nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null , nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null ) is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT function_1() is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT function_1(dIstinct) is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT function_1(nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null) is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT function_1(dIstinct nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null) is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT function_1(nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null , nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null) is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT function_1(dIstinct nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null , nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null) is unique".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% List Comprehension
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"CREATE CONSTRAINT ON (book:Book) ASSERT [ variable_1 iN nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null ] is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT [ variable_1 iN nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null wHere nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null ] is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT [ variable_1 iN nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null | nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null ] is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT [ variable_1 iN nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null wHere nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null | nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null ] is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT [variable_1 iN nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null] is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT [variable_1 iN nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null wHere nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null] is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT [variable_1 iN nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null|nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null] is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT [variable_1 iN nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null wHere nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null|nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null] is unique".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Map Literal
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"CREATE CONSTRAINT ON (book:Book) ASSERT { } is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT { property_1 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null } is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT { property_1 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null , property_2 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null } is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT {} is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null} is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null,property_2:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null} is unique".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Number Literal
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Decimal Integer
"CREATE CONSTRAINT ON (book:Book) ASSERT 1 is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT 4711 is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT 8 is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT -1 is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT -4711 is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT -8 is unique".

%% Hexadecimal Integer
"CREATE CONSTRAINT ON (book:Book) ASSERT 0X0 is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT 0XA is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT 0X0123456789ABCDEF is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT -0X0 is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT -0XA is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT -0X0123456789ABCDEF is unique".

%% Octal Integer
"CREATE CONSTRAINT ON (book:Book) ASSERT 00 is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT 001234567 is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT -00 is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT -001234567 is unique".

%% Exponential Decimal Real
"CREATE CONSTRAINT ON (book:Book) ASSERT 0E0 is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT 0E789 is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT 9E0 is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT 9E789 is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT 0E-0 is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT 0E-789 is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT 9E-0 is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT 9E-789 is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT -0E0 is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT -0E789 is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT -9E0 is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT -9E789 is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT -0E-0 is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT -0E-789 is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT -9E-0 is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT -9E-789 is unique".

%% Regular Decimal Real
"CREATE CONSTRAINT ON (book:Book) ASSERT .0 is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT .00 is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT .01 is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT .08 is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT .1 is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT .12345 is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT .8 is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT 0.0 is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT 0.1 is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT 0.12345 is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT 1.0 is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT 8.0 is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT 00.0 is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT 01.0 is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT 08.0 is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT 10.0 is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT 11.0 is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT 18.0 is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT 3210.0 is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT 3210.1 is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT 3210.12345 is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT 80.0 is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT 81.0 is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT 88.0 is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT -.0 is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT -.00 is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT -.01 is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT -.08 is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT -.1 is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT -.12345 is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT -.8 is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT -0.0 is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT -0.1 is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT -0.12345 is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT -1.0 is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT -8.0 is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT -00.0 is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT -01.0 is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT -08.0 is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT -10.0 is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT -11.0 is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT -18.0 is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT -3210.0 is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT -3210.1 is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT -3210.12345 is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT -80.0 is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT -81.0 is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT -88.0 is unique".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Parameter
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"CREATE CONSTRAINT ON (book:Book) ASSERT {test} is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT {Test} is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT {0} is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT {1} is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT {10} is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT {19} is unique".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Parenthesized Expression
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"CREATE CONSTRAINT ON (book:Book) ASSERT (nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null) is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT ( nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null) is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT (nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null ) is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT ( nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null ) is unique".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Reduce
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"CREATE CONSTRAINT ON (book:Book) ASSERT rEduce ( variable_1 = nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null , variable_2 iN nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null | nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null ) is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT rEduce(variable_1=nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null,variable_2 iN nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null|nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null) is unique".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Relationships Pattern
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"CREATE CONSTRAINT ON (book:Book) ASSERT ( ) <-- [ variable_1 ? * 1 ..99 ] --> (variable_1 :node_1 :node_2 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null}) is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT (variable_1 :node_1 :node_2 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null}) is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT ( variable_1 : node_1 : node_2 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null} ) is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT ( variable_1 : node_1 : node_2 { name_1 } ) is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT ( variable_1 : node_1 : node_2 { 4711 } ) is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT ( variable_1 : node_1 { name_1 } ) is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT ( variable_1 : node_1 { 4711 } ) is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT ( variable_1 : node_1 : node_2 ) is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT ( variable_1 : node_1 ) is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT ( variable_1 { name_1 } ) is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT ( variable_1 { 4711 } ) is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT ( variable_1 ) is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT ( : node_1 : node_2 { name_1 } ) is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT ( : node_1 : node_2 { 4711 } ) is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT ( : node_1 { name_1 } ) is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT ( : node_1 { 4711 } ) is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT ( {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null}) is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT ( { name_1 } ) is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT ( { 4711 } ) is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT ( : node_1 : node_2 ) is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT ( : node_1 ) is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT (  ) is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT ( ) <-- [ variable_1 ? :rel_1 | :rel_2 * 1 ..99 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null} ] --> (variable_1 :node_1 :node_2 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null}) is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT ( ) -- [ variable_1 ? :rel_1 | :rel_2 * 1 ..99 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null} ] --> ( ) is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT ( ) <-- [ variable_1 ? :rel_1 | :rel_2 * 1 ..99 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null} ] -- (variable_1 :node_1 :node_2 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null}) is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT ( ) -- [ variable_1 ? :rel_1 | :rel_2 * 1 ..99 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null} ] -- ( ) is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT ( ) <-- --> (variable_1 :node_1 :node_2 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null}) is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT ( ) -- --> ( ) is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT ( ) <-- -- (variable_1 :node_1 :node_2 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null}) is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT ( ) -- -- ( ) is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT ( ) <-- [ ] --> (variable_1 :node_1 :node_2 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null}) is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT ( ) <-- [ variable_1 ] --> ( ) is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT ( ) <-- [ variable_1 ? ] --> (variable_1 :node_1 :node_2 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null}) is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT ( ) <-- [ variable_1 ? :rel_1 ] --> ( ) is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT ( ) <-- [ variable_1 ? :rel_1 | :rel_2 * ] --> (variable_1 :node_1 :node_2 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null}) is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT ( ) <-- [ variable_1 ? :rel_1 * 1 ..] --> ( ) is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT ( ) <-- [ variable_1 ? :rel_1 | :rel_2 * ..99]--> (variable_1 :node_1 :node_2 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null}) is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT ( ) <-- [ variable_1 ? :rel_1 *] --> ( ) is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT ( ) <-- [ variable_1 ? :rel_1 * 1 ..99] --> ( ) is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT ( ) <-- [ variable_1 ? :rel_1 | :rel_2 * 1 ..99 { name_1 }] --> (variable_1 :node_1 :node_2 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null}) is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT ( ) <-- [ variable_1 ? :rel_1 * 1 ..99 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null}] --> ( ) is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT ( ) <-- [ ? ] --> (variable_1 :node_1 :node_2 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null}) is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT ( ) <-- [ ? :rel_1 ] --> ( ) is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT ( ) <-- [ ? :rel_1 | :rel_2 * ] --> (variable_1 :node_1 :node_2 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null}) is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT ( ) <-- [ ? :rel_1 * 1 ..] --> ( ) is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT ( ) <-- [ ? :rel_1 | :rel_2 * ..99]--> (variable_1 :node_1 :node_2 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null}) is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT ( ) <-- [ ? :rel_1 * ] --> ( ) is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT ( ) <-- [ ? :rel_1 * 1 ..99] --> ( ) is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT ( ) <-- [ ? :rel_1 * ..] --> ( ) is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT ( ) <-- [ ? * ..] --> ( ) is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT ( ) <-- [ * ..] --> ( ) is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT ( ) <-- [ ? :rel_1 * {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null} ] --> ( ) is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT ( ) <-- [ ? :rel_1 * 1 ..99 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null} ] --> ( ) is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT ( ) <-- [ ? :rel_1 * .. {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null} ] --> ( ) is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT ( ) <-- [ ? * .. {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null} ] --> ( ) is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT ( ) <-- [ * .. {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null} ] --> ( ) is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT ( ) <-- [ ? :rel_1 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null} ] --> ( ) is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT ( ) <-- [ ? {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null} ] --> ( ) is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT ( ) <-- [ {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null} ] --> ( ) is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT ( ) <-- [ ? :rel_1 | :rel_2 * 1 ..99 { name_1 }] --> (variable_1 :node_1 :node_2 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null}) is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT ( ) <-- [ ? :rel_1 * 1 ..99 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null}] --> ( ) is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT ( ) <-- [ variable_1 ? {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null} ] --> (variable_1 :node_1 :node_2 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null}) is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT ( ) <-- [ variable_1 ? * 1 ..99 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null} ] --> (variable_1 :node_1 :node_2 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null}) is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT ( ) <-- [ variable_1 ? :rel_1 | :rel_2 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null} ] --> (variable_1 :node_1 :node_2 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null}) is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT ( ) <-- [ ? { name_1 }] --> (variable_1 :node_1 :node_2 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null}) is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT ( variable_1 : node_1 : node_2 { name_1 } ) <-- [ variable_1 ? :rel_1 | :rel_2 * 1 ..99 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null} ] --> (variable_1 :node_1 :node_2 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null}) is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT ( variable_1 : node_1 : node_2 { name_1 } ) <-- [ variable_1 ? :rel_1 | :rel_2 * 1 ..99 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null} ] --> (variable_1 :node_1 :node_2 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null}) <-- [ variable_1 ? :rel_1 | :rel_2 * 1 ..99 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null} ] --> (variable_1 :node_1 :node_2 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null}) is unique".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Shortest Path Pattern
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"CREATE CONSTRAINT ON (book:Book) ASSERT sHortestpath ( (variable_1 :node_1 :node_2 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null}) ) is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT sHortestpath ( ( variable_1 : node_1 : node_2 { name_1 } ) ) is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT sHortestpath ( ( variable_1 : node_1 { name_1 } ) ) is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT sHortestpath ( ( variable_1 : node_1 : node_2 ) <-- [ variable_1 ? :rel_1 * 1 ..99 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null}] --> ( ) ) is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT sHortestpath ( ( variable_1 { name_1 } ) <-- [ variable_1 ? :rel_1 * 1 ..99 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null}] --> ( ) ) is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT sHortestpath ( ( variable_1 ) <-- [ variable_1 ? :rel_1 * 1 ..99 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null}] --> ( ) ) is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT sHortestpath ( ( ( : node_1 : node_2 { name_1 } ) ) ) is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT sHortestpath ( ( (  ) ) ) is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT sHortestpath ( ( ( : node_1 : node_2 ) ) ) is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT sHortestpath ( ( ( { 4711 } ) <--  --> ( ) ) ) is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT sHortestpath ( ( ( { name_1 } ) <-- [ variable_1 ? :rel_1 * 1 ..99 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null}] --> ( ) ) ) is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT sHortestpath ( ( ( { name_1 } ) <-- [ variable_1 ? :rel_1 * 1 ..99 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null}] --> ( ) ) ) is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT aLlsHortestpaths ( ( : node_1 { 4711 } ) ) is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT aLlsHortestpaths ( ( { name_1 } ) ) is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT aLlsHortestpaths ( ( { name_1 } ) ) is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT aLlsHortestpaths ( ( { 4711 } ) <-- [ variable_1 ? :rel_1 * 1 ..99 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null}] --> ( ) ) is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT aLlsHortestpaths ( ( : node_1 ) <-- [ variable_1 ? :rel_1 * 1 ..99 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null}] --> ( ) ) is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT aLlsHortestpaths ( ( : node_1 { name_1 } ) <-- [ variable_1 ? :rel_1 * 1 ..99 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null}] --> ( ) ) is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT aLlsHortestpaths ( ( ( : node_1 : node_2 { 4711 } ) ) ) is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT aLlsHortestpaths ( ( ( variable_1 { 4711 } ) ) ) is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT aLlsHortestpaths ( ( ( variable_1 : node_1 ) ) ) is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT aLlsHortestpaths ( ( ( variable_1 : node_1 { 4711 } ) <-- [ variable_1 ? :rel_1 * 1 ..99 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null}] --> ( ) ) ) is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT aLlsHortestpaths ( ( ( variable_1 : node_1 : node_2 { 4711 } ) <-- [ variable_1 ? :rel_1 * 1 ..99 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null}] --> ( ) ) ) is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT aLlsHortestpaths ( ( ( variable_1 : node_1 : node_2 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null} ) <-- [ variable_1 ? :rel_1 * 1 ..99 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null}] --> ( ) ) ) is unique".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% String Literal
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"CREATE CONSTRAINT ON (book:Book) ASSERT \"Dies ist ein String\" is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT \"Dies' ist 'ein String\" is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT 'Dies ist ein String' is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT 'Dies\" ist \"ein String' is unique".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Variable
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"CREATE CONSTRAINT ON (book:Book) ASSERT Name1 is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT name2 is unique".
"CREATE CONSTRAINT ON (book:Book) ASSERT NAME3 is unique".
