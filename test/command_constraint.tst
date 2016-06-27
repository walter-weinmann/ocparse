%%-*- mode: erlang -*-
%%-*- coding: utf-8 -*-

% Test query options
[{tests, []}].

%% 
%% TESTS
%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Unique Constraint
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"CREATE CONSTRAINT ON ( variable_1 : node_1) ASSERT ( {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null}) IS UNIQUE".
"CREATE CONSTRAINT ON ( variable_1 : node_1) ASSERT sHortestpath ( ( ( : node_1 : node_2 ) ) ) IS UNIQUE".
"CREATE CONSTRAINT ON ( variable_1 : node_1) ASSERT ( {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null}) .property_1 IS UNIQUE".
"CREATE CONSTRAINT ON ( variable_1 : node_1) ASSERT sHortestpath ( ( ( : node_1 : node_2 ) ) ) .property_1 .property2! .property3? IS UNIQUE".
"CREATE CONSTRAINT ON ( variable_1 : node_1) ASSERT ( {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null}) .property_1 IS UNIQUE".
"CREATE CONSTRAINT ON ( variable_1 : node_1) ASSERT sHortestpath ( ( ( : node_1 : node_2 ) ) ) .property_1 .property2! .property3? IS UNIQUE".
"CREATE CONSTRAINT ON (movie:Movie) ASSERT 4711 IS UNIQUE".
"CREATE CONSTRAINT ON (movie:Movie) ASSERT movie.title IS UNIQUE".
"CREATE CONSTRAINT ON ( n : Person ) ASSERT n . name IS UNIQUE;".
"create constraint on (n:Person) assert n.role is unique;".

"DROP CONSTRAINT ON ( variable_1 : node_1) ASSERT ( {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null}) IS UNIQUE".
"DROP CONSTRAINT ON ( variable_1 : node_1) ASSERT sHortestpath ( ( ( : node_1 : node_2 ) ) ) IS UNIQUE".
"DROP CONSTRAINT ON ( variable_1 : node_1) ASSERT ( {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null}) .property_1 IS UNIQUE".
"DROP CONSTRAINT ON ( variable_1 : node_1) ASSERT sHortestpath ( ( ( : node_1 : node_2 ) ) ) .property_1 .property2! .property3? IS UNIQUE".
"DROP CONSTRAINT ON ( variable_1 : node_1) ASSERT ( {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null}) .property_1 IS UNIQUE".
"DROP CONSTRAINT ON ( variable_1 : node_1) ASSERT sHortestpath ( ( ( : node_1 : node_2 ) ) ) .property_1 .property2! .property3? IS UNIQUE".
"DROP CONSTRAINT ON (movie:Movie) ASSERT 4711 IS UNIQUE".
"DROP CONSTRAINT ON (movie:Movie) ASSERT movie.title IS UNIQUE".
"DROP CONSTRAINT ON ( n : Person ) ASSERT n . name IS UNIQUE;".
"drop constraint on (n:Person) assert n.role is unique;".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Node Property Existence Constraint
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"CREATE CONSTRAINT ON ( variable_1 : node_1 ) ASSERT EXISTS ( 'Dies\" ist \"ein String' )".
"CREATE CONSTRAINT ON ( variable_1 : node_1 ) ASSERT EXISTS ( 'Dies\" ist \"ein String' .Property_key_1 )".
"CREATE CONSTRAINT ON ( variable_1 : node_1 ) ASSERT EXISTS ( 'Dies\" ist \"ein String' .Property_key_1? .Property_key_1! )".
"CREATE CONSTRAINT ON (movie:Movie) ASSERT exists ( 4711)".
"CREATE CONSTRAINT ON (movie:Movie) ASSERT exists ( movie.title)".
"CREATE CONSTRAINT ON ( n : Person ) ASSERT exists ( n . name);".
"create constraint on (n:Person) assert EXISTS (n.role);".
"CREATE CONSTRAINT ON (book:Book) ASSERT exists(book.isbn)".

"DROP CONSTRAINT ON ( variable_1 : node_1 ) ASSERT EXISTS ( 'Dies\" ist \"ein String' )".
"DROP CONSTRAINT ON ( variable_1 : node_1 ) ASSERT EXISTS ( 'Dies\" ist \"ein String' .Property_key_1 )".
"DROP CONSTRAINT ON ( variable_1 : node_1 ) ASSERT EXISTS ( 'Dies\" ist \"ein String' .Property_key_1? .Property_key_1! )".
"DROP CONSTRAINT ON (movie:Movie) ASSERT exists ( 4711)".
"DROP CONSTRAINT ON (movie:Movie) ASSERT exists ( movie.title)".
"DROP CONSTRAINT ON ( n : Person ) ASSERT exists ( n . name);".
"drop constraint on (n:Person) ASSERT EXISTS (n.role);".
"DROP CONSTRAINT ON (book:Book) ASSERT exists(book.isbn)".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Relationship Property Existence Constraint
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"CREATE CONSTRAINT ON () <- [ variable_1 : relation_1 ] -> () ASSERT exists ( 'Dies\" ist \"ein String' .Property_key_1? .Property_key_1! )".
"CREATE CONSTRAINT ON () - [ variable_1 : relation_1 ] - () ASSERT exists ( 'Dies\" ist \"ein String' .Property_key_1? .Property_key_1! )".
"CREATE CONSTRAINT ON () <- [ variable_1 : relation_1 ] - () ASSERT exists ( 'Dies\" ist \"ein String' .Property_key_1 )".
"CREATE CONSTRAINT ON () - [ variable_1 : relation_1 ] -> () ASSERT exists ( 'Dies\" ist \"ein String' )".
"CREATE CONSTRAINT ON () - [ variable_1 : relation_1 ] - () ASSERT exists ( 'Dies\" ist \"ein String' .Property_key_1 )".
"CREATE CONSTRAINT ON () - [ variable_1 : relation_1 ] - () ASSERT exists ( 4711)".
"CREATE CONSTRAINT ON () <- [ variable_1 : relation_1 ] - () ASSERT exists ( movie.title)".
"CREATE CONSTRAINT ON () - [ variable_1 : relation_1 ] -> () ASSERT exists ( n . name);".
"create constraint on () - [ variable_1 : relation_1 ] - () assert EXISTS (n.role);".
"CREATE CONSTRAINT ON () <- [ variable_1 : relation_1 ] - () ASSERT exists(book.isbn)".
"CREATE CONSTRAINT ON () - [ variable_1 : relation_1 ] -> () ASSERT exists(like.day)".

"DROP CONSTRAINT ON () <- [ variable_1 : relation_1 ] -> () ASSERT exists ( 'Dies\" ist \"ein String' .Property_key_1? .Property_key_1! )".
"DROP CONSTRAINT ON () - [ variable_1 : relation_1 ] - () ASSERT exists ( 'Dies\" ist \"ein String' .Property_key_1? .Property_key_1! )".
"DROP CONSTRAINT ON () <- [ variable_1 : relation_1 ] - () ASSERT exists ( 'Dies\" ist \"ein String' .Property_key_1 )".
"DROP CONSTRAINT ON () - [ variable_1 : relation_1 ] -> () ASSERT exists ( 'Dies\" ist \"ein String' )".
"DROP CONSTRAINT ON () - [ variable_1 : relation_1 ] - () ASSERT exists ( 'Dies\" ist \"ein String' .Property_key_1 )".
"DROP CONSTRAINT ON () - [ variable_1 : relation_1 ] - () ASSERT exists ( 4711)".
"DROP CONSTRAINT ON () <- [ variable_1 : relation_1 ] - () ASSERT exists ( movie.title)".
"DROP CONSTRAINT ON () - [ variable_1 : relation_1 ] -> () ASSERT exists ( n . name);".
"drop constraint on () - [ variable_1 : relation_1 ] - () assert EXISTS (n.role);".
"DROP CONSTRAINT ON () <- [ variable_1 : relation_1 ] - () ASSERT exists(book.isbn)".
"DROP CONSTRAINT ON () - [ variable_1 : relation_1 ] -> () ASSERT exists(like.day)".
