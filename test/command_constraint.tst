%%-*- mode: erlang -*-
%%-*- coding: utf-8 -*-

% Test query options
[{tests, []}].

%% 
%% TESTS
%%

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
"CREATE CONSTRAINT ON (book:Book) ASSERT exists(book.isbn)".
"CREATE CONSTRAINT ON ()-[like:LIKED]-() ASSERT exists(like.day)".

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
"DROP CONSTRAINT ON (book:Book) ASSERT exists(book.isbn)".
"DROP CONSTRAINT ON ()-[like:LIKED]-() ASSERT exists(like.day)".
