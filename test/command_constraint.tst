%%-*- mode: erlang -*-
%%-*- coding: utf-8 -*-

% Test query options
[{tests, []}].

%% 
%% TESTS
%%


% "CREATE CONSTRAINT ON (movie:Movie) ASSERT 4711 IS UNIQUE".
% "CREATE CONSTRAINT ON (movie:Movie) ASSERT movie.title IS UNIQUE".
% "CREATE CONSTRAINT ON ( n : Person ) ASSERT n . name IS UNIQUE;".
% "create constraint on (n:Person) assert n.role is unique;".
% "CREATE CONSTRAINT ON (book:Book) ASSERT exists(book.isbn)".
% "CREATE CONSTRAINT ON ()-[like:LIKED]-() ASSERT exists(like.day)".

% "DROP CONSTRAINT ON (person:Person) ASSERT person.id IS UNIQUE".
% "DROP CONSTRAINT ON ( movie : Movie ) ASSERT movie . id IS UNIQUE".
% "drop constraint on (person:Person) assert person.id is unique".
% "DROP CONSTRAINT ON (book:Book) ASSERT exists(book.isbn)".
% "DROP CONSTRAINT ON ()-[like:LIKED]-() ASSERT exists(like.day)".
