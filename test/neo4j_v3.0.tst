%% -----------------------------------------------------------------------------
%%
%% neo4j_v3.0.tst: opencypher - eunit tests from neo4j v3.0 documentation.
%%
%% Copyright (c) 2017 Walter Weinmann.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -----------------------------------------------------------------------------

%%-*- mode: erlang -*-
%%-*- coding: utf-8 -*-

% Test query options
[{tests, []}].

%%
%% TESTS
%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 4.2 Patterns in Practice
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"
CREATE (:Movie { title:\"The Matrix\",released:1997 })
".
"
CREATE (p:Person { name:\"Keanu Reeves\", born:1964 })
RETURN p
".
"
CREATE (a:Person { name:\"Tom Hanks\",
  born:1956 })-[r:ACTED_IN { roles: [\"Forrest\"]}]->(m:Movie { title:\"Forrest Gump\",released:1994 })
CREATE (d:Person { name:\"Robert Zemeckis\", born:1951 })-[:DIRECTED]->(m)
RETURN a,d,r,m
".
"
MATCH (m:Movie)
RETURN m
".
"
MATCH (p:Person { name:\"Keanu Reeves\" })
RETURN p
".
"
MATCH (p:Person { name:\"Tom Hanks\" })-[r:ACTED_IN]->(m:Movie)
RETURN m.title, r.roles
".
"
MATCH (p:Person { name:\"Tom Hanks\" })
CREATE (m:Movie { title:\"Cloud Atlas\",released:2012 })
CREATE (p)-[r:ACTED_IN { roles: ['Zachry']}]->(m)
RETURN p,r,m
".
"
MERGE (m:Movie { title:\"Cloud Atlas\" })
ON CREATE SET m.released = 2012
RETURN m
".
"
MATCH (m:Movie { title:\"Cloud Atlas\" })
MATCH (p:Person { name:\"Tom Hanks\" })
MERGE (p)-[r:ACTED_IN]->(m)
ON CREATE SET r.roles =['Zachry']
RETURN p,r,m
".
"
CREATE (y:Year { year:2014 })
MERGE (y)<-[:IN_YEAR]-(m10:Month { month:10 })
MERGE (y)<-[:IN_YEAR]-(m11:Month { month:11 })
RETURN y,m10,m11
".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 4.3 Getting correct results
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"
CREATE (matrix:Movie { title:\"The Matrix\",released:1997 })
CREATE (cloudAtlas:Movie { title:\"Cloud Atlas\",released:2012 })
CREATE (forrestGump:Movie { title:\"Forrest Gump\",released:1994 })
CREATE (keanu:Person { name:\"Keanu Reeves\", born:1964 })
CREATE (robert:Person { name:\"Robert Zemeckis\", born:1951 })
CREATE (tom:Person { name:\"Tom Hanks\", born:1956 })
CREATE (tom)-[:ACTED_IN { roles: [\"Forrest\"]}]->(forrestGump)
CREATE (tom)-[:ACTED_IN { roles: ['Zachry']}]->(cloudAtlas)
CREATE (robert)-[:DIRECTED]->(forrestGump)
".
"
MATCH (m:Movie)
WHERE m.title = \"The Matrix\"
RETURN m
".
"
MATCH (m:Movie { title: \"The Matrix\" })
RETURN m
".
"
MATCH (p:Person)-[r:ACTED_IN]->(m:Movie)
WHERE p.name =~ \"K.+\" OR m.released > 2000 OR \"Neo\" IN r.roles
RETURN p,r,m
".
"
MATCH (p:Person)-[:ACTED_IN]->(m)
WHERE NOT (p)-[:DIRECTED]->()
RETURN p,m
".
"
MATCH (p:Person)
RETURN p, p.name AS name, upper(p.name), coalesce(p.nickname,\"n/a\") AS nickname, { name: p.name,
  label:head(labels(p))} AS person
".
"
MATCH (:Person)
RETURN count(*) AS people
".
"
MATCH (actor:Person)-[:ACTED_IN]->(movie:Movie)<-[:DIRECTED]-(director:Person)
RETURN actor,director,count(*) AS collaborations
".
"
MATCH (a:Person)-[:ACTED_IN]->(m:Movie)
RETURN a,count(*) AS appearances
ORDER BY appearances DESC LIMIT 10;
".
"
MATCH (m:Movie)<-[:ACTED_IN]-(a:Person)
RETURN m.title AS movie, collect(a.name) AS cast, count(*) AS actors
".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 4.4 Composing large statements
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"
MATCH (actor:Person)-[r:ACTED_IN]->(movie:Movie)
RETURN actor.name AS name, type(r) AS acted_in, movie.title AS title
UNION
MATCH (director:Person)-[r:DIRECTED]->(movie:Movie)
RETURN director.name AS name, type(r) AS acted_in, movie.title AS title
".
"
MATCH (person:Person)-[:ACTED_IN]->(m:Movie)
WITH person, count(*) AS appearances, collect(m.title) AS movies
WHERE appearances > 1
RETURN person.name, appearances, movies
".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 4.5 Constraints and indexes
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"
CREATE (actor:Actor { name:\"Tom Hanks\" }),(movie:Movie { title:'Sleepless IN Seattle' }),
(actor)-[:ACTED_IN]->(movie);
".
"
MATCH (actor:Actor { name: \"Tom Hanks\" })
RETURN actor;
".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 5.1 What is Cypher?
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"
MATCH (john {name: 'John'})-[:friend]->()-[:friend]->(fof)
RETURN john.name, fof.name
".
"
MATCH (user)-[:friend]->(follower)
WHERE user.name IN ['Joe', 'John', 'Sara', 'Maria', 'Steve'] AND follower.name =~ 'S.*'
RETURN user.name, follower.name
".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 5.2 Updating the graph
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"
MATCH (n {name: 'John'})-[:FRIEND]-(friend)
WITH n, count(friend) as friendsCount
WHERE friendsCount > 3
RETURN n, friendsCount
".
"
MATCH (n {name: 'John'})-[:FRIEND]-(friend)
WITH n, count(friend) as friendsCount
SET n.friendCount = friendsCount
RETURN n.friendsCount
".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 5.4 Uniqueness
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"
CREATE (adam:User { name: 'Adam' }),(pernilla:User { name: 'Pernilla' }),(david:User { name: 'David' }),
(adam)-[:FRIEND]->(pernilla),(pernilla)-[:FRIEND]->(david)
".
"
MATCH (user:User { name: 'Adam' })-[r1:FRIEND]-()-[r2:FRIEND]-(friend_of_a_friend)
RETURN friend_of_a_friend.name AS fofName
".
"
MATCH (user:User { name: 'Adam' })-[r1:FRIEND]-(friend)
MATCH (friend)-[r2:FRIEND]-(friend_of_a_friend)
RETURN friend_of_a_friend.name AS fofName
".
"
MATCH (user:User { name: 'Adam' })-[r1:FRIEND]-(friend),(friend)-[r2:FRIEND]-(friend_of_a_friend)
RETURN friend_of_a_friend.name AS fofName
".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 5.5 Parameters
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% wwe ??? parameter
"
MATCH (n)
WHERE n.name = $name
RETURN n
".
% wwe ??? parameter
"
MATCH (n { name: $name})
RETURN n
".
% wwe ??? parameter
"
MATCH (n)
WHERE n.name =~ $regex
RETURN n.name
".
% wwe ??? parameter
"
MATCH (n)
WHERE n.name STARTS WITH $name
RETURN n.name
".
% wwe ??? parameter
"
CREATE ($props)
".
% wwe ??? parameter
"
UNWIND $props AS properties
CREATE (n:Person)
SET n = properties
RETURN n
".
% wwe ??? parameter
"
MATCH (n)
WHERE n.name='Michaela'
SET n = $props
".
% wwe ??? parameter
"
MATCH (n)
RETURN n.name
SKIP $s
LIMIT $l
".
% wwe ??? parameter
"
MATCH (n)
WHERE id(n)= $id
RETURN n.name
".
% wwe ??? parameter
"
MATCH (n)
WHERE id(n) IN $ids
RETURN n.name
".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 6.3 Variables
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"
MATCH (n)-->(b) RETURN b
".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 6.4 Operators
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"
MATCH (n) WHERE 21 < n.age <= 30 RETURN n
".
"
MATCH (n) WHERE 21 < n.age AND n.age <= 30 RETURN n
".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 6.5 Comments
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% wwe "MATCH (n) RETURN n //This is an end of line comment".
% wwe "
% wwe MATCH (n)
% wwe //This is a whole line comment
% wwe RETURN n
% wwe ".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 6.6 Patterns
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"
MATCH (me)-[:KNOWS*1..2]-(remote_friend)
WHERE me.name = \"Filipa\"
RETURN remote_friend.name
".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 6.7 Lists
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"
RETURN [0,1,2,3,4,5,6,7,8,9] AS list
".
"
RETURN range(0,10)[3]
".
"
RETURN range(0,10)[-3]
".
% wwe ??? range literal
%"
%RETURN range(0,10)[0..3]
%".
% wwe ??? signed integer
% wwe ??? range literal
%"
%RETURN range(0,10)[0..5]
%".
"
RETURN range(0,10)[-5..]
".
"
RETURN range(0,10)[..4]
".
"
RETURN range(0,10)[15]
".
% wwe ??? range literal
%"
%RETURN range(0,10)[5..15]
%".
% wwe ??? range literal
%"
%RETURN size(range(0,10)[0..3])
%".
"
RETURN [x IN range(0,10) WHERE x % 2 = 0 | x^3] AS result
".
"
RETURN [x IN range(0,10) WHERE x % 2 = 0] AS result
".
"
RETURN [x IN range(0,10)| x^3] AS result
".
"
RETURN { key : \"Value\", listKey: [{ inner: \"Map1\" }, { inner: \"Map2\" }]} AS result
".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 7.1 Return
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"
MATCH (n { name: \"B\" })
RETURN n
".
"
MATCH (n { name: \"A\" })-[r:KNOWS]->(c)
RETURN r
".
"
MATCH (n { name: \"A\" })
RETURN n.name
".
"
MATCH p=(a { name: \"A\" })-[r]->(b)
RETURN *
".
"
MATCH (`This isn't a common variable`)
WHERE `This isn't a common variable`.name='A'
RETURN `This isn't a common variable`.happy
".
"
MATCH (a { name: \"A\" })
RETURN a.age AS SomethingTotallyDifferent
".
"
MATCH (n)
RETURN n.age
".
"
MATCH (a { name: \"A\" })
RETURN a.age > 30, \"I'm a literal\",(a)-->()
".
"
MATCH (a { name: \"A\" })-->(b)
RETURN DISTINCT b
".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 7.2 Order by
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"
MATCH (n)
RETURN n
ORDER BY n.name
".
"
MATCH (n)
RETURN n
ORDER BY n.age, n.name
".
"
MATCH (n)
RETURN n
ORDER BY n.name DESC
".
"
MATCH (n)
RETURN n.length, n
ORDER BY n.length
".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 7.3 Limit
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"
MATCH (n)
RETURN n
ORDER BY n.name
LIMIT 3
".
"
MATCH (n)
RETURN n
ORDER BY n.name
LIMIT toInt(3 * rand())+ 1
".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 7.4 Skip
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"
MATCH (n)
RETURN n
ORDER BY n.name
SKIP 3
".
"
MATCH (n)
RETURN n
ORDER BY n.name
SKIP 1
LIMIT 2
".
"
MATCH (n)
RETURN n
ORDER BY n.name
SKIP toInt(3*rand())+ 1
".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 7.5 With
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"
MATCH (david { name: \"David\" })--(otherPerson)-->()
WITH otherPerson, count(*) AS foaf
WHERE foaf > 1
RETURN otherPerson
".
"
MATCH (n)
WITH n
ORDER BY n.name DESC LIMIT 3
RETURN collect(n.name)
".
"
MATCH (n { name: \"Anders\" })--(m)
WITH m
ORDER BY m.name DESC LIMIT 1
MATCH (m)--(o)
RETURN o.name
".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 7.6 Unwind
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"
UNWIND[1,2,3] AS x
RETURN x
".
"
WITH [1,1,2,2] AS coll UNWIND coll AS x
WITH DISTINCT x
RETURN collect(x) AS SET_4711
".
% wwe ??? parameter
"
UNWIND $events AS event
MERGE (y:Year { year:event.year })
MERGE (y)<-[:IN_4711]-(e:Event { id:event.id })
RETURN e.id AS x
ORDER BY x
".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 7.7 Union
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"
MATCH (n:Actor)
RETURN n.name AS name
UNION ALL MATCH (n:Movie)
RETURN n.title AS name
".
"
MATCH (n:Actor)
RETURN n.name AS name
UNION
MATCH (n:Movie)
RETURN n.title AS name
".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 7.8 Call
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% wwe "CALL db.labels".
% wwe "CALL org.neo4j.procedure.example.addNodeToIndex('users', 0, 'name')".
% wwe "CALL org.neo4j.procedure.example.addNodeToIndex".
% wwe "CALL org.neo4j.procedure.example.addNodeToIndex('users', { node }, 'name')".
% wwe "CALL db.labels() YIELD label RETURN count(label) AS numLabels".
% wwe "CALL db.propertyKeys() YIELD propertyKey AS prop MATCH (n) WHERE n[prop] IS NOT NULL RETURN prop, count(n) AS numNodes".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 8.1 Match
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"
MATCH (n)
RETURN n
".
"
MATCH (movie:Movie)
RETURN movie.title
".
"
MATCH (director { name:'Oliver Stone' })--(movie)
RETURN movie.title
".
"
MATCH (:Person { name:'Oliver Stone' })--(movie:Movie)
RETURN movie.title
".
"
MATCH (:Person { name:'Oliver Stone' })-->(movie)
RETURN movie.title
".
"
MATCH (:Person { name:'Oliver Stone' })-[r]->(movie)
RETURN type(r)
".
"
MATCH (wallstreet:Movie { title:'Wall Street' })<-[:ACTED_IN]-(actor)
RETURN actor.name
".
"
MATCH (wallstreet { title:'Wall Street' })<-[:ACTED_IN|:DIRECTED]-(person)
RETURN person.name
".
"
MATCH (wallstreet { title:'Wall Street' })<-[r:ACTED_IN]-(actor)
RETURN r.role
".
"
MATCH (charlie:Person { name:'Charlie Sheen' }),(rob:Person { name:'Rob Reiner' })
CREATE (rob)-[:`TYPE WITH SPACE`]->(charlie)
".
"
MATCH (n { name:'Rob Reiner' })-[r:`TYPE WITH SPACE`]->()
RETURN type(r)
".
"
MATCH (charlie { name:'Charlie Sheen' })-[:ACTED_IN]->(movie)<-[:DIRECTED]-(director)
RETURN movie.title, director.name
".
"
MATCH (martin { name:'Charlie Sheen' })-[:ACTED_IN*1..3]-(movie:Movie)
RETURN movie.title
".
% wwe "MATCH (actor { name:'Charlie Sheen' })-[r:ACTED_IN*2]-(co_actor) RETURN r".
"
MATCH (charlie:Person { name:'Charlie Sheen' }),(martin:Person { name:'Martin Sheen' })
CREATE (charlie)-[:X { blocked:false }]->(:Unblocked)<-[:X { blocked:false }]-(martin)
CREATE (charlie)-[:X { blocked:true }]->(:Blocked)<-[:X { blocked:false }]-(martin)
".
"
MATCH (wallstreet:Movie { title:'Wall Street' })-[*0..1]-(x)
RETURN x
".
"
MATCH p =(michael { name:'Michael Douglas' })-->()
RETURN p
".
"
MATCH (a)-[r]-(b)
WHERE id(r)= 0
RETURN a,b
".
"
MATCH (n)
WHERE id(n)= 0
RETURN n
".
"
MATCH ()-[r]->()
WHERE id(r)= 0
RETURN r
".
"
MATCH (n)
WHERE id(n) IN [0,3,5]
RETURN n
".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 8.2 Optional Match
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"
MATCH (a:Movie { title: 'Wall Street' })
OPTIONAL MATCH (a)-->(x)
RETURN x
".
"
MATCH (a:Movie { title: 'Wall Street' })
OPTIONAL MATCH (a)-->(x)
RETURN x, x.name
".
"
MATCH (a:Movie { title: 'Wall Street' })
OPTIONAL MATCH (a)-[r:ACTS_IN]->()
RETURN r
".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 8.3 Where
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% wwe ??? parenthesized expression
"
MATCH (n)
WHERE n.name = 'Peter' XOR n.age < 30 AND n.name = \"Tobias\" OR NOT n.name = \"Tobias\" OR n.name=\"Peter\"
RETURN n".
"
MATCH (n)
WHERE n:Swedish
RETURN n
".
"
MATCH (n)
WHERE n.age < 30
RETURN n
".
"
MATCH (n)-[k:KNOWS]->(f)
WHERE k.since < 2000
RETURN f
".
% wwe ??? parameter
"
MATCH (n)
WHERE n[toLower($prop)]< 30
RETURN n
".
"
MATCH (n)
WHERE exists(n.belt)
RETURN n
".
"
MATCH (n)
WHERE n.name STARTS WITH 'Pet'
RETURN n
".
"
MATCH (n)
WHERE n.name ENDS WITH 'ter'
RETURN n
".
"
MATCH (n)
WHERE n.name CONTAINS 'ete'
RETURN n
".
"
MATCH (n)
WHERE NOT n.name ENDS WITH 's'
RETURN n
".
"
MATCH (n)
WHERE n.name =~ 'Tob.*'
RETURN n
".
"
MATCH (n)
WHERE n.address =~ 'Sweden/Malmo'
RETURN n
".
"
MATCH (n)
WHERE n.name =~ '(?i)ANDR.*'
RETURN n
".
"
MATCH (tobias { name: 'Tobias' }),(others)
WHERE others.name IN ['Andres', 'Peter'] AND (tobias)<--(others)
RETURN others
".
"
MATCH (persons),(peter { name: 'Peter' })
WHERE NOT (persons)-->(peter)
RETURN persons
".
"
MATCH (n)
WHERE (n)-[:KNOWS]-({ name:'Tobias' })
RETURN n
".
"
MATCH (n)-[r]->()
WHERE n.name='Andres' AND type(r)=~ 'K.*'
RETURN r
".
"
MATCH (a)
WHERE a.name IN [\"Peter\", \"Tobias\"]
RETURN a
".
"
MATCH (n)
WHERE n.belt = 'white'
RETURN n
".
"
MATCH (n)
WHERE n.belt = 'white' OR n.belt IS NULL RETURN n
ORDER BY n.name
".
"
MATCH (person)
WHERE person.name = 'Peter' AND person.belt IS NULL RETURN person
".
"
MATCH (a)
WHERE a.name >= 'Peter'
RETURN a
".
"
MATCH (a)
WHERE a.name > 'Andres' AND a.name < 'Tobias'
RETURN a
".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 8.5 Aggregation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"
RETURN n, count(*)
".
"
MATCH (me:Person)-->(friend:Person)-->(friend_of_friend:Person)
WHERE me.name = 'A'
RETURN count(DISTINCT friend_of_friend), count(friend_of_friend)
".
"
MATCH (n { name: 'A' })-->(x)
RETURN n, count(*)
".
"
MATCH (n { name: 'A' })-[r]->()
RETURN type(r), count(*)
".
"
MATCH (n { name: 'A' })-->(x)
RETURN count(x)
".
"
MATCH (n:Person)
RETURN count(n.property)
".
"
MATCH (n:Person)
RETURN sum(n.property)
".
"
MATCH (n:Person)
RETURN avg(n.property)
".
"
MATCH (n:Person)
RETURN percentileDisc(n.property, 0.5)
".
"
MATCH (n:Person)
RETURN percentileCont(n.property, 0.4)
".
"
MATCH (n)
WHERE n.name IN ['A', 'B', 'C']
RETURN stdev(n.property)
".
"
MATCH (n)
WHERE n.name IN ['A', 'B', 'C']
RETURN stdevp(n.property)
".
"
MATCH (n:Person)
RETURN max(n.property)
".
"
MATCH (n:Person)
RETURN min(n.property)
".
"
MATCH (n:Person)
RETURN collect(n.property)
".
"
MATCH (a:Person { name: 'A' })-->(b)
RETURN count(DISTINCT b.eyes)
".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 9.1 Create
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"
CREATE (n)
".
"
CREATE (n),(m)
".
"
CREATE (n:Person)
".
"
CREATE (n:Person:Swedish)
".
"
CREATE (n:Person { name : 'Andres', title : 'Developer' })
".
"
CREATE (a { name : 'Andres' })
RETURN a
".
"
MATCH (a:Person),(b:Person)
WHERE a.name = 'Node A' AND b.name = 'Node B'
CREATE (a)-[r:RELTYPE]->(b)
RETURN r
".
"
MATCH (a:Person),(b:Person)
WHERE a.name = 'Node A' AND b.name = 'Node B'
CREATE (a)-[r:RELTYPE { name : a.name + '<->' + b.name }]->(b)
RETURN r
".
"
CREATE p =(andres { name:'Andres' })-[:WORKS_AT]->(neo)<-[:WORKS_AT]-(michael { name:'Michael' })
RETURN p
".
% wwe ??? parameter
"
CREATE (n:Person $props)
RETURN n
".
% wwe ??? parameter
"
UNWIND $props AS map
CREATE (n)
SET n = map
".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 9.2 Merge
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"
MERGE (robert:Critic)
RETURN robert, labels(robert)
".
"
MERGE (charlie { name:'Charlie Sheen', age:10 })
RETURN charlie
".
"
MERGE (michael:Person { name:'Michael Douglas' })
RETURN michael.name, michael.bornIn
".
"
MATCH (person:Person)
MERGE (city:City { name: person.bornIn })
RETURN person.name, person.bornIn, city
".
"
MERGE (keanu:Person { name:'Keanu Reeves' })
ON CREATE SET keanu.created = timestamp()
RETURN keanu.name, keanu.created
".
"
MERGE (person:Person)
ON MATCH SET person.found = TRUE RETURN person.name, person.found
".
"
MERGE (keanu:Person { name:'Keanu Reeves' })
ON CREATE SET keanu.created = timestamp()
ON MATCH SET keanu.lastSeen = timestamp()
RETURN keanu.name, keanu.created, keanu.lastSeen
".
"
MERGE (person:Person)
ON MATCH SET person.found = TRUE , person.lastAccessed = timestamp()
RETURN person.name, person.found, person.lastAccessed
".
"
MATCH (charlie:Person { name:'Charlie Sheen' }),(wallStreet:Movie { title:'Wall Street' })
MERGE (charlie)-[r:ACTED_IN]->(wallStreet)
RETURN charlie.name, type(r), wallStreet.title
".
"
MATCH (oliver:Person { name:'Oliver Stone' }),(reiner:Person { name:'Rob Reiner' })
MERGE (oliver)-[:DIRECTED]->(movie:Movie)<-[:ACTED_IN]-(reiner)
RETURN movie
".
"
MATCH (charlie:Person { name:'Charlie Sheen' }),(oliver:Person { name:'Oliver Stone' })
MERGE (charlie)-[r:KNOWS]-(oliver)
RETURN r
".
"
MATCH (person:Person)
MERGE (city:City { name: person.bornIn })
MERGE (person)-[r:BORN_IN]->(city)
RETURN person.name, person.bornIn, city
".
"
MATCH (person:Person)
MERGE (person)-[r:HAS_CHAUFFEUR]->(chauffeur:Chauffeur { name: person.chauffeurName })
RETURN person.name, person.chauffeurName, chauffeur
".
"
MERGE (laurence:Person { name: 'Laurence Fishburne' })
RETURN laurence.name
".
"
MERGE (oliver:Person { name:'Oliver Stone' })
RETURN oliver.name, oliver.bornIn
".
"
MERGE (michael:Person { name:'Michael Douglas', role:'Gordon Gekko' })
RETURN michael
".
"
MERGE (oliver:Person { name:'Oliver Stone', role:'Gordon Gekko' })
RETURN oliver
".
% wwe ??? parameter
"
MERGE (person:Person { name: $param.name, role: $param.role })
RETURN person.name, person.role
".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 9.3 Set
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"
MATCH (n { name: 'Andres' })
SET n.surname = 'Taylor'
RETURN n
".
"
MATCH (n { name: 'Andres' })
SET n.name = NULL RETURN n
".
"
MATCH (at { name: 'Andres' }),(pn { name: 'Peter' })
SET at = pn
RETURN at, pn
".
"
MATCH (peter { name: 'Peter' })
SET peter += { hungry: TRUE , position: 'Entrepreneur' }
".
% wwe ??? parameter
"
MATCH (n { name: 'Andres' })
SET n.surname = $surname
RETURN n
".
% wwe ??? parameter
"
MATCH (n { name: 'Andres' })
SET n = $props
RETURN n
".
"
MATCH (n { name: 'Andres' })
SET n.position = 'Developer', n.surname = 'Taylor'
".
"
MATCH (n { name: 'Stefan' })
SET n :German
RETURN n
".
"
MATCH (n { name: 'Emil' })
SET n :Swedish:Bossman
RETURN n
".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 9.4 Delete
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"
MATCH (n:Useless)
DELETE n
".
"
MATCH (n)
DETACH DELETE n
".
"
MATCH (n { name:'Andres' })
DETACH DELETE n
".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 9.5 Remove
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"
MATCH (andres { name: 'Andres' })
REMOVE andres.age
RETURN andres
".
"
MATCH (n { name: 'Peter' })
REMOVE n:German
RETURN n
".
"
MATCH (n { name: 'Peter' })
REMOVE n:German:Swedish
RETURN n
".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 10.1 Predicates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"
MATCH p=(a)-[*1..3]->(b)
WHERE a.name='Alice' AND b.name='Daniel' AND ALL (x IN nodes(p) WHERE x.age > 30)
RETURN p
".
"
MATCH (a)
WHERE a.name='Eskil' AND ANY (x IN a.array WHERE x = \"one\")
RETURN a
".
"
MATCH p=(n)-[*1..3]->(b)
WHERE n.name='Alice' AND NONE (x IN nodes(p) WHERE x.age = 25)
RETURN p
".
"
MATCH p=(n)-->(b)
WHERE n.name='Alice' AND SINGLE (var IN nodes(p) WHERE var.eyes = \"blue\")
RETURN p
".
"
MATCH (n)
WHERE EXISTS(n.name)
RETURN n.name AS name, EXISTS((n)-[:MARRIED]->()) AS is_married
".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 10.2 Scalar Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"
RETURN size(['Alice', 'Bob']) AS col
".
"
MATCH (a)
WHERE a.name='Alice'
RETURN size((a)-->()-->()) AS fof
".
"
MATCH p=(a)-->(b)-->(c)
WHERE a.name='Alice'
RETURN length(p)
".
"
MATCH (a)
WHERE length(a.name)> 6
RETURN length(a.name)
".
"
MATCH (n)-[r]->()
WHERE n.name='Alice'
RETURN type(r)
".
"
MATCH (a)
RETURN id(a)
".
"
MATCH (a)
WHERE a.name='Alice'
RETURN coalesce(a.hairColor, a.eyes)
".
"
MATCH (a)
WHERE a.name='Eskil'
RETURN a.array, head(a.array)
".
"
MATCH (a)
WHERE a.name='Eskil'
RETURN a.array, last(a.array)
".
"
RETURN timestamp()
".
"
MATCH (x:foo)-[r]-()
RETURN startNode(r)
".
"
MATCH (x:foo)-[r]-()
RETURN endNode(r)
".
"
CREATE (p:Person { name: 'Stefan', city: 'Berlin' })
RETURN properties(p)
".
"
RETURN toInt(\"42\"), toInt(\"not a number\")
".
"
RETURN toFloat(\"11.5\"), toFloat(\"not a number\")
".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 10.3 List Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"
MATCH p=(a)-->(b)-->(c)
WHERE a.name='Alice' AND c.name='Eskil'
RETURN nodes(p)
".
"
MATCH p=(a)-->(b)-->(c)
WHERE a.name='Alice' AND c.name='Eskil'
RETURN relationships(p)
".
"
MATCH (a)
WHERE a.name='Alice'
RETURN labels(a)
".
"
MATCH (a)
WHERE a.name='Alice'
RETURN keys(a)
".
"
MATCH p=(a)-->(b)-->(c)
WHERE a.name='Alice' AND b.name='Bob' AND c.name='Daniel'
RETURN extract(n IN nodes(p)| n.age) AS extracted
".
"
MATCH (a)
WHERE a.name='Eskil'
RETURN a.array, filter(x IN a.array WHERE size(x)= 3)
".
"
MATCH (a)
WHERE a.name='Eskil'
RETURN a.array, tail(a.array)
".
"
RETURN range(0,10), range(2,18,3)
".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 10.4 Math functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"
MATCH (a),(e)
WHERE a.name = 'Alice' AND e.name = 'Eskil'
RETURN a.age, e.age, abs(a.age - e.age)
".
"
RETURN ceil(0.1)
".
"
RETURN floor(0.9)
".
"
RETURN round(3.141592)
".
"
RETURN sign(-17), sign(0.1)
".
"
RETURN rand()
".
"
RETURN log(27)
".
"
RETURN log10(27)
".
"
RETURN exp(2)
".
"
RETURN e()
".
"
RETURN sqrt(256)
".
"
RETURN sin(0.5)
".
"
RETURN cos(0.5)
".
"
RETURN tan(0.5)
".
"
RETURN cot(0.5)
".
"
RETURN asin(0.5)
".
"
RETURN acos(0.5)
".
"
RETURN atan(0.5)
".
"
RETURN atan2(0.5, 0.6)
".
"
RETURN pi()
".
"
RETURN degrees(3.14159)
".
"
RETURN radians(180)
".
"
RETURN haversin(0.5)
".
"
CREATE (ber:City { lat: 52.5, lon: 13.4 }),(sm:City { lat: 37.5, lon: -122.3 })
RETURN 2 * 6371 * asin(sqrt(haversin(radians(sm.lat - ber.lat))+ cos(radians(sm.lat))*
cos(radians(ber.lat))* haversin(radians(sm.lon - ber.lon)))) AS dist
".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 10.5 String functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"
RETURN replace(\"hello\", \"l\", \"w\")
".
"
RETURN substring(\"hello\", 1, 3), substring(\"hello\", 2)
".
"
RETURN left(\"hello\", 3)
".
"
RETURN right(\"hello\", 3)
".
"
RETURN ltrim(\" hello\")
".
"
RETURN rtrim(\"hello \")
".
"
RETURN trim(\" hello \")
".
"
RETURN lower(\"HELLO\")
".
"
RETURN upper(\"hello\")
".
"
RETURN split(\"one,two\", \",\")
".
"
RETURN reverse(\"anagram\")
".
"
RETURN toString(11.5), toString(\"already a string\"), toString(TRUE )
".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 11.1 Indexes
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"
MATCH (person:Person { name: 'Andres' })
RETURN person
".
"
MATCH (person:Person)
WHERE person.name = 'Andres'
RETURN person
".
"
MATCH (person:Person)
WHERE person.name > 'B'
RETURN person
".
"
MATCH (person:Person)
WHERE person.name IN ['Andres', 'Mark']
RETURN person
".
"
MATCH (person:Person)
WHERE person.name STARTS WITH 'And'
RETURN person
".
"
MATCH (p:Person)
WHERE exists(p.name)
RETURN p
".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 11.2 Constraints
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"
CREATE (book:Book { isbn: '1449356265', title: 'Graph Databases' })
".
"
CREATE (book:Book { isbn: '1449356265', title: 'Graph Databases' })
".
"
CREATE (book:Book { isbn: '1449356265', title: 'Graph Databases' })
".
"
CREATE (book:Book { title: 'Graph Databases' })
".
"
MATCH (book:Book { title: 'Graph Databases' })
REMOVE book.isbn
".
"
CREATE (user:User)-[like:LIKED { day: 'yesterday' }]->(book:Book)
".
"
CREATE (user:User)-[like:LIKED]->(book:Book)
".
"
MATCH (user:User)-[like:LIKED]->(book:Book)
REMOVE like.day
".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 12.3 Basis query tuning
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"
MATCH (p { name:\"Tom Hanks\" })
RETURN p
".
"
MATCH (p:Person { name:\"Tom Hanks\" })
RETURN p
".
"
MATCH (p:Person { name:\"Tom Hanks\" })
RETURN p
".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 12.4 Delete
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"
MATCH (liskov:Scientist { name:'Liskov' })-[:KNOWS]->(wing:Scientist)-[:RESEARCHED]->(cs:Science {
name:'Computer Science' })<-[:RESEARCHED]-(conway:Scientist { name: 'Conway' })
RETURN 1 AS column
".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 13.1 Starting point operators
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"
MATCH (n)
RETURN n
".
"
MATCH (n1)-[r]->()
WHERE id(r)= 0
RETURN r, n1
".
"
MATCH (n)
WHERE id(n)= 0
RETURN n
".
"
MATCH (person:Person)
RETURN person
".
"
MATCH (location:Location { name: \"Malmo\" })
RETURN location
".
"
MATCH (l:Location)
WHERE l.name STARTS WITH 'Lon'
RETURN l
".
"
MATCH (l:Location)
WHERE l.name CONTAINS 'al'
RETURN l
".
"
MATCH (l:Location)
WHERE exists(l.name)
RETURN l
".
"
MATCH (n1)-[r]-()
WHERE id(r)= 1
RETURN r, n1
".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 13.2 Expand
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"
MATCH (p:Person { name: \"me\" })-[:FRIENDS_WITH]->(fof)
RETURN fof
".
"
MATCH (p:Person { name: \"me\" })-[:FRIENDS_WITH]->(fof)-->(p)
RETURN fof
".
"
MATCH (p:Person)
OPTIONAL MATCH (p)-[works_in:WORKS_IN]->(l)
WHERE works_in.duration > 180
RETURN p, l
".
"
MATCH (p:Person)-[:FRIENDS_WITH]->(f)
WITH p, count(f) AS fs
WHERE fs > 2
OPTIONAL MATCH (p)-[:WORKS_IN]->(city)
RETURN city.name
".
"
MATCH (p:Person)
WHERE (p)-[:FRIENDS_WITH]->()
RETURN p.name
".
"
MATCH (me:Person { name: \"me\" }),(other:Person)
WHERE NOT (me)-[:FRIENDS_WITH]->(other)
RETURN other.name
".
"
MATCH (other:Person)
WHERE (other)-[:FRIENDS_WITH]->() OR (other)-[:WORKS_IN]->()
RETURN other.name
".
% wwe ??? parenthesized expression
%"
%MATCH (other:Person)
%WHERE NOT ((other)-[:FRIENDS_WITH]->()) OR (other)-[:WORKS_IN]->()
%RETURN other.name
%".
"
MATCH (other:Person)
WHERE other.age > 25 OR (other)-[:FRIENDS_WITH]->()
RETURN other.name
".
"
MATCH (other:Person)
WHERE other.age > 25 OR NOT (other)-[:FRIENDS_WITH]->()
RETURN other.name
".
"
MERGE (p:Person { name: 'Andres' })
ON MATCH SET p.exists_4711 = TRUE
".
"
MERGE (p:Person { name: 'Andres' })
ON MATCH SET p.exists_4711 = TRUE
".
"
MERGE (t:Team { name: 'Engineering', id: 42 })
".
"
MATCH (andy:Person { name:'Andreas' })-[:WORKS_IN]->(loc)<-[:WORKS_IN]-(matt:Person { name:'Mattis' })
RETURN loc.name
".
"
MATCH (me:Person)-[:FRIENDS_WITH]-()-[:FRIENDS_WITH]-(other)
WHERE NOT (me)-[:FRIENDS_WITH]-(other)
RETURN other.name
".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 13.4 Row operators
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"
MATCH (a)-[r]-(b)
DELETE r,a,b
MERGE ()
".
"
MATCH (l:Location)<-[:WORKS_IN]-(p:Person)
RETURN DISTINCT l
".
"
MATCH (l:Location)<-[:WORKS_IN]-(p:Person)
RETURN l.name AS location, COLLECT(p.name) AS people
".
"
MATCH (p:Person)
RETURN count(p) AS people
".
"
MATCH (p:Person)-[r:WORKS_IN]->()
RETURN count(r) AS jobs
".
"
MATCH (p:Person)
WHERE p.name =~ \"^a.*\"
RETURN p
".
"
MATCH (p:Person)
RETURN p
LIMIT 3
".
"
RETURN \"hello\" AS greeting
".
"
MATCH (p:Person)
RETURN p
ORDER BY p.id
SKIP 1
".
"
MATCH (p:Person)
RETURN p
ORDER BY p.name
".
"
MATCH (p:Person)
RETURN p
ORDER BY p.name
LIMIT 2
".
"
MATCH (p:Location)
RETURN p.name
UNION ALL MATCH (p:Country)
RETURN p.name
".
"
UNWIND range(1,5) AS value
RETURN value;
".
% wwe "CALL db.labels() YIELD label RETURN * ORDER BY label".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 13.5 Update operators
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"
CREATE (:Person)
".
