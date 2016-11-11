%%%-------------------------------------------------------------------
%%% File        : third_party_examples_SUITE.erl
%%% Description : Test Suite covering examples from thirs parties.
%%%
%%% Created     : 04.2016
%%%-------------------------------------------------------------------
-module(third_party_examples_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% COMMON TEST CALLBACK FUNCTIONS - SUITE
%%--------------------------------------------------------------------

suite() ->
    [
        {timetrap, {minutes, 10}}
    ].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

%%--------------------------------------------------------------------
%% COMMON TEST CALLBACK FUNCTIONS - GROUPS - STRUCTURE
%%--------------------------------------------------------------------

groups() ->
    [
        {neo4j, [], [
            test_neo4j_4_2_patterns,
            test_neo4j_4_3_results,
            test_neo4j_4_4_large,
            test_neo4j_4_5_constraints,
            test_neo4j_5_1_what,
            test_neo4j_5_2_updating,
            test_neo4j_5_4_uniqueness,
            test_neo4j_5_5_parameters,
            test_neo4j_6_3_variables,
            test_neo4j_6_4_operators,
            test_neo4j_6_5_comments,
            test_neo4j_6_6_patterns,
            test_neo4j_6_7_lists,
            test_neo4j_7_1_return,
            test_neo4j_7_2_order_by,
            test_neo4j_7_3_limit,
            test_neo4j_7_4_skip,
            test_neo4j_7_5_with,
            test_neo4j_7_6_unwind,
            test_neo4j_7_7_union,
%           test_neo4j_7_8_call,                                                % not supported by openCypher
            test_neo4j_8_1_match,
            test_neo4j_8_2_optional_match,
            test_neo4j_8_3_where,
            test_neo4j_8_5_aggregation,
            test_neo4j_9_1_create,
            test_neo4j_9_2_merge,
            test_neo4j_9_3_set,
            test_neo4j_9_4_delete,
            test_neo4j_9_5_remove,
            test_neo4j_10_1_predicates,
            test_neo4j_10_2_scalar_functions,
            test_neo4j_10_3_list_functions,
            test_neo4j_10_4_math_functions,
            test_neo4j_10_5_string_functions,
            test_neo4j_11_1_indexes,
            test_neo4j_11_2_constraints,
            test_neo4j_12_3_basic_query_tuning,
            test_neo4j_12_4_using,
            test_neo4j_13_1_starting_point,
            test_neo4j_13_2_expand,
            test_neo4j_13_4_row_operators,
            test_neo4j_13_5_update_operators
        ]}
    ].

%%--------------------------------------------------------------------
%% COMMON TEST CALLBACK FUNCTIONS - GROUPS - INIT
%%--------------------------------------------------------------------

init_per_group(_, Config) ->
    Config.

%%--------------------------------------------------------------------
%% COMMON TEST CALLBACK FUNCTIONS - GROUPS - END
%%--------------------------------------------------------------------

end_per_group(_, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% COMMON TEST CALLBACK FUNCTIONS - ALL
%%--------------------------------------------------------------------

all() ->
    [
        {group, neo4j}
    ].

%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Neo4j Developer Manual V3.0.
%%--------------------------------------------------------------------

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 4.2 Patterns in Practice
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test_neo4j_4_2_patterns(_Config) ->
    Cypher_01 = "CREATE (:Movie { title:\"The Matrix\",released:1997 })",
    octest:ct_string(Cypher_01),
    Cypher_02 = "CREATE (p:Person { name:\"Keanu Reeves\", born:1964 })
                 RETURN p",
    octest:ct_string(Cypher_02),
    Cypher_03 = "CREATE (a:Person { name:\"Tom Hanks\",
                   born:1956 })-[r:ACTED_IN { roles: [\"Forrest\"]}]->(m:Movie { title:\"Forrest Gump\",released:1994 })
                 CREATE (d:Person { name:\"Robert Zemeckis\", born:1951 })-[:DIRECTED]->(m)
                 RETURN a,d,r,m",
    octest:ct_string(Cypher_03),
    Cypher_04 = "MATCH (m:Movie)
                 RETURN m",
    octest:ct_string(Cypher_04),
    Cypher_05 = "MATCH (p:Person { name:\"Keanu Reeves\" })
                 RETURN p",
    octest:ct_string(Cypher_05),
    Cypher_06 = "MATCH (p:Person { name:\"Tom Hanks\" })-[r:ACTED_IN]->(m:Movie)
                 RETURN m.title, r.roles",
    octest:ct_string(Cypher_06),
    Cypher_07 = "MATCH (p:Person { name:\"Tom Hanks\" })
                 CREATE (m:Movie { title:\"Cloud Atlas\",released:2012 })
                 CREATE (p)-[r:ACTED_IN { roles: ['Zachry']}]->(m)
                 RETURN p,r,m",
    octest:ct_string(Cypher_07),
    Cypher_08 = "MERGE (m:Movie { title:\"Cloud Atlas\" })
                 ON CREATE SET m.released = 2012
                 RETURN m",
    octest:ct_string(Cypher_08),
    Cypher_09 = "MATCH (m:Movie { title:\"Cloud Atlas\" })
                 MATCH (p:Person { name:\"Tom Hanks\" })
                 MERGE (p)-[r:ACTED_IN]->(m)
                 ON CREATE SET r.roles =['Zachry']
                 RETURN p,r,m",
    octest:ct_string(Cypher_09),
    Cypher_10 = "CREATE (y:Year { year:2014 })
                 MERGE (y)<-[:IN_YEAR]-(m10:Month { month:10 })
                 MERGE (y)<-[:IN_YEAR]-(m11:Month { month:11 })
                 RETURN y,m10,m11",
    octest:ct_string(Cypher_10).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 4.3 Getting correct results
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test_neo4j_4_3_results(_Config) ->
    Cypher_01 = "CREATE (matrix:Movie { title:\"The Matrix\",released:1997 })
                 CREATE (cloudAtlas:Movie { title:\"Cloud Atlas\",released:2012 })
                 CREATE (forrestGump:Movie { title:\"Forrest Gump\",released:1994 })
                 CREATE (keanu:Person { name:\"Keanu Reeves\", born:1964 })
                 CREATE (robert:Person { name:\"Robert Zemeckis\", born:1951 })
                 CREATE (tom:Person { name:\"Tom Hanks\", born:1956 })
                 CREATE (tom)-[:ACTED_IN { roles: [\"Forrest\"]}]->(forrestGump)
                 CREATE (tom)-[:ACTED_IN { roles: ['Zachry']}]->(cloudAtlas)
                 CREATE (robert)-[:DIRECTED]->(forrestGump)",
    octest:ct_string(Cypher_01),
    Cypher_02 = "MATCH (m:Movie)
                 WHERE m.title = \"The Matrix\"
                 RETURN m",
    octest:ct_string(Cypher_02),
    Cypher_03 = "MATCH (m:Movie { title: \"The Matrix\" })
                 RETURN m",
    octest:ct_string(Cypher_03),
    Cypher_04 = "MATCH (p:Person)-[r:ACTED_IN]->(m:Movie)
                 WHERE p.name =~ \"K.+\" OR m.released > 2000 OR \"Neo\" IN r.roles
                 RETURN p,r,m",
    octest:ct_string(Cypher_04),
    Cypher_05 = "MATCH (p:Person)-[:ACTED_IN]->(m)
                 WHERE NOT (p)-[:DIRECTED]->()
                 RETURN p,m",
    octest:ct_string(Cypher_05),
    Cypher_06 = "MATCH (p:Person)
                 RETURN p, p.name AS name, upper(p.name), coalesce(p.nickname,\"n/a\") AS nickname, { name: p.name,
                   label:head(labels(p))} AS person",
    octest:ct_string(Cypher_06),
    Cypher_07 = "MATCH (:Person)
                 RETURN count(*) AS people",
    octest:ct_string(Cypher_07),
    Cypher_08 = "MATCH (actor:Person)-[:ACTED_IN]->(movie:Movie)<-[:DIRECTED]-(director:Person)
                 RETURN actor,director,count(*) AS collaborations",
    octest:ct_string(Cypher_08),
    Cypher_09 = "MATCH (a:Person)-[:ACTED_IN]->(m:Movie)
                 RETURN a,count(*) AS appearances
                 ORDER BY appearances DESC LIMIT 10;",
    octest:ct_string(Cypher_09),
    Cypher_10 = "MATCH (m:Movie)<-[:ACTED_IN]-(a:Person)
                 RETURN m.title AS movie, collect(a.name) AS cast, count(*) AS actors",
    octest:ct_string(Cypher_10).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 4.4 Composing large statements
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test_neo4j_4_4_large(_Config) ->
    Cypher_01 = "MATCH (actor:Person)-[r:ACTED_IN]->(movie:Movie)
                 RETURN actor.name AS name, type(r) AS acted_in, movie.title AS title
                 UNION
                 MATCH (director:Person)-[r:DIRECTED]->(movie:Movie)
                 RETURN director.name AS name, type(r) AS acted_in, movie.title AS title",
    octest:ct_string(Cypher_01),
    Cypher_02 = "MATCH (person:Person)-[:ACTED_IN]->(m:Movie)
                 WITH person, count(*) AS appearances, collect(m.title) AS movies
                 WHERE appearances > 1
                 RETURN person.name, appearances, movies",
    octest:ct_string(Cypher_02).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 4.5 Constraints and indexes
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test_neo4j_4_5_constraints(_Config) ->
    Cypher_03 = "CREATE (actor:Actor { name:\"Tom Hanks\" }),(movie:Movie { title:'Sleepless IN Seattle' }),
                 (actor)-[:ACTED_IN]->(movie);",
    octest:ct_string(Cypher_03),
    Cypher_04 = "MATCH (actor:Actor { name: \"Tom Hanks\" })
                 RETURN actor;",
    octest:ct_string(Cypher_04).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 5.1 What is Cypher?
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test_neo4j_5_1_what(_Config) ->
    Cypher_01 = "MATCH (john {name: 'John'})-[:friend]->()-[:friend]->(fof)
                 RETURN john.name, fof.name",
    octest:ct_string(Cypher_01),
    Cypher_02 = "MATCH (user)-[:friend]->(follower)
                 WHERE user.name IN ['Joe', 'John', 'Sara', 'Maria', 'Steve'] AND follower.name =~ 'S.*'
                 RETURN user.name, follower.name",
    octest:ct_string(Cypher_02).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 5.2 Updating the graph
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test_neo4j_5_2_updating(_Config) ->
    Cypher_01 = "MATCH (n {name: 'John'})-[:FRIEND]-(friend)
                 WITH n, count(friend) as friendsCount
                 WHERE friendsCount > 3
                 RETURN n, friendsCount",
    octest:ct_string(Cypher_01),
    Cypher_02 = "MATCH (n {name: 'John'})-[:FRIEND]-(friend)
                 WITH n, count(friend) as friendsCount
                 SET n.friendCount = friendsCount
                 RETURN n.friendsCount",
    octest:ct_string(Cypher_02).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 5.4 Uniqueness
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test_neo4j_5_4_uniqueness(_Config) ->
    Cypher_01 = "CREATE (adam:User { name: 'Adam' }),(pernilla:User { name: 'Pernilla' }),(david:User { name: 'David' }),
                   (adam)-[:FRIEND]->(pernilla),(pernilla)-[:FRIEND]->(david)",
    octest:ct_string(Cypher_01),
    Cypher_02 = "MATCH (user:User { name: 'Adam' })-[r1:FRIEND]-()-[r2:FRIEND]-(friend_of_a_friend)
                 RETURN friend_of_a_friend.name AS fofName",
    octest:ct_string(Cypher_02),
    Cypher_03 = "MATCH (user:User { name: 'Adam' })-[r1:FRIEND]-(friend)
                 MATCH (friend)-[r2:FRIEND]-(friend_of_a_friend)
                 RETURN friend_of_a_friend.name AS fofName",
    octest:ct_string(Cypher_03),
    Cypher_04 = "MATCH (user:User { name: 'Adam' })-[r1:FRIEND]-(friend),(friend)-[r2:FRIEND]-(friend_of_a_friend)
                 RETURN friend_of_a_friend.name AS fofName",
    octest:ct_string(Cypher_04).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 5.5 Parameters
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test_neo4j_5_5_parameters(_Config) ->
    Cypher_01 = "MATCH (n)
                 WHERE n.name = $name
                 RETURN n",
    % wwe ??? parameter
    octest:ct_string(Cypher_01),
    Cypher_02 = "MATCH (n { name: $name})
                 RETURN n",
    % wwe ??? parameter
    octest:ct_string(Cypher_02),
    Cypher_03 = "MATCH (n)
                 WHERE n.name =~ $regex
                 RETURN n.name",
    % wwe ??? parameter
    octest:ct_string(Cypher_03),
    Cypher_04 = "MATCH (n)
                 WHERE n.name STARTS WITH $name
                 RETURN n.name",
    % wwe ??? parameter
    octest:ct_string(Cypher_04),
    Cypher_05 = "CREATE ($props)",
    % wwe ??? parameter
    octest:ct_string(Cypher_05),
    Cypher_06 = "UNWIND $props AS properties
                 CREATE (n:Person)
                 SET n = properties
                 RETURN n",
    % wwe ??? parameter
    octest:ct_string(Cypher_06),
    Cypher_07 = "MATCH (n)
                 WHERE n.name='Michaela'
                 SET n = $props",
    % wwe ??? parameter
    octest:ct_string(Cypher_07),
    Cypher_08 = "MATCH (n)
                 RETURN n.name
                 SKIP $s
                 LIMIT $l",
    % wwe ??? parameter
    octest:ct_string(Cypher_08),
    Cypher_09 = "MATCH (n)
                 WHERE id(n)= $id
                 RETURN n.name",
    % wwe ??? parameter
    octest:ct_string(Cypher_09),
    Cypher_10 = "MATCH (n)
                 WHERE id(n) IN $ids
                 RETURN n.name",
    % wwe ??? parameter
    octest:ct_string(Cypher_10).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 6.3 Variables
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test_neo4j_6_3_variables(_Config) ->
    Cypher_01 = "MATCH (n)-->(b) RETURN b",
    octest:ct_string(Cypher_01).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 6.4 Operators
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test_neo4j_6_4_operators(_Config) ->
    Cypher_01 = "MATCH (n) WHERE 21 < n.age <= 30 RETURN n",
    octest:ct_string(Cypher_01),
    Cypher_02 = "MATCH (n) WHERE 21 < n.age AND n.age <= 30 RETURN n",
    octest:ct_string(Cypher_02).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 6.5 Comments
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test_neo4j_6_5_comments(_Config) ->
    Cypher_01 = "MATCH (n) RETURN n //This is an end of line comment",
    octest:ct_string(Cypher_01),
    Cypher_02 = "MATCH (n)
                 //This is a whole line comment
                 RETURN n",
    octest:ct_string(Cypher_02).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 6.6 Patterns
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test_neo4j_6_6_patterns(_Config) ->
    Cypher_01 = "MATCH (me)-[:KNOWS*1..2]-(remote_friend)
                 WHERE me.name = \"Filipa\"
                 RETURN remote_friend.name",
    octest:ct_string(Cypher_01).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 6.7 Lists
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test_neo4j_6_7_lists(_Config) ->
    Cypher_01 = "RETURN [0,1,2,3,4,5,6,7,8,9] AS list",
    octest:ct_string(Cypher_01),
    Cypher_02 = "RETURN range(0,10)[3]",
    octest:ct_string(Cypher_02),
    Cypher_03 = "RETURN range(0,10)[-3]",
    octest:ct_string(Cypher_03),
    _Cypher_04 = "RETURN range(0,10)[0..3]",
    % wwe ??? range literal
    %octest:ct_string(Cypher_04),
    _Cypher_05 = "RETURN range(0,10)[0..5]",
    % wwe ??? signed integer
    % wwe ??? range literal
    %octest:ct_string(Cypher_05),
    Cypher_06 = "RETURN range(0,10)[-5..]",
    octest:ct_string(Cypher_06),
    Cypher_07 = "RETURN range(0,10)[..4]",
    octest:ct_string(Cypher_07),
    Cypher_08 = "RETURN range(0,10)[15]",
    octest:ct_string(Cypher_08),
    _Cypher_09 = "RETURN range(0,10)[5..15]",
    % wwe ??? range literal
    %octest:ct_string(Cypher_09),
    _Cypher_10 = "RETURN size(range(0,10)[0..3])",
    % wwe ??? range literal
    %octest:ct_string(Cypher_10),
    Cypher_11 = "RETURN [x IN range(0,10) WHERE x % 2 = 0 | x^3] AS result",
    octest:ct_string(Cypher_11),
    Cypher_12 = "RETURN [x IN range(0,10) WHERE x % 2 = 0] AS result",
    octest:ct_string(Cypher_12),
    Cypher_13 = "RETURN [x IN range(0,10)| x^3] AS result",
    octest:ct_string(Cypher_13),
    Cypher_14 = "RETURN { key : \"Value\", listKey: [{ inner: \"Map1\" }, { inner: \"Map2\" }]} AS result",
    octest:ct_string(Cypher_14).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 7.1 Return
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test_neo4j_7_1_return(_Config) ->
    Cypher_01 = "MATCH (n { name: \"B\" })
                 RETURN n",
    octest:ct_string(Cypher_01),
    Cypher_02 = "MATCH (n { name: \"A\" })-[r:KNOWS]->(c)
                 RETURN r",
    octest:ct_string(Cypher_02),
    Cypher_03 = "MATCH (n { name: \"A\" })
                 RETURN n.name",
    octest:ct_string(Cypher_03),
    Cypher_04 = "MATCH p=(a { name: \"A\" })-[r]->(b)
                 RETURN *",
    octest:ct_string(Cypher_04),
    Cypher_05 = "MATCH (`This isn't a common variable`)
                 WHERE `This isn't a common variable`.name='A'
                 RETURN `This isn't a common variable`.happy",
    octest:ct_string(Cypher_05),
    Cypher_06 = "MATCH (a { name: \"A\" })
                 RETURN a.age AS SomethingTotallyDifferent",
    octest:ct_string(Cypher_06),
    Cypher_07 = "MATCH (n)
                 RETURN n.age",
    octest:ct_string(Cypher_07),
    Cypher_08 = "MATCH (a { name: \"A\" })
                 RETURN a.age > 30, \"I'm a literal\",(a)-->()",
    octest:ct_string(Cypher_08),
    Cypher_09 = "MATCH (a { name: \"A\" })-->(b)
                 RETURN DISTINCT b",
    octest:ct_string(Cypher_09).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 7.2 Order by
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test_neo4j_7_2_order_by(_Config) ->
    Cypher_01 = "MATCH (n)
                 RETURN n
                 ORDER BY n.name",
    octest:ct_string(Cypher_01),
    Cypher_02 = "MATCH (n)
                 RETURN n
                 ORDER BY n.age, n.name",
    octest:ct_string(Cypher_02),
    Cypher_03 = "MATCH (n)
                 RETURN n
                 ORDER BY n.name DESC",
    octest:ct_string(Cypher_03),
    Cypher_04 = "MATCH (n)
                 RETURN n.length, n
                 ORDER BY n.length",
    octest:ct_string(Cypher_04).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 7.3 Limit
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test_neo4j_7_3_limit(_Config) ->
    Cypher_01 = "MATCH (n)
                 RETURN n
                 ORDER BY n.name
                 LIMIT 3",
    octest:ct_string(Cypher_01),
    Cypher_02 = "MATCH (n)
                 RETURN n
                 ORDER BY n.name
                 LIMIT toInt(3 * rand())+ 1",
    octest:ct_string(Cypher_02).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 7.4 Skip
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test_neo4j_7_4_skip(_Config) ->
    Cypher_01 = "MATCH (n)
                 RETURN n
                 ORDER BY n.name
                 SKIP 3",
    octest:ct_string(Cypher_01),
    Cypher_02 = "MATCH (n)
                 RETURN n
                 ORDER BY n.name
                 SKIP 1
                 LIMIT 2",
    octest:ct_string(Cypher_02),
    Cypher_03 = "MATCH (n)
                 RETURN n
                 ORDER BY n.name
                 SKIP toInt(3*rand())+ 1",
    octest:ct_string(Cypher_03).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 7.5 With
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test_neo4j_7_5_with(_Config) ->
    Cypher_01 = "MATCH (david { name: \"David\" })--(otherPerson)-->()
                 WITH otherPerson, count(*) AS foaf
                 WHERE foaf > 1
                 RETURN otherPerson",
    octest:ct_string(Cypher_01),
    Cypher_02 = "MATCH (n)
                 WITH n
                 ORDER BY n.name DESC LIMIT 3
                 RETURN collect(n.name)",
    octest:ct_string(Cypher_02),
    Cypher_03 = "MATCH (n { name: \"Anders\" })--(m)
                 WITH m
                 ORDER BY m.name DESC LIMIT 1
                 MATCH (m)--(o)
                 RETURN o.name",
    octest:ct_string(Cypher_03).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 7.6 Unwind
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test_neo4j_7_6_unwind(_Config) ->
    Cypher_01 = "UNWIND[1,2,3] AS x
                 RETURN x",
    octest:ct_string(Cypher_01),
    Cypher_02 = "WITH [1,1,2,2] AS coll UNWIND coll AS x
                 WITH DISTINCT x
                 RETURN collect(x) AS SET_4711",
    octest:ct_string(Cypher_02),
    Cypher_03 = "UNWIND $events AS event
                 MERGE (y:Year { year:event.year })
                 MERGE (y)<-[:IN_4711]-(e:Event { id:event.id })
                 RETURN e.id AS x
                 ORDER BY x",
    % wwe ??? parameter
    octest:ct_string(Cypher_03).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 7.7 Union
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test_neo4j_7_7_union(_Config) ->
    Cypher_01 = "MATCH (n:Actor)
                 RETURN n.name AS name
                 UNION ALL MATCH (n:Movie)
                 RETURN n.title AS name",
    octest:ct_string(Cypher_01),
    Cypher_02 = "MATCH (n:Actor)
                 RETURN n.name AS name
                 UNION
                 MATCH (n:Movie)
                 RETURN n.title AS name",
    octest:ct_string(Cypher_02).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 7.8 Call
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test_neo4j_7_8_call(_Config) ->
    Cypher_01 = "CALL db.labels",
    octest:ct_string(Cypher_01),
    Cypher_02 = "CALL org.neo4j.procedure.example.addNodeToIndex('users', 0, 'name')",
    octest:ct_string(Cypher_02),
    Cypher_03 = "CALL org.neo4j.procedure.example.addNodeToIndex",
    octest:ct_string(Cypher_03),
    Cypher_04 = "CALL org.neo4j.procedure.example.addNodeToIndex('users', { node }, 'name')",
    octest:ct_string(Cypher_04),
    Cypher_05 = "CALL db.labels() YIELD label
                 RETURN count(label) AS numLabels",
    octest:ct_string(Cypher_05),
    Cypher_06 = "CALL db.propertyKeys() YIELD propertyKey AS prop
                 MATCH (n)
                 WHERE n[prop] IS NOT NULL RETURN prop, count(n) AS numNodes",
    octest:ct_string(Cypher_06).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 8.1 Match
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test_neo4j_8_1_match(_Config) ->
    Cypher_01 = "MATCH (n)
                 RETURN n",
    octest:ct_string(Cypher_01),
    Cypher_02 = "MATCH (movie:Movie)
                 RETURN movie.title",
    octest:ct_string(Cypher_02),
    Cypher_03 = "MATCH (director { name:'Oliver Stone' })--(movie)
                 RETURN movie.title",
    octest:ct_string(Cypher_03),
    Cypher_04 = "MATCH (:Person { name:'Oliver Stone' })--(movie:Movie)
                 RETURN movie.title",
    octest:ct_string(Cypher_04),
    Cypher_05 = "MATCH (:Person { name:'Oliver Stone' })-->(movie)
                 RETURN movie.title",
    octest:ct_string(Cypher_05),
    Cypher_06 = "MATCH (:Person { name:'Oliver Stone' })-[r]->(movie)
                 RETURN type(r)",
    octest:ct_string(Cypher_06),
    Cypher_07 = "MATCH (wallstreet:Movie { title:'Wall Street' })<-[:ACTED_IN]-(actor)
                 RETURN actor.name",
    octest:ct_string(Cypher_07),
    Cypher_08 = "MATCH (wallstreet { title:'Wall Street' })<-[:ACTED_IN|:DIRECTED]-(person)
                 RETURN person.name",
    octest:ct_string(Cypher_08),
    Cypher_09 = "MATCH (wallstreet { title:'Wall Street' })<-[r:ACTED_IN]-(actor)
                 RETURN r.role",
    octest:ct_string(Cypher_09),
    Cypher_10 = "MATCH (charlie:Person { name:'Charlie Sheen' }),(rob:Person { name:'Rob Reiner' })
                 CREATE (rob)-[:`TYPE WITH SPACE`]->(charlie)",
    octest:ct_string(Cypher_10),
    Cypher_11 = "MATCH (n { name:'Rob Reiner' })-[r:`TYPE WITH SPACE`]->()
                 RETURN type(r)",
    octest:ct_string(Cypher_11),
    Cypher_12 = "MATCH (charlie { name:'Charlie Sheen' })-[:ACTED_IN]->(movie)<-[:DIRECTED]-(director)
                 RETURN movie.title, director.name",
    octest:ct_string(Cypher_12),
    Cypher_13 = "MATCH (martin { name:'Charlie Sheen' })-[:ACTED_IN*1..3]-(movie:Movie)
                 RETURN movie.title",
    octest:ct_string(Cypher_13),
    _Cypher_14 = "MATCH (actor { name:'Charlie Sheen' })-[r:ACTED_IN*2]-(co_actor)
                 RETURN r",
%   octest:ct_string(Cypher_14),                                                % not supported by openCypher ???
    Cypher_15 = "MATCH (charlie:Person { name:'Charlie Sheen' }),(martin:Person { name:'Martin Sheen' })
                 CREATE (charlie)-[:X { blocked:false }]->(:Unblocked)<-[:X { blocked:false }]-(martin)
                 CREATE (charlie)-[:X { blocked:true }]->(:Blocked)<-[:X { blocked:false }]-(martin)",
    octest:ct_string(Cypher_15),
    Cypher_16 = "MATCH (wallstreet:Movie { title:'Wall Street' })-[*0..1]-(x)
                 RETURN x",
    octest:ct_string(Cypher_16),
    Cypher_17 = "MATCH p =(michael { name:'Michael Douglas' })-->()
                 RETURN p",
    octest:ct_string(Cypher_17),
    Cypher_18 = "MATCH (a)-[r]-(b)
                 WHERE id(r)= 0
                 RETURN a,b",
    octest:ct_string(Cypher_18),
    Cypher_22 = "MATCH (n)
                 WHERE id(n)= 0
                 RETURN n",
    octest:ct_string(Cypher_22),
    Cypher_23 = "MATCH ()-[r]->()
                 WHERE id(r)= 0
                 RETURN r",
    octest:ct_string(Cypher_23),
    Cypher_24 = "MATCH (n)
                 WHERE id(n) IN [0,3,5]
                 RETURN n",
    octest:ct_string(Cypher_24).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 8.2 Optional Match
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test_neo4j_8_2_optional_match(_Config) ->
    Cypher_01 = "MATCH (a:Movie { title: 'Wall Street' })
                 OPTIONAL MATCH (a)-->(x)
                 RETURN x",
    octest:ct_string(Cypher_01),
    Cypher_02 = "MATCH (a:Movie { title: 'Wall Street' })
                 OPTIONAL MATCH (a)-->(x)
                 RETURN x, x.name",
    octest:ct_string(Cypher_02),
    Cypher_03 = "MATCH (a:Movie { title: 'Wall Street' })
                 OPTIONAL MATCH (a)-[r:ACTS_IN]->()
                 RETURN r",
    octest:ct_string(Cypher_03).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 8.3 Where
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test_neo4j_8_3_where(_Config) ->
    Cypher_01 = "MATCH (n)
                 WHERE n.name = 'Peter' XOR n.age < 30 AND n.name = \"Tobias\" OR NOT n.name = \"Tobias\" OR n.name=\"Peter\"
                 RETURN n",
    % wwe ??? parenthesized expression
    octest:ct_string(Cypher_01),
    Cypher_02 = "MATCH (n)
                 WHERE n:Swedish
                 RETURN n",
    octest:ct_string(Cypher_02),
    Cypher_03 = "MATCH (n)
                 WHERE n.age < 30
                 RETURN n",
    octest:ct_string(Cypher_03),
    Cypher_04 = "MATCH (n)-[k:KNOWS]->(f)
                 WHERE k.since < 2000
                 RETURN f",
    octest:ct_string(Cypher_04),
    Cypher_05 = "MATCH (n)
                 WHERE n[toLower($prop)]< 30
                 RETURN n",
    % wwe ??? parameter
    octest:ct_string(Cypher_05),
    Cypher_06 = "MATCH (n)
                 WHERE exists(n.belt)
                 RETURN n",
    octest:ct_string(Cypher_06),
    Cypher_07 = "MATCH (n)
                 WHERE n.name STARTS WITH 'Pet'
                 RETURN n",
    octest:ct_string(Cypher_07),
    Cypher_08 = "MATCH (n)
                 WHERE n.name ENDS WITH 'ter'
                 RETURN n",
    octest:ct_string(Cypher_08),
    Cypher_09 = "MATCH (n)
                 WHERE n.name CONTAINS 'ete'
                 RETURN n",
    octest:ct_string(Cypher_09),
    Cypher_10 = "MATCH (n)
                 WHERE NOT n.name ENDS WITH 's'
                 RETURN n",
    octest:ct_string(Cypher_10),
    Cypher_11 = "MATCH (n)
                 WHERE n.name =~ 'Tob.*'
                 RETURN n",
    octest:ct_string(Cypher_11),
    Cypher_12 = "MATCH (n)
                 WHERE n.address =~ 'Sweden/Malmo'
                 RETURN n",
    octest:ct_string(Cypher_12),
    Cypher_13 = "MATCH (n)
                 WHERE n.name =~ '(?i)ANDR.*'
                 RETURN n",
    octest:ct_string(Cypher_13),
    Cypher_14 = "MATCH (tobias { name: 'Tobias' }),(others)
                 WHERE others.name IN ['Andres', 'Peter'] AND (tobias)<--(others)
                 RETURN others",
    octest:ct_string(Cypher_14),
    Cypher_15 = "MATCH (persons),(peter { name: 'Peter' })
                 WHERE NOT (persons)-->(peter)
                 RETURN persons",
    octest:ct_string(Cypher_15),
    Cypher_16 = "MATCH (n)
                 WHERE (n)-[:KNOWS]-({ name:'Tobias' })
                 RETURN n",
    octest:ct_string(Cypher_16),
    Cypher_17 = "MATCH (n)-[r]->()
                 WHERE n.name='Andres' AND type(r)=~ 'K.*'
                 RETURN r",
    octest:ct_string(Cypher_17),
    Cypher_18 = "MATCH (a)
                 WHERE a.name IN [\"Peter\", \"Tobias\"]
                 RETURN a",
    octest:ct_string(Cypher_18),
    Cypher_19 = "MATCH (n)
                 WHERE n.belt = 'white'
                 RETURN n",
    octest:ct_string(Cypher_19),
    Cypher_20 = "MATCH (n)
                 WHERE n.belt = 'white' OR n.belt IS NULL RETURN n
                 ORDER BY n.name",
    octest:ct_string(Cypher_20),
    Cypher_21 = "MATCH (person)
                 WHERE person.name = 'Peter' AND person.belt IS NULL RETURN person",
    octest:ct_string(Cypher_21),
    Cypher_22 = "MATCH (a)
                 WHERE a.name >= 'Peter'
                 RETURN a",
    octest:ct_string(Cypher_22),
    Cypher_23 = "MATCH (a)
                 WHERE a.name > 'Andres' AND a.name < 'Tobias'
                 RETURN a",
    octest:ct_string(Cypher_23).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 8.5 Aggregation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test_neo4j_8_5_aggregation(_Config) ->
    Cypher_01 = "RETURN n, count(*)",
    octest:ct_string(Cypher_01),
    Cypher_02 = "MATCH (me:Person)-->(friend:Person)-->(friend_of_friend:Person)
                 WHERE me.name = 'A'
                 RETURN count(DISTINCT friend_of_friend), count(friend_of_friend)",
    octest:ct_string(Cypher_02),
    Cypher_03 = "MATCH (n { name: 'A' })-->(x)
                 RETURN n, count(*)",
    octest:ct_string(Cypher_03),
    Cypher_04 = "MATCH (n { name: 'A' })-[r]->()
                 RETURN type(r), count(*)",
    octest:ct_string(Cypher_04),
    Cypher_05 = "MATCH (n { name: 'A' })-->(x)
                 RETURN count(x)",
    octest:ct_string(Cypher_05),
    Cypher_06 = "MATCH (n:Person)
                 RETURN count(n.property)",
    octest:ct_string(Cypher_06),
    Cypher_07 = "MATCH (n:Person)
                 RETURN sum(n.property)",
    octest:ct_string(Cypher_07),
    Cypher_08 = "MATCH (n:Person)
                 RETURN avg(n.property)",
    octest:ct_string(Cypher_08),
    Cypher_09 = "MATCH (n:Person)
                 RETURN percentileDisc(n.property, 0.5)",
    octest:ct_string(Cypher_09),
    Cypher_10 = "MATCH (n:Person)
                 RETURN percentileCont(n.property, 0.4)",
    octest:ct_string(Cypher_10),
    Cypher_11 = "MATCH (n)
                 WHERE n.name IN ['A', 'B', 'C']
                 RETURN stdev(n.property)",
    octest:ct_string(Cypher_11),
    Cypher_12 = "MATCH (n)
                 WHERE n.name IN ['A', 'B', 'C']
                 RETURN stdevp(n.property)",
    octest:ct_string(Cypher_12),
    Cypher_13 = "MATCH (n:Person)
                 RETURN max(n.property)",
    octest:ct_string(Cypher_13),
    Cypher_14 = "MATCH (n:Person)
                 RETURN min(n.property)",
    octest:ct_string(Cypher_14),
    Cypher_15 = "MATCH (n:Person)
                 RETURN collect(n.property)",
    octest:ct_string(Cypher_15),
    Cypher_16 = "MATCH (a:Person { name: 'A' })-->(b)
                 RETURN count(DISTINCT b.eyes)",
    octest:ct_string(Cypher_16).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 9.1 Create
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test_neo4j_9_1_create(_Config) ->
    Cypher_01 = "CREATE (n)",
    octest:ct_string(Cypher_01),
    Cypher_02 = "CREATE (n),(m)",
    octest:ct_string(Cypher_02),
    Cypher_03 = "CREATE (n:Person)",
    octest:ct_string(Cypher_03),
    Cypher_04 = "CREATE (n:Person:Swedish)",
    octest:ct_string(Cypher_04),
    Cypher_05 = "CREATE (n:Person { name : 'Andres', title : 'Developer' })",
    octest:ct_string(Cypher_05),
    Cypher_06 = "CREATE (a { name : 'Andres' })
                 RETURN a",
    octest:ct_string(Cypher_06),
    Cypher_07 = "MATCH (a:Person),(b:Person)
                 WHERE a.name = 'Node A' AND b.name = 'Node B'
                 CREATE (a)-[r:RELTYPE]->(b)
                 RETURN r",
    octest:ct_string(Cypher_07),
    Cypher_08 = "MATCH (a:Person),(b:Person)
                 WHERE a.name = 'Node A' AND b.name = 'Node B'
                 CREATE (a)-[r:RELTYPE { name : a.name + '<->' + b.name }]->(b)
                 RETURN r",
    octest:ct_string(Cypher_08),
    Cypher_09 = "CREATE p =(andres { name:'Andres' })-[:WORKS_AT]->(neo)<-[:WORKS_AT]-(michael { name:'Michael' })
                 RETURN p",
    octest:ct_string(Cypher_09),
    Cypher_10 = "CREATE (n:Person $props)
                 RETURN n",
    % wwe ??? parameter
    octest:ct_string(Cypher_10),
    Cypher_11 = "UNWIND $props AS map
                 CREATE (n)
                 SET n = map",
    % wwe ??? parameter
    octest:ct_string(Cypher_11).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 9.2 Merge
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test_neo4j_9_2_merge(_Config) ->
    Cypher_01 = "MERGE (robert:Critic)
                 RETURN robert, labels(robert)",
    octest:ct_string(Cypher_01),
    Cypher_02 = "MERGE (charlie { name:'Charlie Sheen', age:10 })
                 RETURN charlie",
    octest:ct_string(Cypher_02),
    Cypher_03 = "MERGE (michael:Person { name:'Michael Douglas' })
                 RETURN michael.name, michael.bornIn",
    octest:ct_string(Cypher_03),
    Cypher_04 = "MATCH (person:Person)
                 MERGE (city:City { name: person.bornIn })
                 RETURN person.name, person.bornIn, city",
    octest:ct_string(Cypher_04),
    Cypher_05 = "MERGE (keanu:Person { name:'Keanu Reeves' })
                 ON CREATE SET keanu.created = timestamp()
                 RETURN keanu.name, keanu.created",
    octest:ct_string(Cypher_05),
    Cypher_06 = "MERGE (person:Person)
                 ON MATCH SET person.found = TRUE RETURN person.name, person.found",
    octest:ct_string(Cypher_06),
    Cypher_07 = "MERGE (keanu:Person { name:'Keanu Reeves' })
                 ON CREATE SET keanu.created = timestamp()
                 ON MATCH SET keanu.lastSeen = timestamp()
                 RETURN keanu.name, keanu.created, keanu.lastSeen",
    octest:ct_string(Cypher_07),
    Cypher_08 = "MERGE (person:Person)
                 ON MATCH SET person.found = TRUE , person.lastAccessed = timestamp()
                 RETURN person.name, person.found, person.lastAccessed",
    octest:ct_string(Cypher_08),
    Cypher_09 = "MATCH (charlie:Person { name:'Charlie Sheen' }),(wallStreet:Movie { title:'Wall Street' })
                 MERGE (charlie)-[r:ACTED_IN]->(wallStreet)
                 RETURN charlie.name, type(r), wallStreet.title",
    octest:ct_string(Cypher_09),
    Cypher_10 = "MATCH (oliver:Person { name:'Oliver Stone' }),(reiner:Person { name:'Rob Reiner' })
                 MERGE (oliver)-[:DIRECTED]->(movie:Movie)<-[:ACTED_IN]-(reiner)
                 RETURN movie",
    octest:ct_string(Cypher_10),
    Cypher_11 = "MATCH (charlie:Person { name:'Charlie Sheen' }),(oliver:Person { name:'Oliver Stone' })
                 MERGE (charlie)-[r:KNOWS]-(oliver)
                 RETURN r",
    octest:ct_string(Cypher_11),
    Cypher_12 = "MATCH (person:Person)
                 MERGE (city:City { name: person.bornIn })
                 MERGE (person)-[r:BORN_IN]->(city)
                 RETURN person.name, person.bornIn, city",
    octest:ct_string(Cypher_12),
    Cypher_13 = "MATCH (person:Person)
                 MERGE (person)-[r:HAS_CHAUFFEUR]->(chauffeur:Chauffeur { name: person.chauffeurName })
                 RETURN person.name, person.chauffeurName, chauffeur",
    octest:ct_string(Cypher_13),
    Cypher_15 = "MERGE (laurence:Person { name: 'Laurence Fishburne' })
                 RETURN laurence.name",
    octest:ct_string(Cypher_15),
    Cypher_16 = "MERGE (oliver:Person { name:'Oliver Stone' })
                 RETURN oliver.name, oliver.bornIn",
    octest:ct_string(Cypher_16),
    Cypher_17 = "MERGE (michael:Person { name:'Michael Douglas', role:'Gordon Gekko' })
                 RETURN michael",
    octest:ct_string(Cypher_17),
    Cypher_18 = "MERGE (oliver:Person { name:'Oliver Stone', role:'Gordon Gekko' })
                 RETURN oliver",
    octest:ct_string(Cypher_18),
    Cypher_19 = "MERGE (person:Person { name: $param.name, role: $param.role })
                 RETURN person.name, person.role",
    % wwe ??? parameter
    octest:ct_string(Cypher_19).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 9.3 Set
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test_neo4j_9_3_set(_Config) ->
    Cypher_01 = "MATCH (n { name: 'Andres' })
                 SET n.surname = 'Taylor'
                 RETURN n",
    octest:ct_string(Cypher_01),
    Cypher_02 = "MATCH (n { name: 'Andres' })
                 SET n.name = NULL RETURN n",
    octest:ct_string(Cypher_02),
    Cypher_03 = "MATCH (at { name: 'Andres' }),(pn { name: 'Peter' })
                 SET at = pn
                 RETURN at, pn",
    octest:ct_string(Cypher_03),
    Cypher_04 = "MATCH (peter { name: 'Peter' })
                 SET peter += { hungry: TRUE , position: 'Entrepreneur' }",
    octest:ct_string(Cypher_04),
    Cypher_05 = "MATCH (n { name: 'Andres' })
                 SET n.surname = $surname
                 RETURN n",
    % wwe ??? parameter
    octest:ct_string(Cypher_05),
    Cypher_06 = "MATCH (n { name: 'Andres' })
                 SET n = $props
                 RETURN n",
    % wwe ??? parameter
    octest:ct_string(Cypher_06),
    Cypher_07 = "MATCH (n { name: 'Andres' })
                 SET n.position = 'Developer', n.surname = 'Taylor'",
    octest:ct_string(Cypher_07),
    Cypher_08 = "MATCH (n { name: 'Stefan' })
                 SET n :German
                 RETURN n",
    octest:ct_string(Cypher_08),
    Cypher_09 = "MATCH (n { name: 'Emil' })
                 SET n :Swedish:Bossman
                 RETURN n",
    octest:ct_string(Cypher_09).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 9.4 Delete
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test_neo4j_9_4_delete(_Config) ->
    Cypher_01 = "MATCH (n:Useless)
                 DELETE n",
    octest:ct_string(Cypher_01),
    Cypher_02 = "MATCH (n)
                 DETACH DELETE n",
    octest:ct_string(Cypher_02),
    Cypher_03 = "MATCH (n { name:'Andres' })
                 DETACH DELETE n",
    octest:ct_string(Cypher_03).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 9.5 Remove
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test_neo4j_9_5_remove(_Config) ->
    Cypher_01 = "MATCH (andres { name: 'Andres' })
                 REMOVE andres.age
                 RETURN andres",
    octest:ct_string(Cypher_01),
    Cypher_02 = "MATCH (n { name: 'Peter' })
                 REMOVE n:German
                 RETURN n",
    octest:ct_string(Cypher_02),
    Cypher_03 = "MATCH (n { name: 'Peter' })
                 REMOVE n:German:Swedish
                 RETURN n",
    octest:ct_string(Cypher_03).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 10.1 Predicates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test_neo4j_10_1_predicates(_Config) ->
    Cypher_01 = "MATCH p=(a)-[*1..3]->(b)
                 WHERE a.name='Alice' AND b.name='Daniel' AND ALL (x IN nodes(p) WHERE x.age > 30)
                 RETURN p",
    octest:ct_string(Cypher_01),
    Cypher_02 = "MATCH (a)
                 WHERE a.name='Eskil' AND ANY (x IN a.array WHERE x = \"one\")
                 RETURN a",
    octest:ct_string(Cypher_02),
    Cypher_03 = "MATCH p=(n)-[*1..3]->(b)
                 WHERE n.name='Alice' AND NONE (x IN nodes(p) WHERE x.age = 25)
                 RETURN p",
    octest:ct_string(Cypher_03),
    Cypher_04 = "MATCH p=(n)-->(b)
                 WHERE n.name='Alice' AND SINGLE (var IN nodes(p) WHERE var.eyes = \"blue\")
                 RETURN p",
    octest:ct_string(Cypher_04),
    Cypher_05 = "MATCH (n)
                 WHERE EXISTS(n.name)
                 RETURN n.name AS name, EXISTS((n)-[:MARRIED]->()) AS is_married",
    octest:ct_string(Cypher_05).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 10.2 Scalar Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test_neo4j_10_2_scalar_functions(_Config) ->

    Cypher_01 = "RETURN size(['Alice', 'Bob']) AS col",
    octest:ct_string(Cypher_01),
    Cypher_02 = "MATCH (a)
                 WHERE a.name='Alice'
                 RETURN size((a)-->()-->()) AS fof",
    octest:ct_string(Cypher_02),
    Cypher_03 = "MATCH p=(a)-->(b)-->(c)
                 WHERE a.name='Alice'
                 RETURN length(p)",
    octest:ct_string(Cypher_03),
    Cypher_04 = "MATCH (a)
                 WHERE length(a.name)> 6
                 RETURN length(a.name)",
    octest:ct_string(Cypher_04),
    Cypher_05 = "MATCH (n)-[r]->()
                 WHERE n.name='Alice'
                 RETURN type(r)",
    octest:ct_string(Cypher_05),
    Cypher_06 = "MATCH (a)
                 RETURN id(a)",
    octest:ct_string(Cypher_06),
    Cypher_07 = "MATCH (a)
                WHERE a.name='Alice'
                RETURN coalesce(a.hairColor, a.eyes)",
    octest:ct_string(Cypher_07),
    Cypher_08 = "MATCH (a)
                 WHERE a.name='Eskil'
                 RETURN a.array, head(a.array)",
    octest:ct_string(Cypher_08),
    Cypher_09 = "MATCH (a)
                 WHERE a.name='Eskil'
                 RETURN a.array, last(a.array)",
    octest:ct_string(Cypher_09),
    Cypher_10 = "RETURN timestamp()",
    octest:ct_string(Cypher_10),
    Cypher_11 = "MATCH (x:foo)-[r]-()
                 RETURN startNode(r)",
    octest:ct_string(Cypher_11),
    Cypher_12 = "MATCH (x:foo)-[r]-()
                 RETURN endNode(r)",
    octest:ct_string(Cypher_12),
    Cypher_13 = "CREATE (p:Person { name: 'Stefan', city: 'Berlin' })
                 RETURN properties(p)",
    octest:ct_string(Cypher_13),
    Cypher_14 = "RETURN toInt(\"42\"), toInt(\"not a number\")",
    octest:ct_string(Cypher_14),
    Cypher_15 = "RETURN toFloat(\"11.5\"), toFloat(\"not a number\")",
    octest:ct_string(Cypher_15).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 10.3 List Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test_neo4j_10_3_list_functions(_Config) ->
    Cypher_01 = "MATCH p=(a)-->(b)-->(c)
                 WHERE a.name='Alice' AND c.name='Eskil'
                 RETURN nodes(p)",
    octest:ct_string(Cypher_01),
    Cypher_02 = "MATCH p=(a)-->(b)-->(c)
                 WHERE a.name='Alice' AND c.name='Eskil'
                 RETURN relationships(p)",
    octest:ct_string(Cypher_02),
    Cypher_03 = "MATCH (a)
                 WHERE a.name='Alice'
                 RETURN labels(a)",
    octest:ct_string(Cypher_03),
    Cypher_04 = "MATCH (a)
                 WHERE a.name='Alice'
                 RETURN keys(a)",
    octest:ct_string(Cypher_04),
    Cypher_05 = "MATCH p=(a)-->(b)-->(c)
                 WHERE a.name='Alice' AND b.name='Bob' AND c.name='Daniel'
                 RETURN extract(n IN nodes(p)| n.age) AS extracted",
    octest:ct_string(Cypher_05),
    Cypher_06 = "MATCH (a)
                 WHERE a.name='Eskil'
                 RETURN a.array, filter(x IN a.array WHERE size(x)= 3)",
    octest:ct_string(Cypher_06),
    Cypher_07 = "MATCH (a)
                 WHERE a.name='Eskil'
                 RETURN a.array, tail(a.array)",
    octest:ct_string(Cypher_07),
    Cypher_08 = "RETURN range(0,10), range(2,18,3)",
    octest:ct_string(Cypher_08).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 10.4 Math functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test_neo4j_10_4_math_functions(_Config) ->
    Cypher_01 = "MATCH (a),(e)
                 WHERE a.name = 'Alice' AND e.name = 'Eskil'
                 RETURN a.age, e.age, abs(a.age - e.age)",
    octest:ct_string(Cypher_01),
    Cypher_02 = "RETURN ceil(0.1)",
    octest:ct_string(Cypher_02),
    Cypher_03 = "RETURN floor(0.9)",
    octest:ct_string(Cypher_03),
    Cypher_04 = "RETURN round(3.141592)",
    octest:ct_string(Cypher_04),
    Cypher_05 = "RETURN sign(-17), sign(0.1)",
    octest:ct_string(Cypher_05),
    Cypher_06 = "RETURN rand()",
    octest:ct_string(Cypher_06),
    Cypher_07 = "RETURN log(27)",
    octest:ct_string(Cypher_07),
    Cypher_08 = "RETURN log10(27)",
    octest:ct_string(Cypher_08),
    Cypher_09 = "RETURN exp(2)",
    octest:ct_string(Cypher_09),
    Cypher_10 = "RETURN e()",
    octest:ct_string(Cypher_10),
    Cypher_11 = "RETURN sqrt(256)",
    octest:ct_string(Cypher_11),
    Cypher_12 = "RETURN sin(0.5)",
    octest:ct_string(Cypher_12),
    Cypher_13 = "RETURN cos(0.5)",
    octest:ct_string(Cypher_13),
    Cypher_14 = "RETURN tan(0.5)",
    octest:ct_string(Cypher_14),
    Cypher_15 = "RETURN cot(0.5)",
    octest:ct_string(Cypher_15),
    Cypher_16 = "RETURN asin(0.5)",
    octest:ct_string(Cypher_16),
    Cypher_17 = "RETURN acos(0.5)",
    octest:ct_string(Cypher_17),
    Cypher_18 = "RETURN atan(0.5)",
    octest:ct_string(Cypher_18),
    Cypher_19 = "RETURN atan2(0.5, 0.6)",
    octest:ct_string(Cypher_19),
    Cypher_20 = "RETURN pi()",
    octest:ct_string(Cypher_20),
    Cypher_21 = "RETURN degrees(3.14159)",
    octest:ct_string(Cypher_21),
    Cypher_22 = "RETURN radians(180)",
    octest:ct_string(Cypher_22),
    Cypher_23 = "RETURN haversin(0.5)",
    octest:ct_string(Cypher_23),
    Cypher_24 = "CREATE (ber:City { lat: 52.5, lon: 13.4 }),(sm:City { lat: 37.5, lon: -122.3 })
                 RETURN 2 * 6371 * asin(sqrt(haversin(radians(sm.lat - ber.lat))+ cos(radians(sm.lat))*
                 cos(radians(ber.lat))* haversin(radians(sm.lon - ber.lon)))) AS dist",
    octest:ct_string(Cypher_24).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 10.5 String functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test_neo4j_10_5_string_functions(_Config) ->
    Cypher_01 = "RETURN replace(\"hello\", \"l\", \"w\")",
    octest:ct_string(Cypher_01),
    Cypher_02 = "RETURN substring(\"hello\", 1, 3), substring(\"hello\", 2)",
    octest:ct_string(Cypher_02),
    Cypher_03 = "RETURN left(\"hello\", 3)",
    octest:ct_string(Cypher_03),
    Cypher_04 = "RETURN right(\"hello\", 3)",
    octest:ct_string(Cypher_04),
    Cypher_05 = "RETURN ltrim(\" hello\")",
    octest:ct_string(Cypher_05),
    Cypher_06 = "RETURN rtrim(\"hello \")",
    octest:ct_string(Cypher_06),
    Cypher_07 = "RETURN trim(\" hello \")",
    octest:ct_string(Cypher_07),
    Cypher_08 = "RETURN lower(\"HELLO\")",
    octest:ct_string(Cypher_08),
    Cypher_09 = "RETURN upper(\"hello\")",
    octest:ct_string(Cypher_09),
    Cypher_10 = "RETURN split(\"one,two\", \",\")",
    octest:ct_string(Cypher_10),
    Cypher_11 = "RETURN reverse(\"anagram\")",
    octest:ct_string(Cypher_11),
    Cypher_12 = "RETURN toString(11.5), toString(\"already a string\"), toString(TRUE )",
    octest:ct_string(Cypher_12).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 11.1 Indexes
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test_neo4j_11_1_indexes(_Config) ->
    Cypher_03 = "MATCH (person:Person { name: 'Andres' })
                 RETURN person",
    octest:ct_string(Cypher_03),
    Cypher_04 = "MATCH (person:Person)
                 WHERE person.name = 'Andres'
                 RETURN person",
    octest:ct_string(Cypher_04),
    Cypher_05 = "MATCH (person:Person)
                 WHERE person.name > 'B'
                 RETURN person",
    octest:ct_string(Cypher_05),
    Cypher_06 = "MATCH (person:Person)
                 WHERE person.name IN ['Andres', 'Mark']
                 RETURN person",
    octest:ct_string(Cypher_06),
    Cypher_07 = "MATCH (person:Person)
                 WHERE person.name STARTS WITH 'And'
                 RETURN person",
    octest:ct_string(Cypher_07),
    Cypher_08 = "MATCH (p:Person)
                 WHERE exists(p.name)
                 RETURN p",
    octest:ct_string(Cypher_08).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 11.2 Constraints
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test_neo4j_11_2_constraints(_Config) ->
    Cypher_03 = "CREATE (book:Book { isbn: '1449356265', title: 'Graph Databases' })",
    octest:ct_string(Cypher_03),
    Cypher_04 = "CREATE (book:Book { isbn: '1449356265', title: 'Graph Databases' })",
    octest:ct_string(Cypher_04),
    Cypher_08 = "CREATE (book:Book { isbn: '1449356265', title: 'Graph Databases' })",
    octest:ct_string(Cypher_08),
    Cypher_09 = "CREATE (book:Book { title: 'Graph Databases' })",
    octest:ct_string(Cypher_09),
    Cypher_10 = "MATCH (book:Book { title: 'Graph Databases' })
                 REMOVE book.isbn",
    octest:ct_string(Cypher_10),
    Cypher_14 = "CREATE (user:User)-[like:LIKED { day: 'yesterday' }]->(book:Book)",
    octest:ct_string(Cypher_14),
    Cypher_15 = "CREATE (user:User)-[like:LIKED]->(book:Book)",
    octest:ct_string(Cypher_15),
    Cypher_16 = "MATCH (user:User)-[like:LIKED]->(book:Book)
                 REMOVE like.day",
    octest:ct_string(Cypher_16).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 12.3 Basis query tuning
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test_neo4j_12_3_basic_query_tuning(_Config) ->
    Cypher_04 = "MATCH (p { name:\"Tom Hanks\" })
                 RETURN p",
    octest:ct_string(Cypher_04),
    Cypher_06 = "MATCH (p:Person { name:\"Tom Hanks\" })
                 RETURN p",
    octest:ct_string(Cypher_06),
    Cypher_09 = "MATCH (p:Person { name:\"Tom Hanks\" })
                 RETURN p",
    octest:ct_string(Cypher_09).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 12.4 Delete
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test_neo4j_12_4_using(_Config) ->
    Cypher_01 = "MATCH (liskov:Scientist { name:'Liskov' })-[:KNOWS]->(wing:Scientist)-[:RESEARCHED]->(cs:Science {
                 name:'Computer Science' })<-[:RESEARCHED]-(conway:Scientist { name: 'Conway' })
                 RETURN 1 AS column",
    octest:ct_string(Cypher_01).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 13.1 Starting point operators
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test_neo4j_13_1_starting_point(_Config) ->
    Cypher_01 = "MATCH (n)
                 RETURN n",
    octest:ct_string(Cypher_01),
    Cypher_02 = "MATCH (n1)-[r]->()
                 WHERE id(r)= 0
                 RETURN r, n1",
    octest:ct_string(Cypher_02),
    Cypher_03 = "MATCH (n)
                 WHERE id(n)= 0
                 RETURN n",
    octest:ct_string(Cypher_03),
    Cypher_04 = "MATCH (person:Person)
                 RETURN person",
    octest:ct_string(Cypher_04),
    Cypher_05 = "MATCH (location:Location { name: \"Malmo\" })
                 RETURN location",
    octest:ct_string(Cypher_05),
    Cypher_06 = "MATCH (l:Location)
                 WHERE l.name STARTS WITH 'Lon'
                 RETURN l",
    octest:ct_string(Cypher_06),
    Cypher_07 = "MATCH (l:Location)
                 WHERE l.name CONTAINS 'al'
                 RETURN l",
    octest:ct_string(Cypher_07),
    Cypher_08 = "MATCH (l:Location)
                 WHERE exists(l.name)
                 RETURN l",
    octest:ct_string(Cypher_08),
    Cypher_09 = "MATCH (n1)-[r]-()
                 WHERE id(r)= 1
                 RETURN r, n1",
    octest:ct_string(Cypher_09).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 13.2 Expand
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test_neo4j_13_2_expand(_Config) ->
    Cypher_01 = "MATCH (p:Person { name: \"me\" })-[:FRIENDS_WITH]->(fof)
                 RETURN fof",
    octest:ct_string(Cypher_01),
    Cypher_02 = "MATCH (p:Person { name: \"me\" })-[:FRIENDS_WITH]->(fof)-->(p)
                 RETURN fof",
    octest:ct_string(Cypher_02),
    Cypher_03 = "MATCH (p:Person)
                 OPTIONAL MATCH (p)-[works_in:WORKS_IN]->(l)
                 WHERE works_in.duration > 180
                 RETURN p, l",
    octest:ct_string(Cypher_03),
    Cypher_04 = "MATCH (p:Person)-[:FRIENDS_WITH]->(f)
                 WITH p, count(f) AS fs
                 WHERE fs > 2
                 OPTIONAL MATCH (p)-[:WORKS_IN]->(city)
                 RETURN city.name",
    octest:ct_string(Cypher_04),
    Cypher_05 = "MATCH (p:Person)
                 WHERE (p)-[:FRIENDS_WITH]->()
                 RETURN p.name",
    octest:ct_string(Cypher_05),
    Cypher_06 = "MATCH (me:Person { name: \"me\" }),(other:Person)
                 WHERE NOT (me)-[:FRIENDS_WITH]->(other)
                 RETURN other.name",
    octest:ct_string(Cypher_06),
    Cypher_07 = "MATCH (other:Person)
                 WHERE (other)-[:FRIENDS_WITH]->() OR (other)-[:WORKS_IN]->()
                 RETURN other.name",
    octest:ct_string(Cypher_07),
    Cypher_08 = "MATCH (other:Person)
                 WHERE NOT (other)-[:FRIENDS_WITH]->() OR (other)-[:WORKS_IN]->()
                 RETURN other.name",
    % wwe ??? parenthesized expression
    octest:ct_string(Cypher_08),
    Cypher_09 = "MATCH (other:Person)
                 WHERE other.age > 25 OR (other)-[:FRIENDS_WITH]->()
                 RETURN other.name",
    octest:ct_string(Cypher_09),
    Cypher_10 = "MATCH (other:Person)
                 WHERE other.age > 25 OR NOT (other)-[:FRIENDS_WITH]->()
                 RETURN other.name",
    octest:ct_string(Cypher_10),
    Cypher_11 = "MERGE (p:Person { name: 'Andres' })
                 ON MATCH SET p.exists_4711 = TRUE",
    octest:ct_string(Cypher_11),
    Cypher_12 = "MERGE (p:Person { name: 'Andres' })
                 ON MATCH SET p.exists_4711 = TRUE",
    octest:ct_string(Cypher_12),
    Cypher_13 = "MERGE (t:Team { name: 'Engineering', id: 42 })",
    octest:ct_string(Cypher_13),
    Cypher_14 = "MATCH (andy:Person { name:'Andreas' })-[:WORKS_IN]->(loc)<-[:WORKS_IN]-(matt:Person { name:'Mattis' })
                 RETURN loc.name",
    octest:ct_string(Cypher_14),
    Cypher_15 = "MATCH (me:Person)-[:FRIENDS_WITH]-()-[:FRIENDS_WITH]-(other)
                 WHERE NOT (me)-[:FRIENDS_WITH]-(other)
                 RETURN other.name",
    octest:ct_string(Cypher_15).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 13.4 Row operators
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test_neo4j_13_4_row_operators(_Config) ->
    Cypher_01 = "MATCH (a)-[r]-(b)
                 DELETE r,a,b
                 MERGE ()",
    octest:ct_string(Cypher_01),
    Cypher_02 = "MATCH (l:Location)<-[:WORKS_IN]-(p:Person)
                 RETURN DISTINCT l",
    octest:ct_string(Cypher_02),
    Cypher_03 = "MATCH (l:Location)<-[:WORKS_IN]-(p:Person)
                 RETURN l.name AS location, COLLECT(p.name) AS people",
    octest:ct_string(Cypher_03),
    Cypher_04 = "MATCH (p:Person)
                 RETURN count(p) AS people",
    octest:ct_string(Cypher_04),
    Cypher_05 = "MATCH (p:Person)-[r:WORKS_IN]->()
                 RETURN count(r) AS jobs",
    octest:ct_string(Cypher_05),
    Cypher_06 = "MATCH (p:Person)
                 WHERE p.name =~ \"^a.*\"
                 RETURN p",
    octest:ct_string(Cypher_06),
    Cypher_07 = "MATCH (p:Person)
                 RETURN p
                 LIMIT 3",
    octest:ct_string(Cypher_07),
    Cypher_08 = "RETURN \"hello\" AS greeting",
    octest:ct_string(Cypher_08),
    Cypher_09 = "MATCH (p:Person)
                 RETURN p
                 ORDER BY p.id
                 SKIP 1",
    octest:ct_string(Cypher_09),
    Cypher_10 = "MATCH (p:Person)
                 RETURN p
                 ORDER BY p.name",
    octest:ct_string(Cypher_10),
    Cypher_11 = "MATCH (p:Person)
                 RETURN p
                 ORDER BY p.name
                 LIMIT 2",
    octest:ct_string(Cypher_11),
    Cypher_12 = "MATCH (p:Location)
                 RETURN p.name
                 UNION ALL MATCH (p:Country)
                 RETURN p.name",
    octest:ct_string(Cypher_12),
    Cypher_13 = "UNWIND range(1,5) AS value
                 RETURN value;",
    octest:ct_string(Cypher_13),
    _Cypher_14 = "CALL db.labels() YIELD label
                 RETURN *
                 ORDER BY label".
%   octest:ct_string(Cypher_14).                                                % not supported by openCypher

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 13.5 Update operators
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test_neo4j_13_5_update_operators(_Config) ->
    Cypher_02 = "CREATE (:Person)",
    octest:ct_string(Cypher_02).
