%%%-------------------------------------------------------------------
%%% File        : performance_compacted_referenceExamples_SUITE.erl
%%% Description : Test Suite for rule: referenceExamples.
%%%
%%% Created     : 27.11.2017
%%%-------------------------------------------------------------------
-module(performance_compacted_referenceExamples_SUITE).

-export([
    all/0,
    end_per_suite/1,
    init_per_suite/1,
    suite/0,
    test_compacted/1
]).

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
%% COMMON TEST CALLBACK FUNCTIONS - ALL
%%--------------------------------------------------------------------

all() ->
    [
        test_compacted
    ].

%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

test_compacted(_Config) ->
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 4. Get started with Cypher
        //      section:    4.1. Patterns
        //      subsection: 4.1.1. Node syntax
        // ---------------------------------------------------------------------
Create () --
        ()
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 4. Get started with Cypher
        //      section:    4.1. Patterns
        //      subsection: 4.1.1. Node syntax
        // ---------------------------------------------------------------------
Create () --
        (matrix)
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 4. Get started with Cypher
        //      section:    4.1. Patterns
        //      subsection: 4.1.1. Node syntax
        // ---------------------------------------------------------------------
Create () --
        (:Movie)
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 4. Get started with Cypher
        //      section:    4.1. Patterns
        //      subsection: 4.1.1. Node syntax
        // ---------------------------------------------------------------------
Create () --
        (matrix:Movie)
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 4. Get started with Cypher
        //      section:    4.1. Patterns
        //      subsection: 4.1.1. Node syntax
        // ---------------------------------------------------------------------
Create () --
        (matrix:Movie {title: \"The Matrix\"})
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 4. Get started with Cypher
        //      section:    4.1. Patterns
        //      subsection: 4.1.1. Node syntax
        // ---------------------------------------------------------------------
Create () --
        (matrix:Movie {title: \"The Matrix\", released: 1997})
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 4. Get started with Cypher
        //      section:    4.1. Patterns
        //      subsection: 4.1.2. Relationship syntax
        // ---------------------------------------------------------------------
Create ()
        -->
    ()"),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 4. Get started with Cypher
        //      section:    4.1. Patterns
        //      subsection: 4.1.2. Relationship syntax
        // ---------------------------------------------------------------------
Create ()
        -[role]->
    ()"),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 4. Get started with Cypher
        //      section:    4.1. Patterns
        //      subsection: 4.1.2. Relationship syntax
        // ---------------------------------------------------------------------
Create ()
        -[:ACTED_IN]->
    ()"),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 4. Get started with Cypher
        //      section:    4.1. Patterns
        //      subsection: 4.1.2. Relationship syntax
        // ---------------------------------------------------------------------
Create ()
        -[role:ACTED_IN]->
    ()"),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 4. Get started with Cypher
        //      section:    4.1. Patterns
        //      subsection: 4.1.2. Relationship syntax
        // ---------------------------------------------------------------------
Create ()
        -[role:ACTED_IN {roles: [\"Neo\"]}]->
    ()"),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 4. Get started with Cypher
        //      section:    4.1. Patterns
        //      subsection: 4.1.3. Pattern syntax
        // ---------------------------------------------------------------------
Create
        (keanu:Person:Actor {name: \"Keanu Reeves\"} )
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 4. Get started with Cypher
        //      section:    4.1. Patterns
        //      subsection: 4.1.3. Pattern syntax
        // ---------------------------------------------------------------------
Create ()
        -[role:ACTED_IN {roles: [\"Neo\"] } ]->
    ()"),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 4. Get started with Cypher
        //      section:    4.1. Patterns
        //      subsection: 4.1.3. Pattern syntax
        // ---------------------------------------------------------------------
Create
        (matrix:Movie {title: \"The Matrix\"} )
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 4. Get started with Cypher
        //      section:    4.1. Patterns
        //      subsection: 4.1.4. Pattern variables
        // ---------------------------------------------------------------------
Create
        acted_in = (:Person)-[:ACTED_IN]->(:Movie)
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 4. Get started with Cypher
        //      section:    4.2. Patterns in practice
        //      subsection: 4.2.1. Creating data
        // ---------------------------------------------------------------------

        CREATE (:Movie { title:\"The Matrix\",released:1997 })
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 4. Get started with Cypher
        //      section:    4.2. Patterns in practice
        //      subsection: 4.2.1. Creating data
        // ---------------------------------------------------------------------

        CREATE (p:Person { name:\"Keanu Reeves\", born:1964 })
        RETURN p
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 4. Get started with Cypher
        //      section:    4.2. Patterns in practice
        //      subsection: 4.2.1. Creating data
        // ---------------------------------------------------------------------

        CREATE (a:Person { name:\"Tom Hanks\",
          born:1956 })-[r:ACTED_IN { roles: [\"Forrest\"]}]->(m:Movie { title:\"Forrest Gump\",released:1994 })
        CREATE (d:Person { name:\"Robert Zemeckis\", born:1951 })-[:DIRECTED]->(m)
        RETURN a,d,r,m
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 4. Get started with Cypher
        //      section:    4.2. Patterns in practice
        //      subsection: 4.2.2. Matching patterns
        // ---------------------------------------------------------------------

        MATCH (m:Movie)
        RETURN m
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 4. Get started with Cypher
        //      section:    4.2. Patterns in practice
        //      subsection: 4.2.2. Matching patterns
        // ---------------------------------------------------------------------

        MATCH (p:Person { name:\"Keanu Reeves\" })
        RETURN p
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 4. Get started with Cypher
        //      section:    4.2. Patterns in practice
        //      subsection: 4.2.2. Matching patterns
        // ---------------------------------------------------------------------

        MATCH (p:Person { name:\"Tom Hanks\" })-[r:ACTED_IN]->(m:Movie)
        RETURN m.title, r.roles
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 4. Get started with Cypher
        //      section:    4.2. Patterns in practice
        //      subsection: 4.2.3. Attaching structures
        // ---------------------------------------------------------------------

        MATCH (p:Person { name:\"Tom Hanks\" })
        CREATE (m:Movie { title:\"Cloud Atlas\",released:2012 })
        CREATE (p)-[r:ACTED_IN { roles: ['Zachry']}]->(m)
        RETURN p,r,m
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 4. Get started with Cypher
        //      section:    4.2. Patterns in practice
        //      subsection: 4.2.4. Completing patterns
        // ---------------------------------------------------------------------

        MERGE (m:Movie { title:\"Cloud Atlas\" })
        ON CREATE SET m.released = 2012
        RETURN m
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 4. Get started with Cypher
        //      section:    4.2. Patterns in practice
        //      subsection: 4.2.4. Completing patterns
        // ---------------------------------------------------------------------

        MATCH (m:Movie { title:\"Cloud Atlas\" })
        MATCH (p:Person { name:\"Tom Hanks\" })
        MERGE (p)-[r:ACTED_IN]->(m)
        ON CREATE SET r.roles =['Zachry']
        RETURN p,r,m
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 4. Get started with Cypher
        //      section:    4.2. Patterns in practice
        //      subsection: 4.2.4. Completing patterns
        // ---------------------------------------------------------------------

        CREATE (y:Year { year:2014 })
        MERGE (y)<-[:IN_YEAR]-(m10:Month { month:10 })
        MERGE (y)<-[:IN_YEAR]-(m11:Month { month:11 })
        RETURN y,m10,m11
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 4. Get started with Cypher
        //      section:    4.3. Getting correct results
        // ---------------------------------------------------------------------

        CREATE (matrix:Movie { title:\"The Matrix\",released:1997 })
        CREATE (cloudAtlas:Movie { title:\"Cloud Atlas\",released:2012 })
        CREATE (forrestGump:Movie { title:\"Forrest Gump\",released:1994 })
        CREATE (keanu:Person { name:\"Keanu Reeves\", born:1964 })
        CREATE (robert:Person { name:\"Robert Zemeckis\", born:1951 })
        CREATE (tom:Person { name:\"Tom Hanks\", born:1956 })
        CREATE (tom)-[:ACTED_IN { roles: [\"Forrest\"]}]->(forrestGump)
        CREATE (tom)-[:ACTED_IN { roles: ['Zachry']}]->(cloudAtlas)
        CREATE (robert)-[:DIRECTED]->(forrestGump)
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 4. Get started with Cypher
        //      section:    4.3. Getting correct results
        //      subsection: 4.3.1. Filtering results
        // ---------------------------------------------------------------------

        MATCH (m:Movie)
        WHERE m.title = \"The Matrix\"
        RETURN m
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 4. Get started with Cypher
        //      section:    4.3. Getting correct results
        //      subsection: 4.3.1. Filtering results
        // ---------------------------------------------------------------------

        MATCH (p:Person)-[r:ACTED_IN]->(m:Movie)
        WHERE p.name =~ \"K.+\" OR m.released > 2000 OR \"Neo\" IN r.roles
        RETURN p,r,m
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 4. Get started with Cypher
        //      section:    4.3. Getting correct results
        //      subsection: 4.3.1. Filtering results
        // ---------------------------------------------------------------------

        MATCH (p:Person)-[:ACTED_IN]->(m)
        WHERE NOT (p)-[:DIRECTED]->()
        RETURN p,m
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 4. Get started with Cypher
        //      section:    4.3. Getting correct results
        //      subsection: 4.3.2. Returning results
        // ---------------------------------------------------------------------

        MATCH (p:Person)
        RETURN p, p.name AS name, toUpper(p.name), coalesce(p.nickname,\"n/a\") AS nickname, { name: p.name,
          label:head(labels(p))} AS person
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 4. Get started with Cypher
        //      section:    4.3. Getting correct results
        //      subsection: 4.3.3. Aggregating information
        // ---------------------------------------------------------------------

        MATCH (:Person)
        RETURN count(*) AS people
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 4. Get started with Cypher
        //      section:    4.3. Getting correct results
        //      subsection: 4.3.3. Aggregating information
        // ---------------------------------------------------------------------

        MATCH (actor:Person)-[:ACTED_IN]->(movie:Movie)<-[:DIRECTED]-(director:Person)
        RETURN actor,director,count(*) AS collaborations
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 4. Get started with Cypher
        //      section:    4.3. Getting correct results
        //      subsection: 4.3.4. Ordering and pagination
        // ---------------------------------------------------------------------

        MATCH (a:Person)-[:ACTED_IN]->(m:Movie)
        RETURN a,count(*) AS appearances
        ORDER BY appearances DESC LIMIT 10;
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 4. Get started with Cypher
        //      section:    4.3. Getting correct results
        //      subsection: 4.3.5. Collecting aggregation
        // ---------------------------------------------------------------------

        MATCH (m:Movie)<-[:ACTED_IN]-(a:Person)
        RETURN m.title AS movie, collect(a.name) AS cast, count(*) AS actors
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 4. Get started with Cypher
        //      section:    4.4. Composing large statements
        // ---------------------------------------------------------------------

        CREATE (matrix:Movie { title:\"The Matrix\",released:1997 })
        CREATE (cloudAtlas:Movie { title:\"Cloud Atlas\",released:2012 })
        CREATE (forrestGump:Movie { title:\"Forrest Gump\",released:1994 })
        CREATE (keanu:Person { name:\"Keanu Reeves\", born:1964 })
        CREATE (robert:Person { name:\"Robert Zemeckis\", born:1951 })
        CREATE (tom:Person { name:\"Tom Hanks\", born:1956 })
        CREATE (tom)-[:ACTED_IN { roles: [\"Forrest\"]}]->(forrestGump)
        CREATE (tom)-[:ACTED_IN { roles: ['Zachry']}]->(cloudAtlas)
        CREATE (robert)-[:DIRECTED]->(forrestGump)
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 4. Get started with Cypher
        //      section:    4.4. Composing large statements
        //      subsection: 4.4.1. UNION
        // ---------------------------------------------------------------------

        MATCH (actor:Person)-[r:ACTED_IN]->(movie:Movie)
        RETURN actor.name AS name, type(r) AS acted_in, movie.title AS title
        UNION
        MATCH (director:Person)-[r:DIRECTED]->(movie:Movie)
        RETURN director.name AS name, type(r) AS acted_in, movie.title AS title
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 4. Get started with Cypher
        //      section:    4.4. Composing large statements
        //      subsection: 4.4.2. WITH
        // ---------------------------------------------------------------------

        MATCH (person:Person)-[:ACTED_IN]->(m:Movie)
        WITH person, count(*) AS appearances, collect(m.title) AS movies
        WHERE appearances > 1
        RETURN person.name, appearances, movies
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 4. Get started with Cypher
        //      section:    4.5. Constraints and indexes
        //      subsection: 4.5.2. Using indexes
        // ---------------------------------------------------------------------

        CREATE (actor:Actor { name:\"Tom Hanks\" }),(movie:Movie { title:'Sleepless IN Seattle' }),
          (actor)-[:ACTED_IN]->(movie);
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 4. Get started with Cypher
        //      section:    4.5. Constraints and indexes
        //      subsection: 4.5.2. Using indexes
        // ---------------------------------------------------------------------

        MATCH (actor:Actor { name: \"Tom Hanks\" })
        RETURN actor;
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 5. Introduction
        //      section:    5.1. What is Cypher?
        // ---------------------------------------------------------------------

        MATCH (john {name: 'John'})-[:friend]->()-[:friend]->(fof)
        RETURN john.name, fof.name
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 5. Introduction
        //      section:    5.1. What is Cypher?
        // ---------------------------------------------------------------------

        MATCH (user)-[:friend]->(follower)
        WHERE user.name IN ['Joe', 'John', 'Sara', 'Maria', 'Steve'] AND follower.name =~ 'S.*'
        RETURN user.name, follower.name
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 5. Introduction
        //      section:    5.2. Querying and updating the graph
        //      subsection: 5.2.1. The structure of update queries
        // ---------------------------------------------------------------------

        MATCH (n {name: 'John'})-[:FRIEND]-(friend)
        WITH n, count(friend) AS friendsCount
        WHERE friendsCount > 3
        RETURN n, friendsCount
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 5. Introduction
        //      section:    5.4. Uniqueness
        // ---------------------------------------------------------------------

        CREATE (adam:User { name: 'Adam' }),(pernilla:User { name: 'Pernilla' }),(david:User { name: 'David'
          }),
          (adam)-[:FRIEND]->(pernilla),(pernilla)-[:FRIEND]->(david)
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 5. Introduction
        //      section:    5.4. Uniqueness
        // ---------------------------------------------------------------------

        MATCH (user:User { name: 'Adam' })-[r1:FRIEND]-()-[r2:FRIEND]-(friend_of_a_friend)
        RETURN friend_of_a_friend.name AS fofName
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 5. Introduction
        //      section:    5.4. Uniqueness
        // ---------------------------------------------------------------------

        MATCH (user:User { name: 'Adam' })-[r1:FRIEND]-(friend)
        MATCH (friend)-[r2:FRIEND]-(friend_of_a_friend)
        RETURN friend_of_a_friend.name AS fofName
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 5. Introduction
        //      section:    5.4. Uniqueness
        // ---------------------------------------------------------------------

        MATCH (user:User { name: 'Adam' })-[r1:FRIEND]-(friend),(friend)-[r2:FRIEND]-(friend_of_a_friend)
        RETURN friend_of_a_friend.name AS fofName
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 6. Syntax
        //      section:    6.2. Expressions
        //      subsection: 6.2.3. CASE expressions
        // ---------------------------------------------------------------------

        MATCH (n)
        RETURN
        CASE n.eyes
        WHEN 'blue'
        THEN 1
        WHEN 'brown'
        THEN 2
        ELSE 3 END AS result
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 6. Syntax
        //      section:    6.2. Expressions
        //      subsection: 6.2.3. CASE expressions
        // ---------------------------------------------------------------------

        MATCH (n)
        RETURN
        CASE
        WHEN n.eyes = 'blue'
        THEN 1
        WHEN n.age < 40
        THEN 2
        ELSE 3 END AS result
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 6. Syntax
        //      section:    6.2. Expressions
        //      subsection: 6.2.3. CASE expressions
        // ---------------------------------------------------------------------

        MATCH (n)
        RETURN n.name,
        CASE n.age
        WHEN n.age IS NULL THEN -1
        ELSE n.age - 10 END AS age_10_years_ago
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 6. Syntax
        //      section:    6.2. Expressions
        //      subsection: 6.2.3. CASE expressions
        // ---------------------------------------------------------------------

        MATCH (n)
        RETURN n.name,
        CASE
        WHEN n.age IS NULL THEN -1
        ELSE n.age - 10 END AS age_10_years_ago
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 6. Syntax
        //      section:    6.3. Variables
        // ---------------------------------------------------------------------

        MATCH (n)-->(b)
        RETURN b
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 6. Syntax
        //      section:    6.4. Parameters
        //      subsection: 6.4.2. String literal
        // ---------------------------------------------------------------------

        MATCH (n:Person)
        WHERE n.name = $name
        RETURN n
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 6. Syntax
        //      section:    6.4. Parameters
        //      subsection: 6.4.2. String literal
        // ---------------------------------------------------------------------

        MATCH (n:Person { name: $name })
        RETURN n
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 6. Syntax
        //      section:    6.4. Parameters
        //      subsection: 6.4.3. Regular expression
        // ---------------------------------------------------------------------

        MATCH (n:Person)
        WHERE n.name =~ $regex
        RETURN n.name
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 6. Syntax
        //      section:    6.4. Parameters
        //      subsection: 6.4.4. Case-sensitive string pattern matching
        // ---------------------------------------------------------------------

        MATCH (n:Person)
        WHERE n.name STARTS WITH $name
        RETURN n.name
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 6. Syntax
        //      section:    6.4. Parameters
        //      subsection: 6.4.5. Create node with properties
        // ---------------------------------------------------------------------

        CREATE ($props)
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 6. Syntax
        //      section:    6.4. Parameters
        //      subsection: 6.4.6. Create multiple nodes with properties
        // ---------------------------------------------------------------------

        UNWIND $props AS properties
        CREATE (n:Person)
        SET n = properties
        RETURN n
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 6. Syntax
        //      section:    6.4. Parameters
        //      subsection: 6.4.7. Setting all properties on a node
        // ---------------------------------------------------------------------

        MATCH (n:Person)
        WHERE n.name='Michaela'
        SET n = $props
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 6. Syntax
        //      section:    6.4. Parameters
        //      subsection: 6.4.8. SKIP and LIMIT
        // ---------------------------------------------------------------------

        MATCH (n:Person)
        RETURN n.name
        SKIP $s
        LIMIT $l
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 6. Syntax
        //      section:    6.4. Parameters
        //      subsection: 6.4.9. Node id
        // ---------------------------------------------------------------------

        MATCH (n)
        WHERE id(n)= $id
        RETURN n.name
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 6. Syntax
        //      section:    6.4. Parameters
        //      subsection: 6.4.10. Multiple node ids
        // ---------------------------------------------------------------------

        MATCH (n)
        WHERE id(n) IN $ids
        RETURN n.name
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 6. Syntax
        //      section:    6.4. Parameters
        //      subsection: 6.4.11. Calling procedures
        // ---------------------------------------------------------------------

        CALL db.resampleIndex($indexname)
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 6. Syntax
        //      section:    6.5. Operators
        //      subsection: 6.5.2. General operators
        // ---------------------------------------------------------------------

        CREATE (a:Person { name: 'Anne', eyeColor: 'blue' }),(b:Person { name: 'Bill', eyeColor: 'brown'
        }),(c:Person { name: 'Carol', eyeColor: 'blue' })
        WITH [a, b, c] AS ps
        UNWIND ps AS p
        RETURN DISTINCT p.eyeColor
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 6. Syntax
        //      section:    6.5. Operators
        //      subsection: 6.5.2. General operators
        // ---------------------------------------------------------------------

        WITH { person: { name: 'Anne', age: 25 }} AS p
        RETURN p.person.name
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 6. Syntax
        //      section:    6.5. Operators
        //      subsection: 6.5.2. General operators
        // ---------------------------------------------------------------------

        CREATE (a:Restaurant { name: 'Hungry Jo', rating_hygiene: 10, rating_food: 7 }),(b:Restaurant { name:
        'Buttercup Tea Rooms', rating_hygiene: 5, rating_food: 6 }),(c1:Category { name: 'hygiene' }),(c2:Category
        { name: 'food' })
        WITH a, b, c1, c2
        MATCH (restaurant:Restaurant),(category:Category)
        WHERE restaurant[\"rating_\" + category.name]> 6
        RETURN DISTINCT restaurant.name
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 6. Syntax
        //      section:    6.5. Operators
        //      subsection: 6.5.3. Mathematical operators
        // ---------------------------------------------------------------------

        WITH 2 AS number, 3 AS exponent
        RETURN number ^ exponent AS result
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 6. Syntax
        //      section:    6.5. Operators
        //      subsection: 6.5.3. Mathematical operators
        // ---------------------------------------------------------------------

        WITH -3 AS a, 4 AS b
        RETURN b - a AS result
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 6. Syntax
        //      section:    6.5. Operators
        //      subsection: 6.5.4. Comparison operators
        // ---------------------------------------------------------------------

        WITH 4 AS one, 3 AS two
        RETURN one > two AS result
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 6. Syntax
        //      section:    6.5. Operators
        //      subsection: 6.5.4. Comparison operators
        // ---------------------------------------------------------------------

        WITH ['John', 'Mark', 'Jonathan', 'Bill'] AS somenames
        UNWIND somenames AS names
        WITH names AS candidate
        WHERE candidate STARTS WITH 'Jo'
        RETURN candidate
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 6. Syntax
        //      section:    6.5. Operators
        //      subsection: 6.5.5. Boolean operators
        // ---------------------------------------------------------------------

        WITH [2, 4, 7, 9, 12] AS numberlist
        UNWIND numberlist AS number
        WITH number
        WHERE number = 4 OR (number > 6 AND number < 10)
        RETURN number
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 6. Syntax
        //      section:    6.5. Operators
        //      subsection: 6.5.6. String operators
        // ---------------------------------------------------------------------

        WITH ['mouse', 'chair', 'door', 'house'] AS wordlist
        UNWIND wordlist AS word
        WITH word
        WHERE word =~ '.*ous.*'
        RETURN word
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 6. Syntax
        //      section:    6.5. Operators
        //      subsection: 6.5.7. List operators
        // ---------------------------------------------------------------------

        RETURN [1,2,3,4,5]+[6,7] AS myList
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 6. Syntax
        //      section:    6.5. Operators
        //      subsection: 6.5.7. List operators
        // ---------------------------------------------------------------------

        WITH [2, 3, 4, 5] AS numberlist
        UNWIND numberlist AS number
        WITH number
        WHERE number IN [2, 3, 8]
        RETURN number
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 6. Syntax
        //      section:    6.5. Operators
        //      subsection: 6.5.7. List operators
        // ---------------------------------------------------------------------

        WITH ['Anne', 'John', 'Bill', 'Diane', 'Eve'] AS names
        RETURN names[1..3] AS result
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 6. Syntax
        //      section:    6.5. Operators
        //      subsection: 6.5.11. Chaining comparison operations
        // ---------------------------------------------------------------------

        MATCH (n) WHERE 21 < n.age <= 30 RETURN n
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 6. Syntax
        //      section:    6.5. Operators
        //      subsection: 6.5.11. Chaining comparison operations
        // ---------------------------------------------------------------------

        MATCH (n) WHERE 21 < n.age AND n.age <= 30 RETURN n
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 6. Syntax
        //      section:    6.6. Comments
        // ---------------------------------------------------------------------

        MATCH (n) RETURN n //This is an end of line comment
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 6. Syntax
        //      section:    6.6. Comments
        // ---------------------------------------------------------------------

        MATCH (n)
        //This is a whole line comment
        RETURN n
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 6. Syntax
        //      section:    6.7. Patterns
        //      subsection: 6.7.2. Patterns for nodes
        // ---------------------------------------------------------------------
Create
        (a)
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 6. Syntax
        //      section:    6.7. Patterns
        //      subsection: 6.7.3. Patterns for related nodes
        // ---------------------------------------------------------------------
Create
        (a)-->(b)
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 6. Syntax
        //      section:    6.7. Patterns
        //      subsection: 6.7.3. Patterns for related nodes
        // ---------------------------------------------------------------------
Create
        (a)-->(b)<--(c)
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 6. Syntax
        //      section:    6.7. Patterns
        //      subsection: 6.7.4. Patterns for labels
        // ---------------------------------------------------------------------
Create
        (a:User)-->(b)
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 6. Syntax
        //      section:    6.7. Patterns
        //      subsection: 6.7.4. Patterns for labels
        // ---------------------------------------------------------------------
Create
        (a:User:Admin)-->(b)
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 6. Syntax
        //      section:    6.7. Patterns
        //      subsection: 6.7.5. Specifying properties
        // ---------------------------------------------------------------------
Create () --
        (a {name: 'Andres', sport: 'Brazilian Ju-Jitsu'})
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 6. Syntax
        //      section:    6.7. Patterns
        //      subsection: 6.7.5. Specifying properties
        // ---------------------------------------------------------------------
Create
        (a)-[{blocked: false}]->(b)
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 6. Syntax
        //      section:    6.7. Patterns
        //      subsection: 6.7.6. Patterns for relationships
        // ---------------------------------------------------------------------
Create
        (a)--(b)
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 6. Syntax
        //      section:    6.7. Patterns
        //      subsection: 6.7.6. Patterns for relationships
        // ---------------------------------------------------------------------
Create
        (a)-[r]->(b)
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 6. Syntax
        //      section:    6.7. Patterns
        //      subsection: 6.7.6. Patterns for relationships
        // ---------------------------------------------------------------------
Create
        (a)-[r:REL_TYPE]->(b)
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 6. Syntax
        //      section:    6.7. Patterns
        //      subsection: 6.7.6. Patterns for relationships
        // ---------------------------------------------------------------------
Create
        (a)-[r:TYPE1|TYPE2]->(b)
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 6. Syntax
        //      section:    6.7. Patterns
        //      subsection: 6.7.6. Patterns for relationships
        // ---------------------------------------------------------------------
Create
        (a)-[:REL_TYPE]->(b)
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 6. Syntax
        //      section:    6.7. Patterns
        //      subsection: 6.7.7. Variable-length pattern matching
        // ---------------------------------------------------------------------
Create
        (a)-[*2]->(b)
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 6. Syntax
        //      section:    6.7. Patterns
        //      subsection: 6.7.7. Variable-length pattern matching
        // ---------------------------------------------------------------------
Create
        (a)-->()-->(b)
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 6. Syntax
        //      section:    6.7. Patterns
        //      subsection: 6.7.7. Variable-length pattern matching
        // ---------------------------------------------------------------------
Create
        (a)-[*3..5]->(b)
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 6. Syntax
        //      section:    6.7. Patterns
        //      subsection: 6.7.7. Variable-length pattern matching
        // ---------------------------------------------------------------------
Create
        (a)-[*3..]->(b)
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 6. Syntax
        //      section:    6.7. Patterns
        //      subsection: 6.7.7. Variable-length pattern matching
        // ---------------------------------------------------------------------
Create
        (a)-[*..5]->(b)
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 6. Syntax
        //      section:    6.7. Patterns
        //      subsection: 6.7.7. Variable-length pattern matching
        // ---------------------------------------------------------------------
Create
        (a)-[*]->(b)
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 6. Syntax
        //      section:    6.7. Patterns
        //      subsection: 6.7.7. Variable-length pattern matching
        // ---------------------------------------------------------------------

        MATCH (me)-[:KNOWS*1..2]-(remote_friend)
        WHERE me.name = 'Filipa'
        RETURN remote_friend.name
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 6. Syntax
        //      section:    6.7. Patterns
        //      subsection: 6.7.8. Assigning to path variables
        // ---------------------------------------------------------------------
Create
        p = (a)-[*3..5]->(b)
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 6. Syntax
        //      section:    6.8. Lists
        //      subsection: 6.8.1. Lists in general
        // ---------------------------------------------------------------------

        RETURN [0, 1, 2, 3, 4, 5, 6, 7, 8, 9] AS list
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 6. Syntax
        //      section:    6.8. Lists
        //      subsection: 6.8.1. Lists in general
        // ---------------------------------------------------------------------

        RETURN range(0, 10)[3]
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 6. Syntax
        //      section:    6.8. Lists
        //      subsection: 6.8.1. Lists in general
        // ---------------------------------------------------------------------

        RETURN range(0, 10)[-3]
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 6. Syntax
        //      section:    6.8. Lists
        //      subsection: 6.8.1. Lists in general
        // ---------------------------------------------------------------------

        RETURN range(0, 10)[0..3]
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 6. Syntax
        //      section:    6.8. Lists
        //      subsection: 6.8.1. Lists in general
        // ---------------------------------------------------------------------

        RETURN range(0, 10)[0..-5]
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 6. Syntax
        //      section:    6.8. Lists
        //      subsection: 6.8.1. Lists in general
        // ---------------------------------------------------------------------

        RETURN range(0, 10)[-5..]
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 6. Syntax
        //      section:    6.8. Lists
        //      subsection: 6.8.1. Lists in general
        // ---------------------------------------------------------------------

        RETURN range(0, 10)[..4]
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 6. Syntax
        //      section:    6.8. Lists
        //      subsection: 6.8.1. Lists in general
        // ---------------------------------------------------------------------

        RETURN range(0, 10)[15]
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 6. Syntax
        //      section:    6.8. Lists
        //      subsection: 6.8.1. Lists in general
        // ---------------------------------------------------------------------

        RETURN range(0, 10)[5..15]
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 6. Syntax
        //      section:    6.8. Lists
        //      subsection: 6.8.1. Lists in general
        // ---------------------------------------------------------------------

        RETURN size(range(0, 10)[0..3])
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 6. Syntax
        //      section:    6.8. Lists
        //      subsection: 6.8.2. List comprehension
        // ---------------------------------------------------------------------

        RETURN [x IN range(0,10) WHERE x % 2 = 0 | x^3] AS result
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 6. Syntax
        //      section:    6.8. Lists
        //      subsection: 6.8.2. List comprehension
        // ---------------------------------------------------------------------

        RETURN [x IN range(0,10) WHERE x % 2 = 0] AS result
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 6. Syntax
        //      section:    6.8. Lists
        //      subsection: 6.8.2. List comprehension
        // ---------------------------------------------------------------------

        RETURN [x IN range(0,10)| x^3] AS result
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 6. Syntax
        //      section:    6.8. Lists
        //      subsection: 6.8.3. Pattern comprehension
        // ---------------------------------------------------------------------

        MATCH (a:Person { name: 'Charlie Sheen' })
        RETURN [(a)-->(b) WHERE b:Movie | b.year] AS years
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 6. Syntax
        //      section:    6.9. Maps
        //      subsection: 6.9.1. Literal maps
        // ---------------------------------------------------------------------

        RETURN { key: 'Value', listKey: [{ inner: 'Map1' }, { inner: 'Map2' }]}
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 7. Clauses
        //      section:    7.1. MATCH
        //      subsection: 7.1.2. Basic node finding
        // ---------------------------------------------------------------------

        MATCH (n)
        RETURN n
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 7. Clauses
        //      section:    7.1. MATCH
        //      subsection: 7.1.2. Basic node finding
        // ---------------------------------------------------------------------

        MATCH (movie:Movie)
        RETURN movie.title
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 7. Clauses
        //      section:    7.1. MATCH
        //      subsection: 7.1.2. Basic node finding
        // ---------------------------------------------------------------------

        MATCH (director { name: 'Oliver Stone' })--(movie)
        RETURN movie.title
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 7. Clauses
        //      section:    7.1. MATCH
        //      subsection: 7.1.2. Basic node finding
        // ---------------------------------------------------------------------

        MATCH (:Person { name: 'Oliver Stone' })--(movie:Movie)
        RETURN movie.title
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 7. Clauses
        //      section:    7.1. MATCH
        //      subsection: 7.1.3. Relationship basics
        // ---------------------------------------------------------------------

        MATCH (:Person { name: 'Oliver Stone' })-->(movie)
        RETURN movie.title
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 7. Clauses
        //      section:    7.1. MATCH
        //      subsection: 7.1.3. Relationship basics
        // ---------------------------------------------------------------------

        MATCH (:Person { name: 'Oliver Stone' })-[r]->(movie)
        RETURN type(r)
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 7. Clauses
        //      section:    7.1. MATCH
        //      subsection: 7.1.3. Relationship basics
        // ---------------------------------------------------------------------

        MATCH (wallstreet:Movie { title: 'Wall Street' })<-[:ACTED_IN]-(actor)
        RETURN actor.name
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 7. Clauses
        //      section:    7.1. MATCH
        //      subsection: 7.1.3. Relationship basics
        // ---------------------------------------------------------------------

        MATCH (wallstreet { title: 'Wall Street' })<-[:ACTED_IN|:DIRECTED]-(person)
        RETURN person.name
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 7. Clauses
        //      section:    7.1. MATCH
        //      subsection: 7.1.3. Relationship basics
        // ---------------------------------------------------------------------

        MATCH (wallstreet { title: 'Wall Street' })<-[r:ACTED_IN]-(actor)
        RETURN r.role
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 7. Clauses
        //      section:    7.1. MATCH
        //      subsection: 7.1.4. Relationships in depth
        // ---------------------------------------------------------------------

        MATCH (charlie:Person { name: 'Charlie Sheen' }),(rob:Person { name: 'Rob Reiner' })
        CREATE (rob)-[:`TYPE
_______ WITH SPACE`]->(charlie)
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 7. Clauses
        //      section:    7.1. MATCH
        //      subsection: 7.1.4. Relationships in depth
        // ---------------------------------------------------------------------

        MATCH (n { name: 'Rob Reiner' })-[r:`TYPE
_______ WITH SPACE`]->()
        RETURN type(r)
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 7. Clauses
        //      section:    7.1. MATCH
        //      subsection: 7.1.4. Relationships in depth
        // ---------------------------------------------------------------------

        MATCH (charlie { name: 'Charlie Sheen' })-[:ACTED_IN]->(movie)<-[:DIRECTED]-(director)
        RETURN movie.title, director.name
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 7. Clauses
        //      section:    7.1. MATCH
        //      subsection: 7.1.4. Relationships in depth
        // ---------------------------------------------------------------------

        MATCH (martin { name: 'Charlie Sheen' })-[:ACTED_IN*1..3]-(movie:Movie)
        RETURN movie.title
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 7. Clauses
        //      section:    7.1. MATCH
        //      subsection: 7.1.4. Relationships in depth
        // ---------------------------------------------------------------------

        MATCH p =(actor { name: 'Charlie Sheen' })-[:ACTED_IN*2]-(co_actor)
        RETURN relationships(p)
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 7. Clauses
        //      section:    7.1. MATCH
        //      subsection: 7.1.4. Relationships in depth
        // ---------------------------------------------------------------------

        MATCH (charlie:Person { name: 'Charlie Sheen' }),(martin:Person { name: 'Martin Sheen' })
        CREATE (charlie)-[:X { blocked: FALSE }]->(:UNBLOCKED)<-[:X { blocked: FALSE }]-(martin)
        CREATE (charlie)-[:X { blocked: TRUE }]->(:BLOCKED)<-[:X { blocked: FALSE }]-(martin)
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 7. Clauses
        //      section:    7.1. MATCH
        //      subsection: 7.1.4. Relationships in depth
        // ---------------------------------------------------------------------

        MATCH p =(charlie:Person)-[* { blocked:false }]-(martin:Person)
        WHERE charlie.name = 'Charlie Sheen' AND martin.name = 'Martin Sheen'
        RETURN p
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 7. Clauses
        //      section:    7.1. MATCH
        //      subsection: 7.1.4. Relationships in depth
        // ---------------------------------------------------------------------

        MATCH (wallstreet:Movie { title: 'Wall Street' })-[*0..1]-(x)
        RETURN x
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 7. Clauses
        //      section:    7.1. MATCH
        //      subsection: 7.1.4. Relationships in depth
        // ---------------------------------------------------------------------

        MATCH p =(michael { name: 'Michael Douglas' })-->()
        RETURN p
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 7. Clauses
        //      section:    7.1. MATCH
        //      subsection: 7.1.4. Relationships in depth
        // ---------------------------------------------------------------------

        MATCH (a)-[r]-(b)
        WHERE id(r)= 0
        RETURN a,b
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 7. Clauses
        //      section:    7.1. MATCH
        //      subsection: 7.1.6. Get node or relationship by id
        // ---------------------------------------------------------------------

        MATCH (n)
        WHERE id(n)= 0
        RETURN n
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 7. Clauses
        //      section:    7.1. MATCH
        //      subsection: 7.1.6. Get node or relationship by id
        // ---------------------------------------------------------------------

        MATCH ()-[r]->()
        WHERE id(r)= 0
        RETURN r
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 7. Clauses
        //      section:    7.1. MATCH
        //      subsection: 7.1.6. Get node or relationship by id
        // ---------------------------------------------------------------------

        MATCH (n)
        WHERE id(n) IN [0, 3, 5]
        RETURN n
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 7. Clauses
        //      section:    7.2. OPTIONAL MATCH
        //      subsection: 7.2.2. Optional relationships
        // ---------------------------------------------------------------------

        MATCH (a:Movie { title: 'Wall Street' })
        OPTIONAL MATCH (a)-->(x)
        RETURN x
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 7. Clauses
        //      section:    7.2. OPTIONAL MATCH
        //      subsection: 7.2.3. Properties on optional elements
        // ---------------------------------------------------------------------

        MATCH (a:Movie { title: 'Wall Street' })
        OPTIONAL MATCH (a)-->(x)
        RETURN x, x.name
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 7. Clauses
        //      section:    7.2. OPTIONAL MATCH
        //      subsection: 7.2.4. Optional typed and named relationship
        // ---------------------------------------------------------------------

        MATCH (a:Movie { title: 'Wall Street' })
        OPTIONAL MATCH (a)-[r:ACTS_IN]->()
        RETURN a.title, r
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 7. Clauses
        //      section:    7.4. RETURN
        //      subsection: 7.4.2. Return nodes
        // ---------------------------------------------------------------------

        MATCH (n { name: 'B' })
        RETURN n
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 7. Clauses
        //      section:    7.4. RETURN
        //      subsection: 7.4.3. Return relationships
        // ---------------------------------------------------------------------

        MATCH (n { name: 'A' })-[r:KNOWS]->(c)
        RETURN r
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 7. Clauses
        //      section:    7.4. RETURN
        //      subsection: 7.4.4. Return property
        // ---------------------------------------------------------------------

        MATCH (n { name: 'A' })
        RETURN n.name
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 7. Clauses
        //      section:    7.4. RETURN
        //      subsection: 7.4.5. Return all elements
        // ---------------------------------------------------------------------

        MATCH p =(a { name: 'A' })-[r]->(b)
        RETURN *
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 7. Clauses
        //      section:    7.4. RETURN
        //      subsection: 7.4.6. Variable with uncommon characters
        // ---------------------------------------------------------------------

        MATCH (`This isn't a common variable`)
        WHERE `This isn't a common variable`.name = 'A'
        RETURN `This isn't a common variable`.happy
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 7. Clauses
        //      section:    7.4. RETURN
        //      subsection: 7.4.7. Column alias
        // ---------------------------------------------------------------------

        MATCH (a { name: 'A' })
        RETURN a.age AS SomethingTotallyDifferent
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 7. Clauses
        //      section:    7.4. RETURN
        //      subsection: 7.4.8. Optional properties
        // ---------------------------------------------------------------------

        MATCH (n)
        RETURN n.age
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 7. Clauses
        //      section:    7.4. RETURN
        //      subsection: 7.4.9. Other expressions
        // ---------------------------------------------------------------------

        MATCH (a { name: 'A' })
        RETURN a.age > 30, \"I'm a literal\",(a)-->()
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 7. Clauses
        //      section:    7.4. RETURN
        //      subsection: 7.4.10. Unique results
        // ---------------------------------------------------------------------

        MATCH (a { name: 'A' })-->(b)
        RETURN DISTINCT b
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 7. Clauses
        //      section:    7.5. WITH
        //      subsection: 7.5.2. Filter on aggregate function results
        // ---------------------------------------------------------------------

        MATCH (david { name: 'David' })--(otherPerson)-->()
        WITH otherPerson, count(*) AS foaf
        WHERE foaf > 1
        RETURN otherPerson.name
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 7. Clauses
        //      section:    7.5. WITH
        //      subsection: 7.5.3. Sort results before using collect on them
        // ---------------------------------------------------------------------

        MATCH (n)
        WITH n
        ORDER BY n.name DESC LIMIT 3
        RETURN collect(n.name)
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 7. Clauses
        //      section:    7.5. WITH
        //      subsection: 7.5.4. Limit branching of a path search
        // ---------------------------------------------------------------------

        MATCH (n { name: 'Anders' })--(m)
        WITH m
        ORDER BY m.name DESC LIMIT 1
        MATCH (m)--(o)
        RETURN o.name
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 7. Clauses
        //      section:    7.6. UNWIND
        //      subsection: 7.6.2. Unwind a list
        // ---------------------------------------------------------------------

        UNWIND [1, 2, 3] AS x
        RETURN x
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 7. Clauses
        //      section:    7.6. UNWIND
        //      subsection: 7.6.3. Create a distinct list
        // ---------------------------------------------------------------------

        WITH [1, 1, 2, 2] AS coll
        UNWIND coll AS x
        WITH DISTINCT x
        RETURN collect(x) AS SET_wwe
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 7. Clauses
        //      section:    7.6. UNWIND
        //      subsection: 7.6.4. Create nodes from a list parameter
        // ---------------------------------------------------------------------

        UNWIND $events AS event
        MERGE (y:Year { year: event.year })
        MERGE (y)<-[:IN]-(e:Event { id: event.id })
        RETURN e.id AS x
        ORDER BY x
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 7. Clauses
        //      section:    7.7. WHERE
        //      subsection: 7.7.2. Basic usage
        // ---------------------------------------------------------------------

        MATCH (n)
        WHERE n.name = 'Peter' XOR (n.age < 30 AND n.name = 'Tobias') OR NOT (n.name = 'Tobias' OR n.name =
        'Peter')
        RETURN n.name, n.age
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 7. Clauses
        //      section:    7.7. WHERE
        //      subsection: 7.7.2. Basic usage
        // ---------------------------------------------------------------------

        MATCH (n)
        WHERE n:Swedish
        RETURN n.name, n.age
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 7. Clauses
        //      section:    7.7. WHERE
        //      subsection: 7.7.2. Basic usage
        // ---------------------------------------------------------------------

        MATCH (n)
        WHERE n.age < 30
        RETURN n.name, n.age
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 7. Clauses
        //      section:    7.7. WHERE
        //      subsection: 7.7.2. Basic usage
        // ---------------------------------------------------------------------

        MATCH (n)-[k:KNOWS]->(f)
        WHERE k.since < 2000
        RETURN f.name, f.age, f.email
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 7. Clauses
        //      section:    7.7. WHERE
        //      subsection: 7.7.2. Basic usage
        // ---------------------------------------------------------------------

        WITH 'AGE' AS propname
        MATCH (n)
        WHERE n[toLower(propname)]< 30
        RETURN n.name, n.age
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 7. Clauses
        //      section:    7.7. WHERE
        //      subsection: 7.7.2. Basic usage
        // ---------------------------------------------------------------------

        MATCH (n)
        WHERE exists(n.belt)
        RETURN n.name, n.belt
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 7. Clauses
        //      section:    7.7. WHERE
        //      subsection: 7.7.3. String matching
        // ---------------------------------------------------------------------

        MATCH (n)
        WHERE n.name STARTS WITH 'Pet'
        RETURN n.name, n.age
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 7. Clauses
        //      section:    7.7. WHERE
        //      subsection: 7.7.3. String matching
        // ---------------------------------------------------------------------

        MATCH (n)
        WHERE n.name ENDS WITH 'ter'
        RETURN n.name, n.age
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 7. Clauses
        //      section:    7.7. WHERE
        //      subsection: 7.7.3. String matching
        // ---------------------------------------------------------------------

        MATCH (n)
        WHERE n.name CONTAINS 'ete'
        RETURN n.name, n.age
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 7. Clauses
        //      section:    7.7. WHERE
        //      subsection: 7.7.3. String matching
        // ---------------------------------------------------------------------

        MATCH (n)
        WHERE NOT n.name ENDS WITH 's'
        RETURN n.name, n.age
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 7. Clauses
        //      section:    7.7. WHERE
        //      subsection: 7.7.4. Regular expressions
        // ---------------------------------------------------------------------

        MATCH (n)
        WHERE n.name =~ 'Tob.*'
        RETURN n.name, n.age
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 7. Clauses
        //      section:    7.7. WHERE
        //      subsection: 7.7.4. Regular expressions
        // ---------------------------------------------------------------------

        MATCH (n)
        WHERE n.address =~ 'Sweden/Malmo'
        RETURN n.name, n.age, n.address
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 7. Clauses
        //      section:    7.7. WHERE
        //      subsection: 7.7.4. Regular expressions
        // ---------------------------------------------------------------------

        MATCH (n)
        WHERE n.name =~ '(?i)ANDR.*'
        RETURN n.name, n.age
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 7. Clauses
        //      section:    7.7. WHERE
        //      subsection: 7.7.5. Using path patterns in WHERE
        // ---------------------------------------------------------------------

        MATCH (tobias { name: 'Tobias' }),(others)
        WHERE others.name IN ['Andres', 'Peter'] AND (tobias)<--(others)
        RETURN others.name, others.age
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 7. Clauses
        //      section:    7.7. WHERE
        //      subsection: 7.7.5. Using path patterns in WHERE
        // ---------------------------------------------------------------------

        MATCH (persons),(peter { name: 'Peter' })
        WHERE NOT (persons)-->(peter)
        RETURN persons.name, persons.age
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 7. Clauses
        //      section:    7.7. WHERE
        //      subsection: 7.7.5. Using path patterns in WHERE
        // ---------------------------------------------------------------------

        MATCH (n)
        WHERE (n)-[:KNOWS]-({ name: 'Tobias' })
        RETURN n.name, n.age
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 7. Clauses
        //      section:    7.7. WHERE
        //      subsection: 7.7.5. Using path patterns in WHERE
        // ---------------------------------------------------------------------

        MATCH (n)-[r]->()
        WHERE n.name='Andres' AND type(r)=~ 'K.*'
        RETURN type(r), r.since
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 7. Clauses
        //      section:    7.7. WHERE
        //      subsection: 7.7.6. Lists
        // ---------------------------------------------------------------------

        MATCH (a)
        WHERE a.name IN ['Peter', 'Tobias']
        RETURN a.name, a.age
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 7. Clauses
        //      section:    7.7. WHERE
        //      subsection: 7.7.7. Missing properties and values
        // ---------------------------------------------------------------------

        MATCH (n)
        WHERE n.belt = 'white'
        RETURN n.name, n.age, n.belt
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 7. Clauses
        //      section:    7.7. WHERE
        //      subsection: 7.7.7. Missing properties and values
        // ---------------------------------------------------------------------

        MATCH (n)
        WHERE n.belt = 'white' OR n.belt IS NULL RETURN n.name, n.age, n.belt
        ORDER BY n.name
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 7. Clauses
        //      section:    7.7. WHERE
        //      subsection: 7.7.7. Missing properties and values
        // ---------------------------------------------------------------------

        MATCH (person)
        WHERE person.name = 'Peter' AND person.belt IS NULL RETURN person.name, person.age, person.belt
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 7. Clauses
        //      section:    7.7. WHERE
        //      subsection: 7.7.8. Using ranges
        // ---------------------------------------------------------------------

        MATCH (a)
        WHERE a.name >= 'Peter'
        RETURN a.name, a.age
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 7. Clauses
        //      section:    7.7. WHERE
        //      subsection: 7.7.8. Using ranges
        // ---------------------------------------------------------------------

        MATCH (a)
        WHERE a.name > 'Andres' AND a.name < 'Tobias'
        RETURN a.name, a.age
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 7. Clauses
        //      section:    7.8. ORDER BY
        //      subsection: 7.8.2. Order nodes by property
        // ---------------------------------------------------------------------

        MATCH (n)
        RETURN n.name, n.age
        ORDER BY n.name
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 7. Clauses
        //      section:    7.8. ORDER BY
        //      subsection: 7.8.3. Order nodes by multiple properties
        // ---------------------------------------------------------------------

        MATCH (n)
        RETURN n.name, n.age
        ORDER BY n.age, n.name
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 7. Clauses
        //      section:    7.8. ORDER BY
        //      subsection: 7.8.4. Order nodes in descending order
        // ---------------------------------------------------------------------

        MATCH (n)
        RETURN n.name, n.age
        ORDER BY n.name DESC
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 7. Clauses
        //      section:    7.8. ORDER BY
        //      subsection: 7.8.5. Ordering null
        // ---------------------------------------------------------------------

        MATCH (n)
        RETURN n.length, n.name, n.age
        ORDER BY n.length
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 7. Clauses
        //      section:    7.9. SKIP
        //      subsection: 7.9.2. Skip first three rows
        // ---------------------------------------------------------------------

        MATCH (n)
        RETURN n.name
        ORDER BY n.name
        SKIP 3
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 7. Clauses
        //      section:    7.9. SKIP
        //      subsection: 7.9.3. Return middle two rows
        // ---------------------------------------------------------------------

        MATCH (n)
        RETURN n.name
        ORDER BY n.name
        SKIP 1
        LIMIT 2
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 7. Clauses
        //      section:    7.9. SKIP
        //      subsection: 7.9.4. Using an expression with SKIP to return a subset of the rows
        // ---------------------------------------------------------------------

        MATCH (n)
        RETURN n.name
        ORDER BY n.name
        SKIP toInteger(3*rand())+ 1
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 7. Clauses
        //      section:    7.10. LIMIT
        //      subsection: 7.10.2. Return a subset of the rows
        // ---------------------------------------------------------------------

        MATCH (n)
        RETURN n.name
        ORDER BY n.name
        LIMIT 3
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 7. Clauses
        //      section:    7.10. LIMIT
        //      subsection: 7.10.3. Using an expression with LIMIT to return a subset of the rows
        // ---------------------------------------------------------------------

        MATCH (n)
        RETURN n.name
        ORDER BY n.name
        LIMIT toInteger(3 * rand())+ 1
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 7. Clauses
        //      section:    7.11. CREATE
        //      subsection: 7.11.1. Create nodes
        // ---------------------------------------------------------------------

        CREATE (n)
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 7. Clauses
        //      section:    7.11. CREATE
        //      subsection: 7.11.1. Create nodes
        // ---------------------------------------------------------------------

        CREATE (n),(m)
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 7. Clauses
        //      section:    7.11. CREATE
        //      subsection: 7.11.1. Create nodes
        // ---------------------------------------------------------------------

        CREATE (n:Person)
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 7. Clauses
        //      section:    7.11. CREATE
        //      subsection: 7.11.1. Create nodes
        // ---------------------------------------------------------------------

        CREATE (n:Person:Swedish)
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 7. Clauses
        //      section:    7.11. CREATE
        //      subsection: 7.11.1. Create nodes
        // ---------------------------------------------------------------------

        CREATE (n:Person { name: 'Andres', title: 'Developer' })
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 7. Clauses
        //      section:    7.11. CREATE
        //      subsection: 7.11.1. Create nodes
        // ---------------------------------------------------------------------

        CREATE (a { name: 'Andres' })
        RETURN a
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 7. Clauses
        //      section:    7.11. CREATE
        //      subsection: 7.11.2. Create relationships
        // ---------------------------------------------------------------------

        MATCH (a:Person),(b:Person)
        WHERE a.name = 'Node A' AND b.name = 'Node B'
        CREATE (a)-[r:RELTYPE]->(b)
        RETURN r
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 7. Clauses
        //      section:    7.11. CREATE
        //      subsection: 7.11.2. Create relationships
        // ---------------------------------------------------------------------

        MATCH (a:Person),(b:Person)
        WHERE a.name = 'Node A' AND b.name = 'Node B'
        CREATE (a)-[r:RELTYPE { name: a.name + '<->' + b.name }]->(b)
        RETURN r
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 7. Clauses
        //      section:    7.11. CREATE
        //      subsection: 7.11.3. Create a full path
        // ---------------------------------------------------------------------

        CREATE p =(andres { name:'Andres' })-[:WORKS_AT]->(neo)<-[:WORKS_AT]-(michael { name: 'Michael' })
        RETURN p
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 7. Clauses
        //      section:    7.11. CREATE
        //      subsection: 7.11.4. Use parameters with CREATE
        // ---------------------------------------------------------------------

        CREATE (n:Person $props)
        RETURN n
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 7. Clauses
        //      section:    7.11. CREATE
        //      subsection: 7.11.4. Use parameters with CREATE
        // ---------------------------------------------------------------------

        UNWIND $props AS map
        CREATE (n)
        SET n = map
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 7. Clauses
        //      section:    7.12. DELETE
        //      subsection: 7.12.2. Delete single node
        // ---------------------------------------------------------------------

        MATCH (n:Useless)
        DELETE n
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 7. Clauses
        //      section:    7.12. DELETE
        //      subsection: 7.12.3. Delete all nodes and relationships
        // ---------------------------------------------------------------------

        MATCH (n)
        DETACH DELETE n
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 7. Clauses
        //      section:    7.12. DELETE
        //      subsection: 7.12.4. Delete a node with all its relationships
        // ---------------------------------------------------------------------

        MATCH (n { name: 'Andres' })
        DETACH DELETE n
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 7. Clauses
        //      section:    7.12. DELETE
        //      subsection: 7.12.3. Delete all nodes and relationships
        // ---------------------------------------------------------------------

        MATCH (n { name: 'Andres' })-[r:KNOWS]->()
        DELETE r
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 7. Clauses
        //      section:    7.13. SET
        //      subsection: 7.13.2. Set a property
        // ---------------------------------------------------------------------

        MATCH (n { name: 'Andres' })
        SET n.surname = 'Taylor'
        RETURN n
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 7. Clauses
        //      section:    7.13. SET
        //      subsection: 7.13.3. Remove a property
        // ---------------------------------------------------------------------

        MATCH (n { name: 'Andres' })
        SET n.name = NULL RETURN n
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 7. Clauses
        //      section:    7.13. SET
        //      subsection: 7.13.4. Copying properties between nodes and relationships
        // ---------------------------------------------------------------------

        MATCH (at { name: 'Andres' }),(pn { name: 'Peter' })
        SET at = pn
        RETURN at, pn
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 7. Clauses
        //      section:    7.13. SET
        //      subsection: 7.13.5. Adding properties from maps
        // ---------------------------------------------------------------------

        MATCH (peter { name: 'Peter' })
        SET peter += { hungry: TRUE , position: 'Entrepreneur' }
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 7. Clauses
        //      section:    7.13. SET
        //      subsection: 7.13.6. Set a property using a parameter
        // ---------------------------------------------------------------------

        MATCH (n { name: 'Andres' })
        SET n.surname = $surname
        RETURN n
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 7. Clauses
        //      section:    7.13. SET
        //      subsection: 7.13.7. Set all properties using a parameter
        // ---------------------------------------------------------------------

        MATCH (n { name: 'Andres' })
        SET n = $props
        RETURN n
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 7. Clauses
        //      section:    7.13. SET
        //      subsection: 7.13.8. Set multiple properties using one SET clause
        // ---------------------------------------------------------------------

        MATCH (n { name: 'Andres' })
        SET n.position = 'Developer', n.surname = 'Taylor'
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 7. Clauses
        //      section:    7.13. SET
        //      subsection: 7.13.9. Set a label on a node
        // ---------------------------------------------------------------------

        MATCH (n { name: 'Stefan' })
        SET n:German
        RETURN n
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 7. Clauses
        //      section:    7.13. SET
        //      subsection: 7.13.10. Set multiple labels on a node
        // ---------------------------------------------------------------------

        MATCH (n { name: 'Emil' })
        SET n:Swedish:Bossman
        RETURN n
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 7. Clauses
        //      section:    7.14. REMOVE
        //      subsection: 7.14.2. Remove a property
        // ---------------------------------------------------------------------

        MATCH (andres { name: 'Andres' })
        REMOVE andres.age
        RETURN andres
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 7. Clauses
        //      section:    7.14. REMOVE
        //      subsection: 7.14.3. Remove a label from a node
        // ---------------------------------------------------------------------

        MATCH (n { name: 'Peter' })
        REMOVE n:German
        RETURN n
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 7. Clauses
        //      section:    7.14. REMOVE
        //      subsection: 7.14.4. Removing multiple labels
        // ---------------------------------------------------------------------

        MATCH (n { name: 'Peter' })
        REMOVE n:German:Swedish
        RETURN n
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 7. Clauses
        //      section:    7.16. MERGE
        //      subsection: 7.16.2. Merge nodes
        // ---------------------------------------------------------------------

        MERGE (robert:Critic)
        RETURN robert, labels(robert)
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 7. Clauses
        //      section:    7.16. MERGE
        //      subsection: 7.16.2. Merge nodes
        // ---------------------------------------------------------------------

        MERGE (charlie { name: 'Charlie Sheen', age: 10 })
        RETURN charlie
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 7. Clauses
        //      section:    7.16. MERGE
        //      subsection: 7.16.2. Merge nodes
        // ---------------------------------------------------------------------

        MERGE (michael:Person { name: 'Michael Douglas' })
        RETURN michael.name, michael.bornIn
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 7. Clauses
        //      section:    7.16. MERGE
        //      subsection: 7.16.2. Merge nodes
        // ---------------------------------------------------------------------

        MATCH (person:Person)
        MERGE (city:City { name: person.bornIn })
        RETURN person.name, person.bornIn, city
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 7. Clauses
        //      section:    7.16. MERGE
        //      subsection: 7.16.3. Use ON CREATE and ON MATCH
        // ---------------------------------------------------------------------

        MERGE (keanu:Person { name: 'Keanu Reeves' })
        ON CREATE SET keanu.created = timestamp()
        RETURN keanu.name, keanu.created
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 7. Clauses
        //      section:    7.16. MERGE
        //      subsection: 7.16.3. Use ON CREATE and ON MATCH
        // ---------------------------------------------------------------------

        MERGE (person:Person)
        ON MATCH SET person.found = TRUE RETURN person.name, person.found
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 7. Clauses
        //      section:    7.16. MERGE
        //      subsection: 7.16.3. Use ON CREATE and ON MATCH
        // ---------------------------------------------------------------------

        MERGE (keanu:Person { name: 'Keanu Reeves' })
        ON CREATE SET keanu.created = timestamp()
        ON MATCH SET keanu.lastSeen = timestamp()
        RETURN keanu.name, keanu.created, keanu.lastSeen
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 7. Clauses
        //      section:    7.16. MERGE
        //      subsection: 7.16.3. Use ON CREATE and ON MATCH
        // ---------------------------------------------------------------------

        MERGE (person:Person)
        ON MATCH SET person.found = TRUE , person.lastAccessed = timestamp()
        RETURN person.name, person.found, person.lastAccessed
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 7. Clauses
        //      section:    7.16. MERGE
        //      subsection: 7.16.4. Merge relationships
        // ---------------------------------------------------------------------

        MATCH (charlie:Person { name: 'Charlie Sheen' }),(wallStreet:Movie { title: 'Wall Street' })
        MERGE (charlie)-[r:ACTED_IN]->(wallStreet)
        RETURN charlie.name, type(r), wallStreet.title
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 7. Clauses
        //      section:    7.16. MERGE
        //      subsection: 7.16.4. Merge relationships
        // ---------------------------------------------------------------------

        MATCH (oliver:Person { name: 'Oliver Stone' }),(reiner:Person { name: 'Rob Reiner' })
        MERGE (oliver)-[:DIRECTED]->(movie:Movie)<-[:ACTED_IN]-(reiner)
        RETURN movie
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 7. Clauses
        //      section:    7.16. MERGE
        //      subsection: 7.16.4. Merge relationships
        // ---------------------------------------------------------------------

        MATCH (charlie:Person { name: 'Charlie Sheen' }),(oliver:Person { name: 'Oliver Stone' })
        MERGE (charlie)-[r:KNOWS]-(oliver)
        RETURN r
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 7. Clauses
        //      section:    7.16. MERGE
        //      subsection: 7.16.4. Merge relationships
        // ---------------------------------------------------------------------

        MATCH (person:Person)
        MERGE (city:City { name: person.bornIn })
        MERGE (person)-[r:BORN_IN]->(city)
        RETURN person.name, person.bornIn, city
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 7. Clauses
        //      section:    7.16. MERGE
        //      subsection: 7.16.4. Merge relationships
        // ---------------------------------------------------------------------

        MATCH (person:Person)
        MERGE (person)-[r:HAS_CHAUFFEUR]->(chauffeur:Chauffeur { name: person.chauffeurName })
        RETURN person.name, person.chauffeurName, chauffeur
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 7. Clauses
        //      section:    7.16. MERGE
        //      subsection: 7.16.5. Using unique constraints with MERGE
        // ---------------------------------------------------------------------

        MERGE (laurence:Person { name: 'Laurence Fishburne' })
        RETURN laurence.name
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 7. Clauses
        //      section:    7.16. MERGE
        //      subsection: 7.16.5. Using unique constraints with MERGE
        // ---------------------------------------------------------------------

        MERGE (oliver:Person { name: 'Oliver Stone' })
        RETURN oliver.name, oliver.bornIn
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 7. Clauses
        //      section:    7.16. MERGE
        //      subsection: 7.16.5. Using unique constraints with MERGE
        // ---------------------------------------------------------------------

        MERGE (michael:Person { name: 'Michael Douglas', role: 'Gordon Gekko' })
        RETURN michael
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 7. Clauses
        //      section:    7.16. MERGE
        //      subsection: 7.16.5. Using unique constraints with MERGE
        // ---------------------------------------------------------------------

        MERGE (oliver:Person { name: 'Oliver Stone', role: 'Gordon Gekko' })
        RETURN oliver
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 7. Clauses
        //      section:    7.16. MERGE
        //      subsection: 7.16.6. Using map parameters with MERGE
        // ---------------------------------------------------------------------

        MERGE (person:Person { name: $param.name, role: $param.role })
        RETURN person.name, person.role
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 7. Clauses
        //      section:    7.17. CALL[...YIELD]
        //      subsection: 7.17.2. Call a procedure using CALL
        // ---------------------------------------------------------------------

        CALL db.labels
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 7. Clauses
        //      section:    7.17. CALL[...YIELD]
        //      subsection: 7.17.4. Call a procedure using a quoted namespace and name
        // ---------------------------------------------------------------------

        CALL `db`.`labels`
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 7. Clauses
        //      section:    7.17. CALL[...YIELD]
        //      subsection: 7.17.5. Call a procedure with literal arguments
        // ---------------------------------------------------------------------

        CALL org.neo4j.procedure.example.addNodeToIndex('users', 0, 'name')
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 7. Clauses
        //      section:    7.17. CALL[...YIELD]
        //      subsection: 7.17.6. Call a procedure with parameter arguments
        // ---------------------------------------------------------------------

        CALL org.neo4j.procedure.example.addNodeToIndex
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 7. Clauses
        //      section:    7.17. CALL[...YIELD]
        //      subsection: 7.17.7. Call a procedure with mixed literal and parameter arguments
        // ---------------------------------------------------------------------

        CALL org.neo4j.procedure.example.addNodeToIndex('users', $node_wwe, 'name')
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 7. Clauses
        //      section:    7.17. CALL[...YIELD]
        //      subsection: 7.17.8. Call a procedure with literal and default arguments
        // ---------------------------------------------------------------------

        CALL org.neo4j.procedure.example.addNodeToIndex('users', 0)
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 7. Clauses
        //      section:    7.17. CALL[...YIELD]
        //      subsection: 7.17.9. Call a procedure within a complex query using CALL YIELD
        // ---------------------------------------------------------------------

        CALL db.labels() YIELD label
        RETURN count(label) AS numLabels
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 7. Clauses
        //      section:    7.17. CALL[...YIELD]
        //      subsection: 7.17.11. Call a procedure within a complex query and rename its outputs
        // ---------------------------------------------------------------------

        CALL db.propertyKeys() YIELD propertyKey AS prop
        MATCH (n)
        WHERE n[prop] IS NOT NULL RETURN prop, count(n) AS numNodes
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 7. Clauses
        //      section:    7.19. UNION
        //      subsection: 7.19.2. Combine two queries and retain duplicates
        // ---------------------------------------------------------------------

        MATCH (n:Actor)
        RETURN n.name AS name
        UNION ALL MATCH (n:Movie)
        RETURN n.title AS name
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 7. Clauses
        //      section:    7.19. UNION
        //      subsection: 7.19.3. Combine two queries and remove duplicates
        // ---------------------------------------------------------------------

        MATCH (n:Actor)
        RETURN n.name AS name
        UNION
        MATCH (n:Movie)
        RETURN n.title AS name
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 8. Functions
        //      section:    8.1. Predicate functions
        //      subsection: 8.1.1. all()
        // ---------------------------------------------------------------------

        MATCH p =(a)-[*1..3]->(b)
        WHERE a.name = 'Alice' AND b.name = 'Daniel' AND ALL (x IN nodes(p) WHERE x.age > 30)
        RETURN p
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 8. Functions
        //      section:    8.1. Predicate functions
        //      subsection: 8.1.2. any()
        // ---------------------------------------------------------------------

        MATCH (a)
        WHERE a.name = 'Eskil' AND ANY (x IN a.array WHERE x = 'one')
        RETURN a.name, a.array
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 8. Functions
        //      section:    8.1. Predicate functions
        //      subsection: 8.1.3. exists()
        // ---------------------------------------------------------------------

        MATCH (n)
        WHERE exists(n.name)
        RETURN n.name AS name, exists((n)-[:MARRIED]->()) AS is_married
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 8. Functions
        //      section:    8.1. Predicate functions
        //      subsection: 8.1.4. none()
        // ---------------------------------------------------------------------

        MATCH p =(n)-[*1..3]->(b)
        WHERE n.name = 'Alice' AND NONE (x IN nodes(p) WHERE x.age = 25)
        RETURN p
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 8. Functions
        //      section:    8.1. Predicate functions
        //      subsection: 8.1.5. single()
        // ---------------------------------------------------------------------

        MATCH p =(n)-->(b)
        WHERE n.name = 'Alice' AND SINGLE (var IN nodes(p) WHERE var.eyes = 'blue')
        RETURN p
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 8. Functions
        //      section:    8.2. Scalar functions
        //      subsection: 8.2.1. coalesce()
        // ---------------------------------------------------------------------

        MATCH (a)
        WHERE a.name = 'Alice'
        RETURN coalesce(a.hairColor, a.eyes)
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 8. Functions
        //      section:    8.2. Scalar functions
        //      subsection: 8.2.2. endNode()
        // ---------------------------------------------------------------------

        MATCH (x:Developer)-[r]-()
        RETURN endNode(r)
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 8. Functions
        //      section:    8.2. Scalar functions
        //      subsection: 8.2.3. head()
        // ---------------------------------------------------------------------

        MATCH (a)
        WHERE a.name = 'Eskil'
        RETURN a.array, head(a.array)
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 8. Functions
        //      section:    8.2. Scalar functions
        //      subsection: 8.2.4. id()
        // ---------------------------------------------------------------------

        MATCH (a)
        RETURN id(a)
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 8. Functions
        //      section:    8.2. Scalar functions
        //      subsection: 8.2.5. last()
        // ---------------------------------------------------------------------

        MATCH (a)
        WHERE a.name = 'Eskil'
        RETURN a.array, last(a.array)
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 8. Functions
        //      section:    8.2. Scalar functions
        //      subsection: 8.2.6. length()
        // ---------------------------------------------------------------------

        MATCH p =(a)-->(b)-->(c)
        WHERE a.name = 'Alice'
        RETURN length(p)
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 8. Functions
        //      section:    8.2. Scalar functions
        //      subsection: 8.2.7. properties()
        // ---------------------------------------------------------------------

        CREATE (p:Person { name: 'Stefan', city: 'Berlin' })
        RETURN properties(p)
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 8. Functions
        //      section:    8.2. Scalar functions
        //      subsection: 8.2.8. size()
        // ---------------------------------------------------------------------

        RETURN size(['Alice', 'Bob'])
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 8. Functions
        //      section:    8.2. Scalar functions
        //      subsection: 8.2.9. size() applied to pattern expression
        // ---------------------------------------------------------------------

        MATCH (a)
        WHERE a.name = 'Alice'
        RETURN size((a)-->()-->()) AS fof
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 8. Functions
        //      section:    8.2. Scalar functions
        //      subsection: 8.2.10. size() applied to string
        // ---------------------------------------------------------------------

        MATCH (a)
        WHERE size(a.name)> 6
        RETURN size(a.name)
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 8. Functions
        //      section:    8.2. Scalar functions
        //      subsection: 8.2.11. startNode()
        // ---------------------------------------------------------------------

        MATCH (x:Developer)-[r]-()
        RETURN startNode(r)
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 8. Functions
        //      section:    8.2. Scalar functions
        //      subsection: 8.2.12. timestamp()
        // ---------------------------------------------------------------------

        RETURN timestamp()
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 8. Functions
        //      section:    8.2. Scalar functions
        //      subsection: 8.2.13. toBoolean()
        // ---------------------------------------------------------------------

        RETURN toBoolean('TRUE'), toBoolean('not a boolean')
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 8. Functions
        //      section:    8.2. Scalar functions
        //      subsection: 8.2.14. toFloat()
        // ---------------------------------------------------------------------

        RETURN toFloat('11.5'), toFloat('not a number')
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 8. Functions
        //      section:    8.2. Scalar functions
        //      subsection: 8.2.15. toInteger()
        // ---------------------------------------------------------------------

        RETURN toInteger('42'), toInteger('not a number')
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 8. Functions
        //      section:    8.2. Scalar functions
        //      subsection: 8.2.16. type()
        // ---------------------------------------------------------------------

        MATCH (n)-[r]->()
        WHERE n.name = 'Alice'
        RETURN type(r)
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 8. Functions
        //      section:    8.3. Aggregating functions
        //      subsection: 8.3.1. avg()
        // ---------------------------------------------------------------------

        MATCH (n:Person)
        RETURN avg(n.age)
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 8. Functions
        //      section:    8.3. Aggregating functions
        //      subsection: 8.3.2. collect()
        // ---------------------------------------------------------------------

        MATCH (n:Person)
        RETURN collect(n.age)
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 8. Functions
        //      section:    8.3. Aggregating functions
        //      subsection: 8.3.3. count()
        // ---------------------------------------------------------------------

        MATCH (n { name: 'A' })-->(x)
        RETURN labels(n), n.age, count(*)
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 8. Functions
        //      section:    8.3. Aggregating functions
        //      subsection: 8.3.3. count()
        // ---------------------------------------------------------------------

        MATCH (n { name: 'A' })-[r]->()
        RETURN type(r), count(*)
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 8. Functions
        //      section:    8.3. Aggregating functions
        //      subsection: 8.3.3. count()
        // ---------------------------------------------------------------------

        MATCH (n { name: 'A' })-->(x)
        RETURN count(x)
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 8. Functions
        //      section:    8.3. Aggregating functions
        //      subsection: 8.3.3. count()
        // ---------------------------------------------------------------------

        MATCH (n:Person)
        RETURN count(n.age)
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 8. Functions
        //      section:    8.3. Aggregating functions
        //      subsection: 8.3.3. count()
        // ---------------------------------------------------------------------

        MATCH (me:Person)-->(friend:Person)-->(friend_of_friend:Person)
        WHERE me.name = 'A'
        RETURN count(DISTINCT friend_of_friend), count(friend_of_friend)
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 8. Functions
        //      section:    8.3. Aggregating functions
        //      subsection: 8.3.4. max()
        // ---------------------------------------------------------------------

        UNWIND [1, 'a', NULL , 0.2, 'b', '1', '99'] AS val
        RETURN max(val)
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 8. Functions
        //      section:    8.3. Aggregating functions
        //      subsection: 8.3.4. max()
        // ---------------------------------------------------------------------

        UNWIND [[1, 'a', 89],[1, 2]] AS val
        RETURN max(val)
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 8. Functions
        //      section:    8.3. Aggregating functions
        //      subsection: 8.3.4. max()
        // ---------------------------------------------------------------------

        MATCH (n:Person)
        RETURN max(n.age)
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 8. Functions
        //      section:    8.3. Aggregating functions
        //      subsection: 8.3.5. min()
        // ---------------------------------------------------------------------

        UNWIND [1, 'a', NULL , 0.2, 'b', '1', '99'] AS val
        RETURN min(val)
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 8. Functions
        //      section:    8.3. Aggregating functions
        //      subsection: 8.3.5. min()
        // ---------------------------------------------------------------------

        UNWIND ['d',[1, 2],['a', 'c', 23]] AS val
        RETURN min(val)
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 8. Functions
        //      section:    8.3. Aggregating functions
        //      subsection: 8.3.5. min()
        // ---------------------------------------------------------------------

        MATCH (n:Person)
        RETURN min(n.age)
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 8. Functions
        //      section:    8.3. Aggregating functions
        //      subsection: 8.3.6. percentileCont()
        // ---------------------------------------------------------------------

        MATCH (n:Person)
        RETURN percentileCont(n.age, 0.4)
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 8. Functions
        //      section:    8.3. Aggregating functions
        //      subsection: 8.3.7. percentileDisc()
        // ---------------------------------------------------------------------

        MATCH (n:Person)
        RETURN percentileDisc(n.age, 0.5)
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 8. Functions
        //      section:    8.3. Aggregating functions
        //      subsection: 8.3.8. stDev()
        // ---------------------------------------------------------------------

        MATCH (n)
        WHERE n.name IN ['A', 'B', 'C']
        RETURN stDev(n.age)
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 8. Functions
        //      section:    8.3. Aggregating functions
        //      subsection: 8.3.9. stDevP()
        // ---------------------------------------------------------------------

        MATCH (n)
        WHERE n.name IN ['A', 'B', 'C']
        RETURN stDevP(n.age)
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 8. Functions
        //      section:    8.3. Aggregating functions
        //      subsection: 8.3.10. sum()
        // ---------------------------------------------------------------------

        MATCH (n:Person)
        RETURN sum(n.age)
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 8. Functions
        //      section:    8.4. List functions
        //      subsection: 8.4.1. extract()
        // ---------------------------------------------------------------------

        MATCH p =(a)-->(b)-->(c)
        WHERE a.name = 'Alice' AND b.name = 'Bob' AND c.name = 'Daniel'
        RETURN extract(n IN nodes(p)| n.age) AS extracted
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 8. Functions
        //      section:    8.4. List functions
        //      subsection: 8.4.2. filter()
        // ---------------------------------------------------------------------

        MATCH (a)
        WHERE a.name = 'Eskil'
        RETURN a.array, filter(x IN a.array WHERE size(x)= 3)
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 8. Functions
        //      section:    8.4. List functions
        //      subsection: 8.4.3. keys()
        // ---------------------------------------------------------------------

        MATCH (a)
        WHERE a.name = 'Alice'
        RETURN keys(a)
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 8. Functions
        //      section:    8.4. List functions
        //      subsection: 8.4.4. labels()
        // ---------------------------------------------------------------------

        MATCH (a)
        WHERE a.name = 'Alice'
        RETURN labels(a)
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 8. Functions
        //      section:    8.4. List functions
        //      subsection: 8.4.5. nodes()
        // ---------------------------------------------------------------------

        MATCH p =(a)-->(b)-->(c)
        WHERE a.name = 'Alice' AND c.name = 'Eskil'
        RETURN nodes(p)
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 8. Functions
        //      section:    8.4. List functions
        //      subsection: 8.4.6. range()
        // ---------------------------------------------------------------------

        RETURN range(0, 10), range(2, 18, 3)
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 8. Functions
        //      section:    8.4. List functions
        //      subsection: 8.4.8. relationships()
        // ---------------------------------------------------------------------

        MATCH p =(a)-->(b)-->(c)
        WHERE a.name = 'Alice' AND c.name = 'Eskil'
        RETURN relationships(p)
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 8. Functions
        //      section:    8.4. List functions
        //      subsection: 8.4.9. reverse()
        // ---------------------------------------------------------------------

        WITH [4923,'abc',521, NULL , 487] AS ids
        RETURN reverse(ids)
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 8. Functions
        //      section:    8.4. List functions
        //      subsection: 8.4.10. tail()
        // ---------------------------------------------------------------------

        MATCH (a)
        WHERE a.name = 'Eskil'
        RETURN a.array, tail(a.array)
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 8. Functions
        //      section:    8.5. Mathematical functions - numeric
        //      subsection: 8.5.1. abs()
        // ---------------------------------------------------------------------

        MATCH (a),(e)
        WHERE a.name = 'Alice' AND e.name = 'Eskil'
        RETURN a.age, e.age, abs(a.age - e.age)
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 8. Functions
        //      section:    8.5. Mathematical functions - numeric
        //      subsection: 8.5.2. ceil()
        // ---------------------------------------------------------------------

        RETURN ceil(0.1)
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 8. Functions
        //      section:    8.5. Mathematical functions - numeric
        //      subsection: 8.5.3. floor()
        // ---------------------------------------------------------------------

        RETURN floor(0.9)
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 8. Functions
        //      section:    8.5. Mathematical functions - numeric
        //      subsection: 8.5.4. rand()
        // ---------------------------------------------------------------------

        RETURN rand()
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 8. Functions
        //      section:    8.5. Mathematical functions - numeric
        //      subsection: 8.5.5. round()
        // ---------------------------------------------------------------------

        RETURN round(3.141592)
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 8. Functions
        //      section:    8.5. Mathematical functions - numeric
        //      subsection: 8.5.6. sign()
        // ---------------------------------------------------------------------

        RETURN sign(-17), sign(0.1)
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 8. Functions
        //      section:    8.6. Mathematical functions - logarithmic
        //      subsection: 8.6.1. e()
        // ---------------------------------------------------------------------

        RETURN e()
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 8. Functions
        //      section:    8.6. Mathematical functions - logarithmic
        //      subsection: 8.6.2. exp()
        // ---------------------------------------------------------------------

        RETURN exp(2)
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 8. Functions
        //      section:    8.6. Mathematical functions - logarithmic
        //      subsection: 8.6.3. log()
        // ---------------------------------------------------------------------

        RETURN log(27)
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 8. Functions
        //      section:    8.6. Mathematical functions - logarithmic
        //      subsection: 8.6.4. log10()
        // ---------------------------------------------------------------------

        RETURN log10(27)
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 8. Functions
        //      section:    8.6. Mathematical functions - logarithmic
        //      subsection: 8.6.5. sqrt()
        // ---------------------------------------------------------------------

        RETURN sqrt(256)
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 8. Functions
        //      section:    8.7. Mathematical functions - trigonometric
        //      subsection: 8.7.1. acos()
        // ---------------------------------------------------------------------

        RETURN acos(0.5)
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 8. Functions
        //      section:    8.7. Mathematical functions - trigonometric
        //      subsection: 8.7.2. asin()
        // ---------------------------------------------------------------------

        RETURN asin(0.5)
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 8. Functions
        //      section:    8.7. Mathematical functions - trigonometric
        //      subsection: 8.7.3. atan()
        // ---------------------------------------------------------------------

        RETURN atan(0.5)
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 8. Functions
        //      section:    8.7. Mathematical functions - trigonometric
        //      subsection: 8.7.4. atan2()
        // ---------------------------------------------------------------------

        RETURN atan2(0.5, 0.6)
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 8. Functions
        //      section:    8.7. Mathematical functions - trigonometric
        //      subsection: 8.7.5. cos()
        // ---------------------------------------------------------------------

        RETURN cos(0.5)
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 8. Functions
        //      section:    8.7. Mathematical functions - trigonometric
        //      subsection: 8.7.6. cot()
        // ---------------------------------------------------------------------

        RETURN cot(0.5)
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 8. Functions
        //      section:    8.7. Mathematical functions - trigonometric
        //      subsection: 8.7.7. degrees()
        // ---------------------------------------------------------------------

        RETURN degrees(3.14159)
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 8. Functions
        //      section:    8.7. Mathematical functions - trigonometric
        //      subsection: 8.7.8. haversin()
        // ---------------------------------------------------------------------

        RETURN haversin(0.5)
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 8. Functions
        //      section:    8.7. Mathematical functions - trigonometric
        //      subsection: 8.7.9. Spherical distance using the haversin() function
        // ---------------------------------------------------------------------

        CREATE (ber:City { lat: 52.5, lon: 13.4 }),(sm:City { lat: 37.5, lon: -122.3 })
        RETURN 2 * 6371 * asin(sqrt(haversin(radians(sm.lat - ber.lat))+ cos(radians(sm.lat))*
        cos(radians(ber.lat))* haversin(radians(sm.lon - ber.lon)))) AS dist
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 8. Functions
        //      section:    8.7. Mathematical functions - trigonometric
        //      subsection: 8.7.10. pi()
        // ---------------------------------------------------------------------

        RETURN pi()
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 8. Functions
        //      section:    8.7. Mathematical functions - trigonometric
        //      subsection: 8.7.11. radians()
        // ---------------------------------------------------------------------

        RETURN radians(180)
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 8. Functions
        //      section:    8.7. Mathematical functions - trigonometric
        //      subsection: 8.7.12. sin()
        // ---------------------------------------------------------------------

        RETURN sin(0.5)
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 8. Functions
        //      section:    8.7. Mathematical functions - trigonometric
        //      subsection: 8.7.13. tan()
        // ---------------------------------------------------------------------

        RETURN tan(0.5)
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 8. Functions
        //      section:    8.8. String functions
        //      subsection: 8.8.1. left()
        // ---------------------------------------------------------------------

        RETURN left('hello', 3)
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 8. Functions
        //      section:    8.8. String functions
        //      subsection: 8.8.2. ltrim()
        // ---------------------------------------------------------------------

        RETURN lTrim(' hello')
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 8. Functions
        //      section:    8.8. String functions
        //      subsection: 8.8.3. replace()
        // ---------------------------------------------------------------------

        RETURN replace(\"hello\", \"l\", \"w\")
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 8. Functions
        //      section:    8.8. String functions
        //      subsection: 8.8.4. reverse()
        // ---------------------------------------------------------------------

        RETURN reverse('anagram')
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 8. Functions
        //      section:    8.8. String functions
        //      subsection: 8.8.5. right()
        // ---------------------------------------------------------------------

        RETURN right('hello', 3)
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 8. Functions
        //      section:    8.8. String functions
        //      subsection: 8.8.6. rtrim()
        // ---------------------------------------------------------------------

        RETURN rTrim('hello ')
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 8. Functions
        //      section:    8.8. String functions
        //      subsection: 8.8.7. split()
        // ---------------------------------------------------------------------

        RETURN split('one,two', ',')
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 8. Functions
        //      section:    8.8. String functions
        //      subsection: 8.8.8. substring()
        // ---------------------------------------------------------------------

        RETURN substring('hello', 1, 3), substring('hello', 2)
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 8. Functions
        //      section:    8.8. String functions
        //      subsection: 8.8.9. toLower()
        // ---------------------------------------------------------------------

        RETURN toLower('HELLO')
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 8. Functions
        //      section:    8.8. String functions
        //      subsection: 8.8.10. toString()
        // ---------------------------------------------------------------------

        RETURN toString(11.5), toString('already a string'), toString(TRUE )
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 8. Functions
        //      section:    8.8. String functions
        //      subsection: 8.8.11. toUpper()
        // ---------------------------------------------------------------------

        RETURN toUpper('hello')
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 8. Functions
        //      section:    8.8. String functions
        //      subsection: 8.8.12. trim()
        // ---------------------------------------------------------------------

        RETURN trim(' hello ')
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 8. Functions
        //      section:    8.9. Spatial functions
        //      subsection: 8.9.1. distance()
        // ---------------------------------------------------------------------

        WITH point({ x: 2.3, y: 4.5, crs: 'cartesian' }) AS p1, point({ x: 1.1, y: 5.4, crs: 'cartesian' }) AS p2
        RETURN distance(p1,p2) AS dist
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 8. Functions
        //      section:    8.9. Spatial functions
        //      subsection: 8.9.1. distance()
        // ---------------------------------------------------------------------

        MATCH (t:TrainStation)-[:TRAVEL_ROUTE]->(o:Office)
        WITH point({ longitude: t.longitude, latitude: t.latitude }) AS trainPoint, point({ longitude:
        o.longitude, latitude: o.latitude }) AS officePoint
        RETURN round(distance(trainPoint, officePoint)) AS travelDistance
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 8. Functions
        //      section:    8.9. Spatial functions
        //      subsection: 8.9.1. distance()
        // ---------------------------------------------------------------------

        RETURN distance(NULL , point({ longitude: 56.7, latitude: 12.78 })) AS d
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 8. Functions
        //      section:    8.9. Spatial functions
        //      subsection: 8.9.2. point() - WGS 84
        // ---------------------------------------------------------------------

        RETURN point({ longitude: 56.7, latitude: 12.78 }) AS point
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 8. Functions
        //      section:    8.9. Spatial functions
        //      subsection: 8.9.2. point() - WGS 84
        // ---------------------------------------------------------------------

        RETURN point({ x: 2.3, y: 4.5, crs: 'WGS-84' }) AS point
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 8. Functions
        //      section:    8.9. Spatial functions
        //      subsection: 8.9.2. point() - WGS 84
        // ---------------------------------------------------------------------

        MATCH (p:Office)
        RETURN point({ longitude: p.longitude, latitude: p.latitude }) AS officePoint
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 8. Functions
        //      section:    8.9. Spatial functions
        //      subsection: 8.9.2. point() - WGS 84
        // ---------------------------------------------------------------------

        RETURN point(NULL ) AS p
    "),
    {ok, _} = ocparse:source_to_pt("
        // =====================================================================
        // from chapter:    Chapter 8. Functions
        //      section:    8.9. Spatial functions
        //      subsection: 8.9.3. point() - cartesian 2D
        // ---------------------------------------------------------------------

        RETURN point({ x: 2.3, y: 4.5 }) AS point
    "),
    ok.
