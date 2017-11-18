%% -----------------------------------------------------------------------------
%%
%% ocparse_generator.erl: opencypher - test data generator.
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

-define(ALL_CLAUSE_PERFORMANCE, [
    cypher,
    referenceExamples,
    special
]).
-define(ALL_CLAUSE_RELIABILITY, [
    create,
    cypher,
    merge,
    multiPartQuery,
    readOnlyEnd,
    readUpdateEnd,
    regularQuery,
    return,
    standaloneCall,
    updatingEnd
]).
-define(ALL_CLAUSE_RELIABILITY_DETAILED, [
%%%% Level 01 ..........................
%%    decimalInteger,
%%    escapedSymbolicName,
%%    exponentDecimalReal,
%%    hexInteger,
%%    hexLetter,
%%    octalInteger,
%%    propertyKeyName,
%%    regularDecimalReal,
%%    relTypeName,
%%    reservedWord,
%%    symbolicName,
%%    unescapedSymbolicName,
%%%% Level 02 ..........................
%%    nodeLabel,
%%    procedureName,
%%    propertyLookup,
%%    rangeLiteral,
%%    relationshipTypes,
%%    yieldItem,
%%%% Level 03 ..........................
%%    nodeLabels,
%%    yieldItems,
%%%% Level 04 ..........................
%%    removeItem,
%%%% Level 05 ..........................
%%    remove,
%%%% Level 11 ..........................
%%    addOrSubtractExpression,
%%    andExpression,
%%    comparisonExpression,
%%    expressionCommalist,
%%    multiplyDivideModuloExpression,
%%    notExpression,
%%    orExpression,
%%    partialComparisonExpression,
%%    powerOfExpression,
%%    propertyOrLabelsExpression,
%%    stringListNullOperatorExpression,
%%    unaryAddOrSubtractExpression,
%%    xorExpression,
%%%% ...................................
%%    expression,
%%%% Level 12 ..........................
%%    caseAlternatives,
%%    delete,
%%    explicitProcedureInvocation,
%%    idInColl,
%%    limit,
%%    nodePattern,
%%    propertyExpression,
%%    relationshipDetail,
%%    returnItem,
%%    skip,
%%    sortItem,
%%    unwind,
%%    where,
%%%% Level 13 ..........................
%%    caseAlternativesList,
%%    inQueryCall,
%%    order,
%%    relationshipPattern,
%%    returnItems,
%%    setItem,
%%    standaloneCall,
%%%% Level 14 ..........................
%%    patternElementChain,
%%    patternElementChainList,
%%    returnBody,
%%    set,
%%%% Level 15 ..........................
%%    mergeAction,
%%    patternElement,
%%    return,
%%    with,
%%%% Level 16 ..........................
%%    patternPart,
%%%% Level 17 ..........................
%%    merge,
%%    pattern,
%%%% Level 18 ..........................
%%    create,
%%    match,
%%    readPart,
%%    updatingPart,
%%%% Level 19 ..........................
%%    readOnlyEnd,
%%    readPartUpdatingPartWithList,
%%    readUpdateEnd,
%%    updatingEnd,
%%%% Level 20 ..........................
%%    multiPartQuery,
%%%% Level 21 ..........................
%%    union,
%%%% Level 22 ..........................
%%    regularQuery,
%%%% Level 23 ..........................
%%    cypher,
%%%% Level 24 ..........................
%%    caseExpression,
%%    filterExpression,
%%    functionInvocation,
%%    listComprehension,
%%%% ...................................
%%    booleanLiteral,
%%    listLiteral,
%%    mapLiteral,
%%    numberLiteral,
%%    stringLiteral,
%%%% ...................................
%%    literal,
%%    parameter,
%%    parenthesizedExpression,
%%    patternComprehension,
%%    relationshipsPattern,
%%    variable,
%%%% ...................................
%%    atom,
%% Level 25 ..........................
    referenceExamples,
    special
]).

-define(CODE_TEMPLATES, code_templates).
-define(CREATE_CODE_END,
    [_CodeFirst | _] = Code,
    {_, _MemorySize} = erlang:process_info(self(), memory),
    ?debugFmt("~ntime (ms)          ===  ~12.. B rule: ~s ~n", [erlang:monotonic_time(1000) - _Start, atom_to_list(Rule)]),
    ?debugFmt("~nmemory (bytes)     ===  ~12.. B rule: ~s ~n", [_MemorySize, atom_to_list(Rule)]),
    ?debugFmt("~ncode size (bytes) <===  ~12.. B rule: ~s ~n", [length(_CodeFirst), atom_to_list(Rule)]),
    ok
).
-define(CREATE_CODE_START,
    [garbage_collect(Pid) || Pid <- processes()],
    _Start = erlang:monotonic_time(1000)
).
-define(DASH, "-").

-define(F_RANDOM, fun(X, Y) -> erlang:phash2(X) < erlang:phash2(Y) end).

-define(GENERATE_COMPACTED, true).                         % true: compacted / false: detailed.
-define(GENERATE_CT, true).
-define(GENERATE_EUNIT, false).
-define(GENERATE_PERFORMANCE, true).

-define(LEFT_ARROW_HEAD, "<").

%-define(MAX_BASIC_RULE, 250).
-define(MAX_BASIC_RULE, 100).
-define(MAX_CYPHER, ?MAX_BASIC_RULE * 10).

-define(PATH_CT, "test").
-define(PATH_EUNIT, "test").

-define(RIGHT_ARROW_HEAD, ">").
-define(SP, " ").
-define(SP_OPT, []).

-define(TESTS_FROM_NEO4J_V3_3, [
    % ==========================================================================
    % Tuple structure: {chapter, section, subsection, completion, code}
    % ==========================================================================
    % Chapter 4. Get started with Cypher
    % --------------------------------------------------------------------------
    {"Chapter 4. Get started with Cypher", "4.1. Patterns", "",
        "
        (:Person) -[:LIVES_IN]-> (:City) -[:PART_OF]-> (:Country)
    "},
    % --------------------------------------------------------------------------
    {"Chapter 4. Get started with Cypher", "4.1. Patterns", "4.1.1. Node syntax", nodePattern,
        "
        ()
    "},
    % --------------------------------------------------------------------------
    {"Chapter 4. Get started with Cypher", "4.1. Patterns", "4.1.1. Node syntax", nodePattern,
        "
        (matrix)
    "},
    % --------------------------------------------------------------------------
    {"Chapter 4. Get started with Cypher", "4.1. Patterns", "4.1.1. Node syntax", nodePattern,
        "
        (:Movie)
    "},
    % --------------------------------------------------------------------------
    {"Chapter 4. Get started with Cypher", "4.1. Patterns", "4.1.1. Node syntax", nodePattern,
        "
        (matrix:Movie)
    "},
    % --------------------------------------------------------------------------
    {"Chapter 4. Get started with Cypher", "4.1. Patterns", "4.1.1. Node syntax", nodePattern,
        "
        (matrix:Movie {title: \"The Matrix\"})
    "},
    % --------------------------------------------------------------------------
    {"Chapter 4. Get started with Cypher", "4.1. Patterns", "4.1.1. Node syntax", nodePattern,
        "
        (matrix:Movie {title: \"The Matrix\", released: 1997})
    "},
    % --------------------------------------------------------------------------
    {"Chapter 4. Get started with Cypher", "4.1. Patterns", "4.1.2. Relationship syntax", relationshipPattern,
        "
        -->
    "},
    % --------------------------------------------------------------------------
    {"Chapter 4. Get started with Cypher", "4.1. Patterns", "4.1.2. Relationship syntax", relationshipPattern,
        "
        -[role]->
    "},
    % --------------------------------------------------------------------------
    {"Chapter 4. Get started with Cypher", "4.1. Patterns", "4.1.2. Relationship syntax", relationshipPattern,
        "
        -[:ACTED_IN]->
    "},
    % --------------------------------------------------------------------------
    {"Chapter 4. Get started with Cypher", "4.1. Patterns", "4.1.2. Relationship syntax", relationshipPattern,
        "
        -[role:ACTED_IN]->
    "},
    % --------------------------------------------------------------------------
    {"Chapter 4. Get started with Cypher", "4.1. Patterns", "4.1.2. Relationship syntax", relationshipPattern,
        "
        -[role:ACTED_IN {roles: [\"Neo\"]}]->
    "},
    % --------------------------------------------------------------------------
    {"Chapter 4. Get started with Cypher", "4.1. Patterns", "4.1.3. Pattern syntax", pattern,
        "
        (keanu:Person:Actor {name: \"Keanu Reeves\"} )
    "},
    % --------------------------------------------------------------------------
    {"Chapter 4. Get started with Cypher", "4.1. Patterns", "4.1.3. Pattern syntax", relationshipPattern,
        "
        -[role:ACTED_IN {roles: [\"Neo\"] } ]->
    "},
    % --------------------------------------------------------------------------
    {"Chapter 4. Get started with Cypher", "4.1. Patterns", "4.1.3. Pattern syntax", pattern,
        "
        (matrix:Movie {title: \"The Matrix\"} )
    "},
    % --------------------------------------------------------------------------
    {"Chapter 4. Get started with Cypher", "4.1. Patterns", "4.1.4. Pattern variables", pattern,
        "
        acted_in = (:Person)-[:ACTED_IN]->(:Movie)
    "},
    % --------------------------------------------------------------------------
    {"Chapter 4. Get started with Cypher", "4.2. Patterns in practice", "4.2.1. Creating data", cypher,
        "
        CREATE (:Movie { title:\"The Matrix\",released:1997 })
    "},
    % --------------------------------------------------------------------------
    {"Chapter 4. Get started with Cypher", "4.2. Patterns in practice", "4.2.1. Creating data", cypher,
        "
        CREATE (p:Person { name:\"Keanu Reeves\", born:1964 })
        RETURN p
    "},
    % --------------------------------------------------------------------------
    {"Chapter 4. Get started with Cypher", "4.2. Patterns in practice", "4.2.1. Creating data", cypher,
        "
        CREATE (a:Person { name:\"Tom Hanks\",
          born:1956 })-[r:ACTED_IN { roles: [\"Forrest\"]}]->(m:Movie { title:\"Forrest Gump\",released:1994 })
        CREATE (d:Person { name:\"Robert Zemeckis\", born:1951 })-[:DIRECTED]->(m)
        RETURN a,d,r,m
    "},
    % --------------------------------------------------------------------------
    {"Chapter 4. Get started with Cypher", "4.2. Patterns in practice", "4.2.2. Matching patterns", cypher,
        "
        MATCH (m:Movie)
        RETURN m
    "},
    % --------------------------------------------------------------------------
    {"Chapter 4. Get started with Cypher", "4.2. Patterns in practice", "4.2.2. Matching patterns", cypher,
        "
        MATCH (p:Person { name:\"Keanu Reeves\" })
        RETURN p
    "},
    % --------------------------------------------------------------------------
    {"Chapter 4. Get started with Cypher", "4.2. Patterns in practice", "4.2.2. Matching patterns", cypher,
        "
        MATCH (p:Person { name:\"Tom Hanks\" })-[r:ACTED_IN]->(m:Movie)
        RETURN m.title, r.roles
    "},
    % --------------------------------------------------------------------------
    {"Chapter 4. Get started with Cypher", "4.2. Patterns in practice", "4.2.3. Attaching structures", cypher,
        "
        MATCH (p:Person { name:\"Tom Hanks\" })
        CREATE (m:Movie { title:\"Cloud Atlas\",released:2012 })
        CREATE (p)-[r:ACTED_IN { roles: ['Zachry']}]->(m)
        RETURN p,r,m
    "},
    % --------------------------------------------------------------------------
    {"Chapter 4. Get started with Cypher", "4.2. Patterns in practice", "4.2.4. Completing patterns", cypher,
        "
        MERGE (m:Movie { title:\"Cloud Atlas\" })
        ON CREATE SET m.released = 2012
        RETURN m
    "},
    % --------------------------------------------------------------------------
    {"Chapter 4. Get started with Cypher", "4.2. Patterns in practice", "4.2.4. Completing patterns", cypher,
        "
        MATCH (m:Movie { title:\"Cloud Atlas\" })
        MATCH (p:Person { name:\"Tom Hanks\" })
        MERGE (p)-[r:ACTED_IN]->(m)
        ON CREATE SET r.roles =['Zachry']
        RETURN p,r,m
    "},
    % --------------------------------------------------------------------------
    {"Chapter 4. Get started with Cypher", "4.2. Patterns in practice", "4.2.4. Completing patterns", cypher,
        "
        CREATE (y:Year { year:2014 })
        MERGE (y)<-[:IN_YEAR]-(m10:Month { month:10 })
        MERGE (y)<-[:IN_YEAR]-(m11:Month { month:11 })
        RETURN y,m10,m11
    "},
    % --------------------------------------------------------------------------
    {"Chapter 4. Get started with Cypher", "4.3. Getting correct results", "", cypher,
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
    "},
    % --------------------------------------------------------------------------
    {"Chapter 4. Get started with Cypher", "4.3. Getting correct results", "4.3.1. Filtering results", cypher,
        "
        MATCH (m:Movie)
        WHERE m.title = \"The Matrix\"
        RETURN m
    "},
    % --------------------------------------------------------------------------
    {"Chapter 4. Get started with Cypher", "4.3. Getting correct results", "4.3.1. Filtering results", cypher,
        "
        MATCH (p:Person)-[r:ACTED_IN]->(m:Movie)
        WHERE p.name =~ \"K.+\" OR m.released > 2000 OR \"Neo\" IN r.roles
        RETURN p,r,m
    "},
    % --------------------------------------------------------------------------
    {"Chapter 4. Get started with Cypher", "4.3. Getting correct results", "4.3.1. Filtering results", cypher,
        "
        MATCH (p:Person)-[:ACTED_IN]->(m)
        WHERE NOT (p)-[:DIRECTED]->()
        RETURN p,m
    "},
    % --------------------------------------------------------------------------
    {"Chapter 4. Get started with Cypher", "4.3. Getting correct results", "4.3.2. Returning results", cypher,
        "
        MATCH (p:Person)
        RETURN p, p.name AS name, toUpper(p.name), coalesce(p.nickname,\"n/a\") AS nickname, { name: p.name,
          label:head(labels(p))} AS person
    "},
    % --------------------------------------------------------------------------
    {"Chapter 4. Get started with Cypher", "4.3. Getting correct results", "4.3.3. Aggregating information", cypher,
        "
        MATCH (:Person)
        RETURN count(*) AS people
    "},
    % --------------------------------------------------------------------------
    {"Chapter 4. Get started with Cypher", "4.3. Getting correct results", "4.3.3. Aggregating information", cypher,
        "
        MATCH (actor:Person)-[:ACTED_IN]->(movie:Movie)<-[:DIRECTED]-(director:Person)
        RETURN actor,director,count(*) AS collaborations
    "},
    % --------------------------------------------------------------------------
    {"Chapter 4. Get started with Cypher", "4.3. Getting correct results", "4.3.4. Ordering and pagination", cypher,
        "
        MATCH (a:Person)-[:ACTED_IN]->(m:Movie)
        RETURN a,count(*) AS appearances
        ORDER BY appearances DESC LIMIT 10;
    "},
    % --------------------------------------------------------------------------
    {"Chapter 4. Get started with Cypher", "4.3. Getting correct results", "4.3.5. Collecting aggregation", cypher,
        "
        MATCH (m:Movie)<-[:ACTED_IN]-(a:Person)
        RETURN m.title AS movie, collect(a.name) AS cast, count(*) AS actors
    "},
    % --------------------------------------------------------------------------
    {"Chapter 4. Get started with Cypher", "4.4. Composing large statements", "", cypher,
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
    "},
    % --------------------------------------------------------------------------
    {"Chapter 4. Get started with Cypher", "4.4. Composing large statements", "4.4.1. UNION", cypher,
        "
        MATCH (actor:Person)-[r:ACTED_IN]->(movie:Movie)
        RETURN actor.name AS name, type(r) AS acted_in, movie.title AS title
        UNION
        MATCH (director:Person)-[r:DIRECTED]->(movie:Movie)
        RETURN director.name AS name, type(r) AS acted_in, movie.title AS title
    "},
    % --------------------------------------------------------------------------
    {"Chapter 4. Get started with Cypher", "4.4. Composing large statements", "4.4.2. WITH", cypher,
        "
        MATCH (person:Person)-[:ACTED_IN]->(m:Movie)
        WITH person, count(*) AS appearances, collect(m.title) AS movies
        WHERE appearances > 1
        RETURN person.name, appearances, movies
    "},
%% wwe legacy - CONSTRAINT / INDEX
%%    % --------------------------------------------------------------------------
%%    {"Chapter 4. Get started with Cypher", "4.5. Constraints and indexes", "4.5.1. Using constraints", cypher,
%%        "
%%        CREATE CONSTRAINT ON (movie:Movie) ASSERT movie.title IS UNIQUE
%%    "},
%%    % --------------------------------------------------------------------------
%%    {"Chapter 4. Get started with Cypher", "4.5. Constraints and indexes", "4.5.2. Using indexes", cypher,
%%        "
%%        CREATE INDEX ON :Actor(name)
%%    "},
    % --------------------------------------------------------------------------
    {"Chapter 4. Get started with Cypher", "4.5. Constraints and indexes", "4.5.2. Using indexes", cypher,
        "
        CREATE (actor:Actor { name:\"Tom Hanks\" }),(movie:Movie { title:'Sleepless IN Seattle' }),
          (actor)-[:ACTED_IN]->(movie);
    "},
    % --------------------------------------------------------------------------
    {"Chapter 4. Get started with Cypher", "4.5. Constraints and indexes", "4.5.2. Using indexes", cypher,
        "
        MATCH (actor:Actor { name: \"Tom Hanks\" })
        RETURN actor;
    "},
    % --------------------------------------------------------------------------
    {"Chapter 5. Introduction", "5.1. What is Cypher?", "", cypher,
        "
        MATCH (john {name: 'John'})-[:friend]->()-[:friend]->(fof)
        RETURN john.name, fof.name
    "},
    % --------------------------------------------------------------------------
    {"Chapter 5. Introduction", "5.1. What is Cypher?", "", cypher,
        "
        MATCH (user)-[:friend]->(follower)
        WHERE user.name IN ['Joe', 'John', 'Sara', 'Maria', 'Steve'] AND follower.name =~ 'S.*'
        RETURN user.name, follower.name
    "},
    % --------------------------------------------------------------------------
    {"Chapter 5. Introduction", "5.2. Querying and updating the graph", "5.2.1. The structure of update queries", cypher,
        "
        MATCH (n {name: 'John'})-[:FRIEND]-(friend)
        WITH n, count(friend) AS friendsCount
        WHERE friendsCount > 3
        RETURN n, friendsCount
    "},
%% wwe - open issue
%%    % --------------------------------------------------------------------------
%%    {"Chapter 5. Introduction", "5.2. Querying and updating the graph", "5.2.1. The structure of update queries", cypher,
%%        "
%%        MATCH (n {name: 'John'})-[:FRIEND]-(friend)
%%        WITH n, count(friend) AS friendsCount
%%        SET n.friendsCount = friendsCount
%%        RETURN n.friendsCount
%%    "},
    % --------------------------------------------------------------------------
    {"Chapter 5. Introduction", "5.4. Uniqueness", "", cypher,
        "
        CREATE (adam:User { name: 'Adam' }),(pernilla:User { name: 'Pernilla' }),(david:User { name: 'David'
          }),
          (adam)-[:FRIEND]->(pernilla),(pernilla)-[:FRIEND]->(david)
    "},
    % --------------------------------------------------------------------------
    {"Chapter 5. Introduction", "5.4. Uniqueness", "", cypher,
        "
        MATCH (user:User { name: 'Adam' })-[r1:FRIEND]-()-[r2:FRIEND]-(friend_of_a_friend)
        RETURN friend_of_a_friend.name AS fofName
    "},
    % --------------------------------------------------------------------------
    {"Chapter 5. Introduction", "5.4. Uniqueness", "", cypher,
        "
        MATCH (user:User { name: 'Adam' })-[r1:FRIEND]-(friend)
        MATCH (friend)-[r2:FRIEND]-(friend_of_a_friend)
        RETURN friend_of_a_friend.name AS fofName
    "},
    % --------------------------------------------------------------------------
    {"Chapter 5. Introduction", "5.4. Uniqueness", "", cypher,
        "
        MATCH (user:User { name: 'Adam' })-[r1:FRIEND]-(friend),(friend)-[r2:FRIEND]-(friend_of_a_friend)
        RETURN friend_of_a_friend.name AS fofName
    "},
    % --------------------------------------------------------------------------
    {"Chapter 6. Syntax", "6.2. Expressions", "6.2.3. CASE expressions", cypher,
        "
        MATCH (n)
        RETURN
        CASE n.eyes
        WHEN 'blue'
        THEN 1
        WHEN 'brown'
        THEN 2
        ELSE 3 END AS result
    "},
    % --------------------------------------------------------------------------
    {"Chapter 6. Syntax", "6.2. Expressions", "6.2.3. CASE expressions", cypher,
        "
        MATCH (n)
        RETURN
        CASE
        WHEN n.eyes = 'blue'
        THEN 1
        WHEN n.age < 40
        THEN 2
        ELSE 3 END AS result
    "},
    % --------------------------------------------------------------------------
    {"Chapter 6. Syntax", "6.2. Expressions", "6.2.3. CASE expressions", cypher,
        "
        MATCH (n)
        RETURN n.name,
        CASE n.age
        WHEN n.age IS NULL THEN -1
        ELSE n.age - 10 END AS age_10_years_ago
    "},
    % --------------------------------------------------------------------------
    {"Chapter 6. Syntax", "6.2. Expressions", "6.2.3. CASE expressions", cypher,
        "
        MATCH (n)
        RETURN n.name,
        CASE
        WHEN n.age IS NULL THEN -1
        ELSE n.age - 10 END AS age_10_years_ago
    "},
    % --------------------------------------------------------------------------
    {"Chapter 6. Syntax", "6.3. Variables", "", cypher,
        "
        MATCH (n)-->(b)
        RETURN b
    "},
    % --------------------------------------------------------------------------
    {"Chapter 6. Syntax", "6.4. Parameters", "6.4.2. String literal", cypher,
        "
        MATCH (n:Person)
        WHERE n.name = $name
        RETURN n
    "},
    % --------------------------------------------------------------------------
    {"Chapter 6. Syntax", "6.4. Parameters", "6.4.2. String literal", cypher,
        "
        MATCH (n:Person { name: $name })
        RETURN n
    "},
    % --------------------------------------------------------------------------
    {"Chapter 6. Syntax", "6.4. Parameters", "6.4.3. Regular expression", cypher,
        "
        MATCH (n:Person)
        WHERE n.name =~ $regex
        RETURN n.name
    "},
    % --------------------------------------------------------------------------
    {"Chapter 6. Syntax", "6.4. Parameters", "6.4.4. Case-sensitive string pattern matching", cypher,
        "
        MATCH (n:Person)
        WHERE n.name STARTS WITH $name
        RETURN n.name
    "},
    % --------------------------------------------------------------------------
    {"Chapter 6. Syntax", "6.4. Parameters", "6.4.5. Create node with properties", cypher,
        "
        CREATE ($props)
    "},
    % --------------------------------------------------------------------------
    {"Chapter 6. Syntax", "6.4. Parameters", "6.4.6. Create multiple nodes with properties", cypher,
        "
        UNWIND $props AS properties
        CREATE (n:Person)
        SET n = properties
        RETURN n
    "},
    % --------------------------------------------------------------------------
    {"Chapter 6. Syntax", "6.4. Parameters", "6.4.7. Setting all properties on a node", cypher,
        "
        MATCH (n:Person)
        WHERE n.name='Michaela'
        SET n = $props
    "},
    % --------------------------------------------------------------------------
    {"Chapter 6. Syntax", "6.4. Parameters", "6.4.8. SKIP and LIMIT", cypher,
        "
        MATCH (n:Person)
        RETURN n.name
        SKIP $s
        LIMIT $l
    "},
    % --------------------------------------------------------------------------
    {"Chapter 6. Syntax", "6.4. Parameters", "6.4.9. Node id", cypher,
        "
        MATCH (n)
        WHERE id(n)= $id
        RETURN n.name
    "},
    % --------------------------------------------------------------------------
    {"Chapter 6. Syntax", "6.4. Parameters", "6.4.10. Multiple node ids", cypher,
        "
        MATCH (n)
        WHERE id(n) IN $ids
        RETURN n.name
    "},
    % --------------------------------------------------------------------------
    {"Chapter 6. Syntax", "6.4. Parameters", "6.4.11. Calling procedures", cypher,
        "
        CALL db.resampleIndex($indexname)
    "},
%% wwe legacy - START
%%    % --------------------------------------------------------------------------
%%    {"Chapter 6. Syntax", "6.4. Parameters", "6.4.12. Index value (explicit indexes)", cypher,
%%        "
%%        START n=node:people(name = $value)
%%        RETURN n
%%    "},
    % --------------------------------------------------------------------------
%%    {"Chapter 6. Syntax", "6.4. Parameters", "6.4.13. Index query (explicit indexes)", cypher,
%%        "
%%        START n=node:people($query)
%%        RETURN n
%%    "},
    % --------------------------------------------------------------------------
    {"Chapter 6. Syntax", "6.5. Operators", "6.5.2. General operators", cypher,
        "
        CREATE (a:Person { name: 'Anne', eyeColor: 'blue' }),(b:Person { name: 'Bill', eyeColor: 'brown'
        }),(c:Person { name: 'Carol', eyeColor: 'blue' })
        WITH [a, b, c] AS ps
        UNWIND ps AS p
        RETURN DISTINCT p.eyeColor
    "},
    % --------------------------------------------------------------------------
    {"Chapter 6. Syntax", "6.5. Operators", "6.5.2. General operators", cypher,
        "
        WITH { person: { name: 'Anne', age: 25 }} AS p
        RETURN p.person.name
    "},
    % --------------------------------------------------------------------------
    {"Chapter 6. Syntax", "6.5. Operators", "6.5.2. General operators", cypher,
        "
        CREATE (a:Restaurant { name: 'Hungry Jo', rating_hygiene: 10, rating_food: 7 }),(b:Restaurant { name:
        'Buttercup Tea Rooms', rating_hygiene: 5, rating_food: 6 }),(c1:Category { name: 'hygiene' }),(c2:Category
        { name: 'food' })
        WITH a, b, c1, c2
        MATCH (restaurant:Restaurant),(category:Category)
        WHERE restaurant[\"rating_\" + category.name]> 6
        RETURN DISTINCT restaurant.name
    "},
    % --------------------------------------------------------------------------
    {"Chapter 6. Syntax", "6.5. Operators", "6.5.3. Mathematical operators", cypher,
        "
        WITH 2 AS number, 3 AS exponent
        RETURN number ^ exponent AS result
    "},
    % --------------------------------------------------------------------------
    {"Chapter 6. Syntax", "6.5. Operators", "6.5.3. Mathematical operators", cypher,
        "
        WITH -3 AS a, 4 AS b
        RETURN b - a AS result
    "},
    % --------------------------------------------------------------------------
    {"Chapter 6. Syntax", "6.5. Operators", "6.5.4. Comparison operators", cypher,
        "
        WITH 4 AS one, 3 AS two
        RETURN one > two AS result
    "},
    % --------------------------------------------------------------------------
    {"Chapter 6. Syntax", "6.5. Operators", "6.5.4. Comparison operators", cypher,
        "
        WITH ['John', 'Mark', 'Jonathan', 'Bill'] AS somenames
        UNWIND somenames AS names
        WITH names AS candidate
        WHERE candidate STARTS WITH 'Jo'
        RETURN candidate
    "},
    % --------------------------------------------------------------------------
    {"Chapter 6. Syntax", "6.5. Operators", "6.5.5. Boolean operators", cypher,
        "
        WITH [2, 4, 7, 9, 12] AS numberlist
        UNWIND numberlist AS number
        WITH number
        WHERE number = 4 OR (number > 6 AND number < 10)
        RETURN number
    "},
    % --------------------------------------------------------------------------
    {"Chapter 6. Syntax", "6.5. Operators", "6.5.6. String operators", cypher,
        "
        WITH ['mouse', 'chair', 'door', 'house'] AS wordlist
        UNWIND wordlist AS word
        WITH word
        WHERE word =~ '.*ous.*'
        RETURN word
    "},
    % --------------------------------------------------------------------------
    {"Chapter 6. Syntax", "6.5. Operators", "6.5.7. List operators", cypher,
        "
        RETURN [1,2,3,4,5]+[6,7] AS myList
    "},
    % --------------------------------------------------------------------------
    {"Chapter 6. Syntax", "6.5. Operators", "6.5.7. List operators", cypher,
        "
        WITH [2, 3, 4, 5] AS numberlist
        UNWIND numberlist AS number
        WITH number
        WHERE number IN [2, 3, 8]
        RETURN number
    "},
    % --------------------------------------------------------------------------
    {"Chapter 6. Syntax", "6.5. Operators", "6.5.7. List operators", cypher,
        "
        WITH ['Anne', 'John', 'Bill', 'Diane', 'Eve'] AS names
        RETURN names[1..3] AS result
    "},
    % --------------------------------------------------------------------------
    {"Chapter 6. Syntax", "6.5. Operators", "6.5.11. Chaining comparison operations", cypher,
        "
        MATCH (n) WHERE 21 < n.age <= 30 RETURN n
    "},
    % --------------------------------------------------------------------------
    {"Chapter 6. Syntax", "6.5. Operators", "6.5.11. Chaining comparison operations", cypher,
        "
        MATCH (n) WHERE 21 < n.age AND n.age <= 30 RETURN n
    "},
    % --------------------------------------------------------------------------
    {"Chapter 6. Syntax", "6.6. Comments", "", cypher,
        "
        MATCH (n) RETURN n //This is an end of line comment
    "},
    % --------------------------------------------------------------------------
    {"Chapter 6. Syntax", "6.6. Comments", "", cypher,
        "
        MATCH (n)
        //This is a whole line comment
        RETURN n
    "},
    % --------------------------------------------------------------------------
    {"Chapter 6. Syntax", "6.7. Patterns", "6.7.2. Patterns for nodes", pattern,
        "
        (a)
    "},
    % --------------------------------------------------------------------------
    {"Chapter 6. Syntax", "6.7. Patterns", "6.7.3. Patterns for related nodes", pattern,
        "
        (a)-->(b)
    "},
    % --------------------------------------------------------------------------
    {"Chapter 6. Syntax", "6.7. Patterns", "6.7.3. Patterns for related nodes", pattern,
        "
        (a)-->(b)<--(c)
    "},
    % --------------------------------------------------------------------------
    {"Chapter 6. Syntax", "6.7. Patterns", "6.7.4. Patterns for labels", pattern,
        "
        (a:User)-->(b)
    "},
    % --------------------------------------------------------------------------
    {"Chapter 6. Syntax", "6.7. Patterns", "6.7.4. Patterns for labels", pattern,
        "
        (a:User:Admin)-->(b)
    "},
    % --------------------------------------------------------------------------
    {"Chapter 6. Syntax", "6.7. Patterns", "6.7.5. Specifying properties", nodePattern,
        "
        (a {name: 'Andres', sport: 'Brazilian Ju-Jitsu'})
    "},
    % --------------------------------------------------------------------------
    {"Chapter 6. Syntax", "6.7. Patterns", "6.7.5. Specifying properties", pattern,
        "
        (a)-[{blocked: false}]->(b)
    "},
    % --------------------------------------------------------------------------
    {"Chapter 6. Syntax", "6.7. Patterns", "6.7.6. Patterns for relationships", pattern,
        "
        (a)--(b)
    "},
    % --------------------------------------------------------------------------
    {"Chapter 6. Syntax", "6.7. Patterns", "6.7.6. Patterns for relationships", pattern,
        "
        (a)-[r]->(b)
    "},
    % --------------------------------------------------------------------------
    {"Chapter 6. Syntax", "6.7. Patterns", "6.7.6. Patterns for relationships", pattern,
        "
        (a)-[r:REL_TYPE]->(b)
    "},
    % --------------------------------------------------------------------------
    {"Chapter 6. Syntax", "6.7. Patterns", "6.7.6. Patterns for relationships", pattern,
        "
        (a)-[r:TYPE1|TYPE2]->(b)
    "},
    % --------------------------------------------------------------------------
    {"Chapter 6. Syntax", "6.7. Patterns", "6.7.6. Patterns for relationships", pattern,
        "
        (a)-[:REL_TYPE]->(b)
    "},
    % --------------------------------------------------------------------------
    {"Chapter 6. Syntax", "6.7. Patterns", "6.7.7. Variable-length pattern matching", pattern,
        "
        (a)-[*2]->(b)
    "},
    % --------------------------------------------------------------------------
    {"Chapter 6. Syntax", "6.7. Patterns", "6.7.7. Variable-length pattern matching", pattern,
        "
        (a)-->()-->(b)
    "},
    % --------------------------------------------------------------------------
    {"Chapter 6. Syntax", "6.7. Patterns", "6.7.7. Variable-length pattern matching", pattern,
        "
        (a)-[*3..5]->(b)
    "},
    % --------------------------------------------------------------------------
    {"Chapter 6. Syntax", "6.7. Patterns", "6.7.7. Variable-length pattern matching", pattern,
        "
        (a)-[*3..]->(b)
    "},
    % --------------------------------------------------------------------------
    {"Chapter 6. Syntax", "6.7. Patterns", "6.7.7. Variable-length pattern matching", pattern,
        "
        (a)-[*..5]->(b)
    "},
    % --------------------------------------------------------------------------
    {"Chapter 6. Syntax", "6.7. Patterns", "6.7.7. Variable-length pattern matching", pattern,
        "
        (a)-[*]->(b)
    "},
    % --------------------------------------------------------------------------
    {"Chapter 6. Syntax", "6.7. Patterns", "6.7.7. Variable-length pattern matching", cypher,
        "
        MATCH (me)-[:KNOWS*1..2]-(remote_friend)
        WHERE me.name = 'Filipa'
        RETURN remote_friend.name
    "},
    % --------------------------------------------------------------------------
    {"Chapter 6. Syntax", "6.7. Patterns", "6.7.8. Assigning to path variables", pattern,
        "
        p = (a)-[*3..5]->(b)
    "},
    % --------------------------------------------------------------------------
    {"Chapter 6. Syntax", "6.8. Lists", "6.8.1. Lists in general", cypher,
        "
        RETURN [0, 1, 2, 3, 4, 5, 6, 7, 8, 9] AS list
    "},
    % --------------------------------------------------------------------------
    {"Chapter 6. Syntax", "6.8. Lists", "6.8.1. Lists in general", cypher,
        "
        RETURN range(0, 10)[3]
    "},
    % --------------------------------------------------------------------------
    {"Chapter 6. Syntax", "6.8. Lists", "6.8.1. Lists in general", cypher,
        "
        RETURN range(0, 10)[-3]
    "},
    % --------------------------------------------------------------------------
    {"Chapter 6. Syntax", "6.8. Lists", "6.8.1. Lists in general", cypher,
        "
        RETURN range(0, 10)[0..3]
    "},
    % --------------------------------------------------------------------------
    {"Chapter 6. Syntax", "6.8. Lists", "6.8.1. Lists in general", cypher,
        "
        RETURN range(0, 10)[0..-5]
    "},
    % --------------------------------------------------------------------------
    {"Chapter 6. Syntax", "6.8. Lists", "6.8.1. Lists in general", cypher,
        "
        RETURN range(0, 10)[-5..]
    "},
    % --------------------------------------------------------------------------
    {"Chapter 6. Syntax", "6.8. Lists", "6.8.1. Lists in general", cypher,
        "
        RETURN range(0, 10)[..4]
    "},
    % --------------------------------------------------------------------------
    {"Chapter 6. Syntax", "6.8. Lists", "6.8.1. Lists in general", cypher,
        "
        RETURN range(0, 10)[15]
    "},
    % --------------------------------------------------------------------------
    {"Chapter 6. Syntax", "6.8. Lists", "6.8.1. Lists in general", cypher,
        "
        RETURN range(0, 10)[5..15]
    "},
    % --------------------------------------------------------------------------
    {"Chapter 6. Syntax", "6.8. Lists", "6.8.1. Lists in general", cypher,
        "
        RETURN size(range(0, 10)[0..3])
    "},
    % --------------------------------------------------------------------------
    {"Chapter 6. Syntax", "6.8. Lists", "6.8.2. List comprehension", cypher,
        "
        RETURN [x IN range(0,10) WHERE x % 2 = 0 | x^3] AS result
    "},
    % --------------------------------------------------------------------------
    {"Chapter 6. Syntax", "6.8. Lists", "6.8.2. List comprehension", cypher,
        "
        RETURN [x IN range(0,10) WHERE x % 2 = 0] AS result
    "},
    % --------------------------------------------------------------------------
    {"Chapter 6. Syntax", "6.8. Lists", "6.8.2. List comprehension", cypher,
        "
        RETURN [x IN range(0,10)| x^3] AS result
    "},
    % --------------------------------------------------------------------------
    {"Chapter 6. Syntax", "6.8. Lists", "6.8.3. Pattern comprehension", cypher,
        "
        MATCH (a:Person { name: 'Charlie Sheen' })
        RETURN [(a)-->(b) WHERE b:Movie | b.year] AS years
    "},
    % --------------------------------------------------------------------------
    {"Chapter 6. Syntax", "6.9. Maps", "6.9.1. Literal maps", cypher,
        "
        RETURN { key: 'Value', listKey: [{ inner: 'Map1' }, { inner: 'Map2' }]}
    "},
%% wwe - open issue
%%    % ------------------ --------------------------------------------------------
%%    {"Chapter 6. Syntax", "6.9. Maps", "6.9.2. Map projection", cypher,
%%        "
%%        MATCH (actor:Person { name: 'Charlie Sheen' })-[:ACTED_IN]->(movie:Movie)
%%        RETURN actor { .name, .realName, movies: collect(movie { .title, .year })}
%%    "},
%%    % --------------------------------------------------------------------------
%%    {"Chapter 6. Syntax", "6.9. Maps", "6.9.2. Map projection", cypher,
%%        "
%%        MATCH (actor:Person)-[:ACTED_IN]->(movie:Movie)
%%        WITH actor, count(movie) AS nrOfMovies
%%        RETURN actor { .name, nrOfMovies }
%%    "},
%%    % --------------------------------------------------------------------------
%%    {"Chapter 6. Syntax", "6.9. Maps", "6.9.2. Map projection", cypher,
%%        "
%%        MATCH (actor:Person { name: 'Charlie Sheen' })
%%        RETURN actor { .*, .age }
%%    "},
    % --------------------------------------------------------------------------
    {"Chapter 7. Clauses", "7.1. MATCH", "7.1.2. Basic node finding", cypher,
        "
        MATCH (n)
        RETURN n
    "},
    % --------------------------------------------------------------------------
    {"Chapter 7. Clauses", "7.1. MATCH", "7.1.2. Basic node finding", cypher,
        "
        MATCH (movie:Movie)
        RETURN movie.title
    "},
    % --------------------------------------------------------------------------
    {"Chapter 7. Clauses", "7.1. MATCH", "7.1.2. Basic node finding", cypher,
        "
        MATCH (director { name: 'Oliver Stone' })--(movie)
        RETURN movie.title
    "},
    % --------------------------------------------------------------------------
    {"Chapter 7. Clauses", "7.1. MATCH", "7.1.2. Basic node finding", cypher,
        "
        MATCH (:Person { name: 'Oliver Stone' })--(movie:Movie)
        RETURN movie.title
    "},
    % --------------------------------------------------------------------------
    {"Chapter 7. Clauses", "7.1. MATCH", "7.1.3. Relationship basics", cypher,
        "
        MATCH (:Person { name: 'Oliver Stone' })-->(movie)
        RETURN movie.title
    "},
    % --------------------------------------------------------------------------
    {"Chapter 7. Clauses", "7.1. MATCH", "7.1.3. Relationship basics", cypher,
        "
        MATCH (:Person { name: 'Oliver Stone' })-[r]->(movie)
        RETURN type(r)
    "},
    % --------------------------------------------------------------------------
    {"Chapter 7. Clauses", "7.1. MATCH", "7.1.3. Relationship basics", cypher,
        "
        MATCH (wallstreet:Movie { title: 'Wall Street' })<-[:ACTED_IN]-(actor)
        RETURN actor.name
    "},
    % --------------------------------------------------------------------------
    {"Chapter 7. Clauses", "7.1. MATCH", "7.1.3. Relationship basics", cypher,
        "
        MATCH (wallstreet { title: 'Wall Street' })<-[:ACTED_IN|:DIRECTED]-(person)
        RETURN person.name
    "},
    % --------------------------------------------------------------------------
    {"Chapter 7. Clauses", "7.1. MATCH", "7.1.3. Relationship basics", cypher,
        "
        MATCH (wallstreet { title: 'Wall Street' })<-[r:ACTED_IN]-(actor)
        RETURN r.role
    "},
    % --------------------------------------------------------------------------
    {"Chapter 7. Clauses", "7.1. MATCH", "7.1.4. Relationships in depth", cypher,
        "
        MATCH (charlie:Person { name: 'Charlie Sheen' }),(rob:Person { name: 'Rob Reiner' })
        CREATE (rob)-[:`TYPE
_______ WITH SPACE`]->(charlie)
    "},
    % --------------------------------------------------------------------------
    {"Chapter 7. Clauses", "7.1. MATCH", "7.1.4. Relationships in depth", cypher,
        "
        MATCH (n { name: 'Rob Reiner' })-[r:`TYPE
_______ WITH SPACE`]->()
        RETURN type(r)
    "},
    % --------------------------------------------------------------------------
    {"Chapter 7. Clauses", "7.1. MATCH", "7.1.4. Relationships in depth", cypher,
        "
        MATCH (charlie { name: 'Charlie Sheen' })-[:ACTED_IN]->(movie)<-[:DIRECTED]-(director)
        RETURN movie.title, director.name
    "},
    % --------------------------------------------------------------------------
    {"Chapter 7. Clauses", "7.1. MATCH", "7.1.4. Relationships in depth", cypher,
        "
        MATCH (martin { name: 'Charlie Sheen' })-[:ACTED_IN*1..3]-(movie:Movie)
        RETURN movie.title
    "},
    % --------------------------------------------------------------------------
    {"Chapter 7. Clauses", "7.1. MATCH", "7.1.4. Relationships in depth", cypher,
        "
        MATCH p =(actor { name: 'Charlie Sheen' })-[:ACTED_IN*2]-(co_actor)
        RETURN relationships(p)
    "},
    % --------------------------------------------------------------------------
    {"Chapter 7. Clauses", "7.1. MATCH", "7.1.4. Relationships in depth", cypher,
        "
        MATCH (charlie:Person { name: 'Charlie Sheen' }),(martin:Person { name: 'Martin Sheen' })
        CREATE (charlie)-[:X { blocked: FALSE }]->(:UNBLOCKED)<-[:X { blocked: FALSE }]-(martin)
        CREATE (charlie)-[:X { blocked: TRUE }]->(:BLOCKED)<-[:X { blocked: FALSE }]-(martin)
    "},
    % --------------------------------------------------------------------------
    {"Chapter 7. Clauses", "7.1. MATCH", "7.1.4. Relationships in depth", cypher,
        "
        MATCH p =(charlie:Person)-[* { blocked:false }]-(martin:Person)
        WHERE charlie.name = 'Charlie Sheen' AND martin.name = 'Martin Sheen'
        RETURN p
    "},
    % --------------------------------------------------------------------------
    {"Chapter 7. Clauses", "7.1. MATCH", "7.1.4. Relationships in depth", cypher,
        "
        MATCH (wallstreet:Movie { title: 'Wall Street' })-[*0..1]-(x)
        RETURN x
    "},
    % --------------------------------------------------------------------------
    {"Chapter 7. Clauses", "7.1. MATCH", "7.1.4. Relationships in depth", cypher,
        "
        MATCH p =(michael { name: 'Michael Douglas' })-->()
        RETURN p
    "},
    % --------------------------------------------------------------------------
    {"Chapter 7. Clauses", "7.1. MATCH", "7.1.4. Relationships in depth", cypher,
        "
        MATCH (a)-[r]-(b)
        WHERE id(r)= 0
        RETURN a,b
    "},
%% wwe legacy - function_call with pattern as argument
%%    % --------------------------------------------------------------------------
%%    {"Chapter 7. Clauses", "7.1. MATCH", "7.1.5. Shortest path", cypher,
%%        "
%%        MATCH (martin:Person { name: 'Martin Sheen' }),(oliver:Person { name: 'Oliver Stone' }), p =
%%        shortestPath((martin)-[*..15]-(oliver))
%%        RETURN p
%%    "},
%%    % --------------------------------------------------------------------------
%%    {"Chapter 7. Clauses", "7.1. MATCH", "7.1.5. Shortest path", cypher,
%%        "
%%        MATCH (charlie:Person { name: 'Charlie Sheen' }),(martin:Person { name: 'Martin Sheen' }), p =
%%        shortestPath((charlie)-[*]-(martin))
%%        WHERE NONE (r IN relationships(p) WHERE type(r)= 'FATHER')
%%        RETURN p
%%    "},
%%    % --------------------------------------------------------------------------
%%    {"Chapter 7. Clauses", "7.1. MATCH", "7.1.5. Shortest path", cypher,
%%        "
%%        MATCH (martin:Person { name: 'Martin Sheen' }),(michael:Person { name: 'Michael Douglas' }), p =
%%        allShortestPaths((martin)-[*]-(michael))
%%        RETURN p
%%    "},
    % --------------------------------------------------------------------------
    {"Chapter 7. Clauses", "7.1. MATCH", "7.1.6. Get node or relationship by id", cypher,
        "
        MATCH (n)
        WHERE id(n)= 0
        RETURN n
    "},
    % --------------------------------------------------------------------------
    {"Chapter 7. Clauses", "7.1. MATCH", "7.1.6. Get node or relationship by id", cypher,
        "
        MATCH ()-[r]->()
        WHERE id(r)= 0
        RETURN r
    "},
    % --------------------------------------------------------------------------
    {"Chapter 7. Clauses", "7.1. MATCH", "7.1.6. Get node or relationship by id", cypher,
        "
        MATCH (n)
        WHERE id(n) IN [0, 3, 5]
        RETURN n
    "},
    % --------------------------------------------------------------------------
    {"Chapter 7. Clauses", "7.2. OPTIONAL MATCH", "7.2.2. Optional relationships", cypher,
        "
        MATCH (a:Movie { title: 'Wall Street' })
        OPTIONAL MATCH (a)-->(x)
        RETURN x
    "},
    % --------------------------------------------------------------------------
    {"Chapter 7. Clauses", "7.2. OPTIONAL MATCH", "7.2.3. Properties on optional elements", cypher,
        "
        MATCH (a:Movie { title: 'Wall Street' })
        OPTIONAL MATCH (a)-->(x)
        RETURN x, x.name
    "},
    % --------------------------------------------------------------------------
    {"Chapter 7. Clauses", "7.2. OPTIONAL MATCH", "7.2.4. Optional typed and named relationship", cypher,
        "
        MATCH (a:Movie { title: 'Wall Street' })
        OPTIONAL MATCH (a)-[r:ACTS_IN]->()
        RETURN a.title, r
    "},
    % --------------------------------------------------------------------------
    {"Chapter 7. Clauses", "7.4. RETURN", "7.4.2. Return nodes", cypher,
        "
        MATCH (n { name: 'B' })
        RETURN n
    "},
    % --------------------------------------------------------------------------
    {"Chapter 7. Clauses", "7.4. RETURN", "7.4.3. Return relationships", cypher,
        "
        MATCH (n { name: 'A' })-[r:KNOWS]->(c)
        RETURN r
    "},
    % --------------------------------------------------------------------------
    {"Chapter 7. Clauses", "7.4. RETURN", "7.4.4. Return property", cypher,
        "
        MATCH (n { name: 'A' })
        RETURN n.name
    "},
    % --------------------------------------------------------------------------
    {"Chapter 7. Clauses", "7.4. RETURN", "7.4.5. Return all elements", cypher,
        "
        MATCH p =(a { name: 'A' })-[r]->(b)
        RETURN *
    "},
    % --------------------------------------------------------------------------
    {"Chapter 7. Clauses", "7.4. RETURN", "7.4.6. Variable with uncommon characters", cypher,
        "
        MATCH (`This isn\'t a common variable`)
        WHERE `This isn\'t a common variable`.name = 'A'
        RETURN `This isn\'t a common variable`.happy
    "},
    % --------------------------------------------------------------------------
    {"Chapter 7. Clauses", "7.4. RETURN", "7.4.7. Column alias", cypher,
        "
        MATCH (a { name: 'A' })
        RETURN a.age AS SomethingTotallyDifferent
    "},
    % --------------------------------------------------------------------------
    {"Chapter 7. Clauses", "7.4. RETURN", "7.4.8. Optional properties", cypher,
        "
        MATCH (n)
        RETURN n.age
    "},
    % --------------------------------------------------------------------------
    {"Chapter 7. Clauses", "7.4. RETURN", "7.4.9. Other expressions", cypher,
        "
        MATCH (a { name: 'A' })
        RETURN a.age > 30, \"I'm a literal\",(a)-->()
    "},
    % --------------------------------------------------------------------------
    {"Chapter 7. Clauses", "7.4. RETURN", "7.4.10. Unique results", cypher,
        "
        MATCH (a { name: 'A' })-->(b)
        RETURN DISTINCT b
    "},
    % --------------------------------------------------------------------------
    {"Chapter 7. Clauses", "7.5. WITH", "7.5.2. Filter on aggregate function results", cypher,
        "
        MATCH (david { name: 'David' })--(otherPerson)-->()
        WITH otherPerson, count(*) AS foaf
        WHERE foaf > 1
        RETURN otherPerson.name
    "},
    % --------------------------------------------------------------------------
    {"Chapter 7. Clauses", "7.5. WITH", "7.5.3. Sort results before using collect on them", cypher,
        "
        MATCH (n)
        WITH n
        ORDER BY n.name DESC LIMIT 3
        RETURN collect(n.name)
    "},
    % --------------------------------------------------------------------------
    {"Chapter 7. Clauses", "7.5. WITH", "7.5.4. Limit branching of a path search", cypher,
        "
        MATCH (n { name: 'Anders' })--(m)
        WITH m
        ORDER BY m.name DESC LIMIT 1
        MATCH (m)--(o)
        RETURN o.name
    "},
    % --------------------------------------------------------------------------
    {"Chapter 7. Clauses", "7.6. UNWIND", "7.6.2. Unwind a list", cypher,
        "
        UNWIND [1, 2, 3] AS x
        RETURN x
    "},
    % --------------------------------------------------------------------------
    {"Chapter 7. Clauses", "7.6. UNWIND", "7.6.3. Create a distinct list", cypher,
        "
        WITH [1, 1, 2, 2] AS coll
        UNWIND coll AS x
        WITH DISTINCT x
        RETURN collect(x) AS SET_wwe
    "},
    % --------------------------------------------------------------------------
    {"Chapter 7. Clauses", "7.6. UNWIND", "7.6.4. Create nodes from a list parameter", cypher,
        "
        UNWIND $events AS event
        MERGE (y:Year { year: event.year })
        MERGE (y)<-[:IN]-(e:Event { id: event.id })
        RETURN e.id AS x
        ORDER BY x
    "},
    % --------------------------------------------------------------------------
    {"Chapter 7. Clauses", "7.7. WHERE", "7.7.2. Basic usage", cypher,
        "
        MATCH (n)
        WHERE n.name = 'Peter' XOR (n.age < 30 AND n.name = 'Tobias') OR NOT (n.name = 'Tobias' OR n.name =
        'Peter')
        RETURN n.name, n.age
    "},
    % --------------------------------------------------------------------------
    {"Chapter 7. Clauses", "7.7. WHERE", "7.7.2. Basic usage", cypher,
        "
        MATCH (n)
        WHERE n:Swedish
        RETURN n.name, n.age
    "},
    % --------------------------------------------------------------------------
    {"Chapter 7. Clauses", "7.7. WHERE", "7.7.2. Basic usage", cypher,
        "
        MATCH (n)
        WHERE n.age < 30
        RETURN n.name, n.age
    "},
    % --------------------------------------------------------------------------
    {"Chapter 7. Clauses", "7.7. WHERE", "7.7.2. Basic usage", cypher,
        "
        MATCH (n)-[k:KNOWS]->(f)
        WHERE k.since < 2000
        RETURN f.name, f.age, f.email
    "},
    % --------------------------------------------------------------------------
    {"Chapter 7. Clauses", "7.7. WHERE", "7.7.2. Basic usage", cypher,
        "
        WITH 'AGE' AS propname
        MATCH (n)
        WHERE n[toLower(propname)]< 30
        RETURN n.name, n.age
    "},
    % --------------------------------------------------------------------------
    {"Chapter 7. Clauses", "7.7. WHERE", "7.7.2. Basic usage", cypher,
        "
        MATCH (n)
        WHERE exists(n.belt)
        RETURN n.name, n.belt
    "},
    % --------------------------------------------------------------------------
    {"Chapter 7. Clauses", "7.7. WHERE", "7.7.3. String matching", cypher,
        "
        MATCH (n)
        WHERE n.name STARTS WITH 'Pet'
        RETURN n.name, n.age
    "},
    % --------------------------------------------------------------------------
    {"Chapter 7. Clauses", "7.7. WHERE", "7.7.3. String matching", cypher,
        "
        MATCH (n)
        WHERE n.name ENDS WITH 'ter'
        RETURN n.name, n.age
    "},
    % --------------------------------------------------------------------------
    {"Chapter 7. Clauses", "7.7. WHERE", "7.7.3. String matching", cypher,
        "
        MATCH (n)
        WHERE n.name CONTAINS 'ete'
        RETURN n.name, n.age
    "},
    % --------------------------------------------------------------------------
    {"Chapter 7. Clauses", "7.7. WHERE", "7.7.3. String matching", cypher,
        "
        MATCH (n)
        WHERE NOT n.name ENDS WITH 's'
        RETURN n.name, n.age
    "},
    % --------------------------------------------------------------------------
    {"Chapter 7. Clauses", "7.7. WHERE", "7.7.4. Regular expressions", cypher,
        "
        MATCH (n)
        WHERE n.name =~ 'Tob.*'
        RETURN n.name, n.age
    "},
    % --------------------------------------------------------------------------
    {"Chapter 7. Clauses", "7.7. WHERE", "7.7.4. Regular expressions", cypher,
        "
        MATCH (n)
        WHERE n.address =~ 'Sweden\/Malmo'
        RETURN n.name, n.age, n.address
    "},
    % --------------------------------------------------------------------------
    {"Chapter 7. Clauses", "7.7. WHERE", "7.7.4. Regular expressions", cypher,
        "
        MATCH (n)
        WHERE n.name =~ '(?i)ANDR.*'
        RETURN n.name, n.age
    "},
    % --------------------------------------------------------------------------
    {"Chapter 7. Clauses", "7.7. WHERE", "7.7.5. Using path patterns in WHERE", cypher,
        "
        MATCH (tobias { name: 'Tobias' }),(others)
        WHERE others.name IN ['Andres', 'Peter'] AND (tobias)<--(others)
        RETURN others.name, others.age
    "},
    % --------------------------------------------------------------------------
    {"Chapter 7. Clauses", "7.7. WHERE", "7.7.5. Using path patterns in WHERE", cypher,
        "
        MATCH (persons),(peter { name: 'Peter' })
        WHERE NOT (persons)-->(peter)
        RETURN persons.name, persons.age
    "},
    % --------------------------------------------------------------------------
    {"Chapter 7. Clauses", "7.7. WHERE", "7.7.5. Using path patterns in WHERE", cypher,
        "
        MATCH (n)
        WHERE (n)-[:KNOWS]-({ name: 'Tobias' })
        RETURN n.name, n.age
    "},
    % --------------------------------------------------------------------------
    {"Chapter 7. Clauses", "7.7. WHERE", "7.7.5. Using path patterns in WHERE", cypher,
        "
        MATCH (n)-[r]->()
        WHERE n.name='Andres' AND type(r)=~ 'K.*'
        RETURN type(r), r.since
    "},
    % --------------------------------------------------------------------------
    {"Chapter 7. Clauses", "7.7. WHERE", "7.7.6. Lists", cypher,
        "
        MATCH (a)
        WHERE a.name IN ['Peter', 'Tobias']
        RETURN a.name, a.age
    "},
    % --------------------------------------------------------------------------
    {"Chapter 7. Clauses", "7.7. WHERE", "7.7.7. Missing properties and values", cypher,
        "
        MATCH (n)
        WHERE n.belt = 'white'
        RETURN n.name, n.age, n.belt
    "},
    % --------------------------------------------------------------------------
    {"Chapter 7. Clauses", "7.7. WHERE", "7.7.7. Missing properties and values", cypher,
        "
        MATCH (n)
        WHERE n.belt = 'white' OR n.belt IS NULL RETURN n.name, n.age, n.belt
        ORDER BY n.name
    "},
    % --------------------------------------------------------------------------
    {"Chapter 7. Clauses", "7.7. WHERE", "7.7.7. Missing properties and values", cypher,
        "
        MATCH (person)
        WHERE person.name = 'Peter' AND person.belt IS NULL RETURN person.name, person.age, person.belt
    "},
    % --------------------------------------------------------------------------
    {"Chapter 7. Clauses", "7.7. WHERE", "7.7.8. Using ranges", cypher,
        "
        MATCH (a)
        WHERE a.name >= 'Peter'
        RETURN a.name, a.age
    "},
    % --------------------------------------------------------------------------
    {"Chapter 7. Clauses", "7.7. WHERE", "7.7.8. Using ranges", cypher,
        "
        MATCH (a)
        WHERE a.name > 'Andres' AND a.name < 'Tobias'
        RETURN a.name, a.age
    "},
    % --------------------------------------------------------------------------
    {"Chapter 7. Clauses", "7.8. ORDER BY", "7.8.2. Order nodes by property", cypher,
        "
        MATCH (n)
        RETURN n.name, n.age
        ORDER BY n.name
    "},
    % --------------------------------------------------------------------------
    {"Chapter 7. Clauses", "7.8. ORDER BY", "7.8.3. Order nodes by multiple properties", cypher,
        "
        MATCH (n)
        RETURN n.name, n.age
        ORDER BY n.age, n.name
    "},
    % --------------------------------------------------------------------------
    {"Chapter 7. Clauses", "7.8. ORDER BY", "7.8.4. Order nodes in descending order", cypher,
        "
        MATCH (n)
        RETURN n.name, n.age
        ORDER BY n.name DESC
    "},
    % --------------------------------------------------------------------------
    {"Chapter 7. Clauses", "7.8. ORDER BY", "7.8.5. Ordering null", cypher,
        "
        MATCH (n)
        RETURN n.length, n.name, n.age
        ORDER BY n.length
    "},
    % --------------------------------------------------------------------------
    {"Chapter 7. Clauses", "7.9. SKIP", "7.9.2. Skip first three rows", cypher,
        "
        MATCH (n)
        RETURN n.name
        ORDER BY n.name
        SKIP 3
    "},
    % --------------------------------------------------------------------------
    {"Chapter 7. Clauses", "7.9. SKIP", "7.9.3. Return middle two rows", cypher,
        "
        MATCH (n)
        RETURN n.name
        ORDER BY n.name
        SKIP 1
        LIMIT 2
    "},
    % --------------------------------------------------------------------------
    {"Chapter 7. Clauses", "7.9. SKIP", "7.9.4. Using an expression with SKIP to return a subset of the rows", cypher,
        "
        MATCH (n)
        RETURN n.name
        ORDER BY n.name
        SKIP toInteger(3*rand())+ 1
    "},
    % --------------------------------------------------------------------------
    {"Chapter 7. Clauses", "7.10. LIMIT", "7.10.2. Return a subset of the rows", cypher,
        "
        MATCH (n)
        RETURN n.name
        ORDER BY n.name
        LIMIT 3
    "},
    % --------------------------------------------------------------------------
    {"Chapter 7. Clauses", "7.10. LIMIT", "7.10.3. Using an expression with LIMIT to return a subset of the rows", cypher,
        "
        MATCH (n)
        RETURN n.name
        ORDER BY n.name
        LIMIT toInteger(3 * rand())+ 1
    "},
    % --------------------------------------------------------------------------
    {"Chapter 7. Clauses", "7.11. CREATE", "7.11.1. Create nodes", cypher,
        "
        CREATE (n)
    "},
    % --------------------------------------------------------------------------
    {"Chapter 7. Clauses", "7.11. CREATE", "7.11.1. Create nodes", cypher,
        "
        CREATE (n),(m)
    "},
    % --------------------------------------------------------------------------
    {"Chapter 7. Clauses", "7.11. CREATE", "7.11.1. Create nodes", cypher,
        "
        CREATE (n:Person)
    "},
    % --------------------------------------------------------------------------
    {"Chapter 7. Clauses", "7.11. CREATE", "7.11.1. Create nodes", cypher,
        "
        CREATE (n:Person:Swedish)
    "},
    % --------------------------------------------------------------------------
    {"Chapter 7. Clauses", "7.11. CREATE", "7.11.1. Create nodes", cypher,
        "
        CREATE (n:Person { name: 'Andres', title: 'Developer' })
    "},
    % --------------------------------------------------------------------------
    {"Chapter 7. Clauses", "7.11. CREATE", "7.11.1. Create nodes", cypher,
        "
        CREATE (a { name: 'Andres' })
        RETURN a
    "},
    % --------------------------------------------------------------------------
    {"Chapter 7. Clauses", "7.11. CREATE", "7.11.2. Create relationships", cypher,
        "
        MATCH (a:Person),(b:Person)
        WHERE a.name = 'Node A' AND b.name = 'Node B'
        CREATE (a)-[r:RELTYPE]->(b)
        RETURN r
    "},
    % --------------------------------------------------------------------------
    {"Chapter 7. Clauses", "7.11. CREATE", "7.11.2. Create relationships", cypher,
        "
        MATCH (a:Person),(b:Person)
        WHERE a.name = 'Node A' AND b.name = 'Node B'
        CREATE (a)-[r:RELTYPE { name: a.name + '<->' + b.name }]->(b)
        RETURN r
    "},
    % --------------------------------------------------------------------------
    {"Chapter 7. Clauses", "7.11. CREATE", "7.11.3. Create a full path", cypher,
        "
        CREATE p =(andres { name:'Andres' })-[:WORKS_AT]->(neo)<-[:WORKS_AT]-(michael { name: 'Michael' })
        RETURN p
    "},
    % --------------------------------------------------------------------------
    {"Chapter 7. Clauses", "7.11. CREATE", "7.11.4. Use parameters with CREATE", cypher,
        "
        CREATE (n:Person $props)
        RETURN n
    "},
    % --------------------------------------------------------------------------
    {"Chapter 7. Clauses", "7.11. CREATE", "7.11.4. Use parameters with CREATE", cypher,
        "
        UNWIND $props AS map
        CREATE (n)
        SET n = map
    "},
    % --------------------------------------------------------------------------
    {"Chapter 7. Clauses", "7.12. DELETE", "7.12.2. Delete single node", cypher,
        "
        MATCH (n:Useless)
        DELETE n
    "},
    % --------------------------------------------------------------------------
    {"Chapter 7. Clauses", "7.12. DELETE", "7.12.3. Delete all nodes and relationships", cypher,
        "
        MATCH (n)
        DETACH DELETE n
    "},
    % --------------------------------------------------------------------------
    {"Chapter 7. Clauses", "7.12. DELETE", "7.12.4. Delete a node with all its relationships", cypher,
        "
        MATCH (n { name: 'Andres' })
        DETACH DELETE n
    "},
    % --------------------------------------------------------------------------
    {"Chapter 7. Clauses", "7.12. DELETE", "7.12.3. Delete all nodes and relationships", cypher,
        "
        MATCH (n { name: 'Andres' })-[r:KNOWS]->()
        DELETE r
    "},
    % --------------------------------------------------------------------------
    {"Chapter 7. Clauses", "7.13. SET", "7.13.2. Set a property", cypher,
        "
        MATCH (n { name: 'Andres' })
        SET n.surname = 'Taylor'
        RETURN n
    "},
    % --------------------------------------------------------------------------
    {"Chapter 7. Clauses", "7.13. SET", "7.13.3. Remove a property", cypher,
        "
        MATCH (n { name: 'Andres' })
        SET n.name = NULL RETURN n
    "},
    % --------------------------------------------------------------------------
    {"Chapter 7. Clauses", "7.13. SET", "7.13.4. Copying properties between nodes and relationships", cypher,
        "
        MATCH (at { name: 'Andres' }),(pn { name: 'Peter' })
        SET at = pn
        RETURN at, pn
    "},
    % --------------------------------------------------------------------------
    {"Chapter 7. Clauses", "7.13. SET", "7.13.5. Adding properties from maps", cypher,
        "
        MATCH (peter { name: 'Peter' })
        SET peter += { hungry: TRUE , position: 'Entrepreneur' }
    "},
    % --------------------------------------------------------------------------
    {"Chapter 7. Clauses", "7.13. SET", "7.13.6. Set a property using a parameter", cypher,
        "
        MATCH (n { name: 'Andres' })
        SET n.surname = $surname
        RETURN n
    "},
    % --------------------------------------------------------------------------
    {"Chapter 7. Clauses", "7.13. SET", "7.13.7. Set all properties using a parameter", cypher,
        "
        MATCH (n { name: 'Andres' })
        SET n = $props
        RETURN n
    "},
    % --------------------------------------------------------------------------
    {"Chapter 7. Clauses", "7.13. SET", "7.13.8. Set multiple properties using one SET clause", cypher,
        "
        MATCH (n { name: 'Andres' })
        SET n.position = 'Developer', n.surname = 'Taylor'
    "},
    % --------------------------------------------------------------------------
    {"Chapter 7. Clauses", "7.13. SET", "7.13.9. Set a label on a node", cypher,
        "
        MATCH (n { name: 'Stefan' })
        SET n:German
        RETURN n
    "},
    % --------------------------------------------------------------------------
    {"Chapter 7. Clauses", "7.13. SET", "7.13.10. Set multiple labels on a node", cypher,
        "
        MATCH (n { name: 'Emil' })
        SET n:Swedish:Bossman
        RETURN n
    "},
    % --------------------------------------------------------------------------
    {"Chapter 7. Clauses", "7.14. REMOVE", "7.14.2. Remove a property", cypher,
        "
        MATCH (andres { name: 'Andres' })
        REMOVE andres.age
        RETURN andres
    "},
    % --------------------------------------------------------------------------
    {"Chapter 7. Clauses", "7.14. REMOVE", "7.14.3. Remove a label from a node", cypher,
        "
        MATCH (n { name: 'Peter' })
        REMOVE n:German
        RETURN n
    "},
    % --------------------------------------------------------------------------
    {"Chapter 7. Clauses", "7.14. REMOVE", "7.14.4. Removing multiple labels", cypher,
        "
        MATCH (n { name: 'Peter' })
        REMOVE n:German:Swedish
        RETURN n
    "},
    % --------------------------------------------------------------------------
    {"Chapter 7. Clauses", "7.16. MERGE", "7.16.2. Merge nodes", cypher,
        "
        MERGE (robert:Critic)
        RETURN robert, labels(robert)
    "},
    % --------------------------------------------------------------------------
    {"Chapter 7. Clauses", "7.16. MERGE", "7.16.2. Merge nodes", cypher,
        "
        MERGE (charlie { name: 'Charlie Sheen', age: 10 })
        RETURN charlie
    "},
    % --------------------------------------------------------------------------
    {"Chapter 7. Clauses", "7.16. MERGE", "7.16.2. Merge nodes", cypher,
        "
        MERGE (michael:Person { name: 'Michael Douglas' })
        RETURN michael.name, michael.bornIn
    "},
    % --------------------------------------------------------------------------
    {"Chapter 7. Clauses", "7.16. MERGE", "7.16.2. Merge nodes", cypher,
        "
        MATCH (person:Person)
        MERGE (city:City { name: person.bornIn })
        RETURN person.name, person.bornIn, city
    "},
    % --------------------------------------------------------------------------
    {"Chapter 7. Clauses", "7.16. MERGE", "7.16.3. Use ON CREATE and ON MATCH", cypher,
        "
        MERGE (keanu:Person { name: 'Keanu Reeves' })
        ON CREATE SET keanu.created = timestamp()
        RETURN keanu.name, keanu.created
    "},
    % --------------------------------------------------------------------------
    {"Chapter 7. Clauses", "7.16. MERGE", "7.16.3. Use ON CREATE and ON MATCH", cypher,
        "
        MERGE (person:Person)
        ON MATCH SET person.found = TRUE RETURN person.name, person.found
    "},
    % --------------------------------------------------------------------------
    {"Chapter 7. Clauses", "7.16. MERGE", "7.16.3. Use ON CREATE and ON MATCH", cypher,
        "
        MERGE (keanu:Person { name: 'Keanu Reeves' })
        ON CREATE SET keanu.created = timestamp()
        ON MATCH SET keanu.lastSeen = timestamp()
        RETURN keanu.name, keanu.created, keanu.lastSeen
    "},
    % --------------------------------------------------------------------------
    {"Chapter 7. Clauses", "7.16. MERGE", "7.16.3. Use ON CREATE and ON MATCH", cypher,
        "
        MERGE (person:Person)
        ON MATCH SET person.found = TRUE , person.lastAccessed = timestamp()
        RETURN person.name, person.found, person.lastAccessed
    "},
    % --------------------------------------------------------------------------
    {"Chapter 7. Clauses", "7.16. MERGE", "7.16.4. Merge relationships", cypher,
        "
        MATCH (charlie:Person { name: 'Charlie Sheen' }),(wallStreet:Movie { title: 'Wall Street' })
        MERGE (charlie)-[r:ACTED_IN]->(wallStreet)
        RETURN charlie.name, type(r), wallStreet.title
    "},
    % --------------------------------------------------------------------------
    {"Chapter 7. Clauses", "7.16. MERGE", "7.16.4. Merge relationships", cypher,
        "
        MATCH (oliver:Person { name: 'Oliver Stone' }),(reiner:Person { name: 'Rob Reiner' })
        MERGE (oliver)-[:DIRECTED]->(movie:Movie)<-[:ACTED_IN]-(reiner)
        RETURN movie
    "},
    % --------------------------------------------------------------------------
    {"Chapter 7. Clauses", "7.16. MERGE", "7.16.4. Merge relationships", cypher,
        "
        MATCH (charlie:Person { name: 'Charlie Sheen' }),(oliver:Person { name: 'Oliver Stone' })
        MERGE (charlie)-[r:KNOWS]-(oliver)
        RETURN r
    "},
    % --------------------------------------------------------------------------
    {"Chapter 7. Clauses", "7.16. MERGE", "7.16.4. Merge relationships", cypher,
        "
        MATCH (person:Person)
        MERGE (city:City { name: person.bornIn })
        MERGE (person)-[r:BORN_IN]->(city)
        RETURN person.name, person.bornIn, city
    "},
    % --------------------------------------------------------------------------
    {"Chapter 7. Clauses", "7.16. MERGE", "7.16.4. Merge relationships", cypher,
        "
        MATCH (person:Person)
        MERGE (person)-[r:HAS_CHAUFFEUR]->(chauffeur:Chauffeur { name: person.chauffeurName })
        RETURN person.name, person.chauffeurName, chauffeur
    "},
%% wwe legacy - CONSTRAINT
%%    % --------------------------------------------------------------------------
%%    {"Chapter 7. Clauses", "7.16. MERGE", "7.16.5. Using unique constraints with MERGE", cypher,
%%        "
%%        CREATE CONSTRAINT ON (n:Person) ASSERT n.name IS UNIQUE;
%%        CREATE CONSTRAINT ON (n:Person) ASSERT n.role IS UNIQUE;
%%    "},
    % --------------------------------------------------------------------------
    {"Chapter 7. Clauses", "7.16. MERGE", "7.16.5. Using unique constraints with MERGE", cypher,
        "
        MERGE (laurence:Person { name: 'Laurence Fishburne' })
        RETURN laurence.name
    "},
    % --------------------------------------------------------------------------
    {"Chapter 7. Clauses", "7.16. MERGE", "7.16.5. Using unique constraints with MERGE", cypher,
        "
        MERGE (oliver:Person { name: 'Oliver Stone' })
        RETURN oliver.name, oliver.bornIn
    "},
    % --------------------------------------------------------------------------
    {"Chapter 7. Clauses", "7.16. MERGE", "7.16.5. Using unique constraints with MERGE", cypher,
        "
        MERGE (michael:Person { name: 'Michael Douglas', role: 'Gordon Gekko' })
        RETURN michael
    "},
    % --------------------------------------------------------------------------
    {"Chapter 7. Clauses", "7.16. MERGE", "7.16.5. Using unique constraints with MERGE", cypher,
        "
        MERGE (oliver:Person { name: 'Oliver Stone', role: 'Gordon Gekko' })
        RETURN oliver
    "},
    % --------------------------------------------------------------------------
    {"Chapter 7. Clauses", "7.16. MERGE", "7.16.6. Using map parameters with MERGE", cypher,
        "
        MERGE (person:Person { name: $param.name, role: $param.role })
        RETURN person.name, person.role
    "},
    % --------------------------------------------------------------------------
    {"Chapter 7. Clauses", "7.17. CALL[...YIELD]", "7.17.2. Call a procedure using CALL", cypher,
        "
        CALL db.labels
    "},
%% wwe legacy - CALL
%%    % --------------------------------------------------------------------------
%%    {"Chapter 7. Clauses", "7.17. CALL[...YIELD]", "7.17.3. View the signature for a procedure", cypher,
%%        "
%%        CALL dbms.procedures() YIELD name, signature
%%        WHERE name='dbms.listConfig'
%%        RETURN signature
%%    "},
    % --------------------------------------------------------------------------
    {"Chapter 7. Clauses", "7.17. CALL[...YIELD]", "7.17.4. Call a procedure using a quoted namespace and name", cypher,
        "
        CALL `db`.`labels`
    "},
    % --------------------------------------------------------------------------
    {"Chapter 7. Clauses", "7.17. CALL[...YIELD]", "7.17.5. Call a procedure with literal arguments", cypher,
        "
        CALL org.neo4j.procedure.example.addNodeToIndex('users', 0, 'name')
    "},
    % --------------------------------------------------------------------------
    {"Chapter 7. Clauses", "7.17. CALL[...YIELD]", "7.17.6. Call a procedure with parameter arguments", cypher,
        "
        CALL org.neo4j.procedure.example.addNodeToIndex
    "},
    % --------------------------------------------------------------------------
    {"Chapter 7. Clauses", "7.17. CALL[...YIELD]", "7.17.7. Call a procedure with mixed literal and parameter arguments", cypher,
        "
        CALL org.neo4j.procedure.example.addNodeToIndex('users', $node_wwe, 'name')
    "},
    % --------------------------------------------------------------------------
    {"Chapter 7. Clauses", "7.17. CALL[...YIELD]", "7.17.8. Call a procedure with literal and default arguments", cypher,
        "
        CALL org.neo4j.procedure.example.addNodeToIndex('users', 0)
    "},
    % --------------------------------------------------------------------------
    {"Chapter 7. Clauses", "7.17. CALL[...YIELD]", "7.17.9. Call a procedure within a complex query using CALL YIELD", cypher,
        "
        CALL db.labels() YIELD label
        RETURN count(label) AS numLabels
    "},
%% wwe legacy - CALL
    % --------------------------------------------------------------------------
%%    {"Chapter 7. Clauses", "7.17. CALL[...YIELD]", "7.17.10. Call a procedure and filter its results", cypher,
%%        "
%%        CALL db.labels() YIELD label
%%        WHERE label CONTAINS 'User'
%%        RETURN count(label) AS numLabels
%%    "},
    % --------------------------------------------------------------------------
    {"Chapter 7. Clauses", "7.17. CALL[...YIELD]", "7.17.11. Call a procedure within a complex query and rename its outputs", cypher,
        "
        CALL db.propertyKeys() YIELD propertyKey AS prop
        MATCH (n)
        WHERE n[prop] IS NOT NULL RETURN prop, count(n) AS numNodes
    "},
    % --------------------------------------------------------------------------
    {"Chapter 7. Clauses", "7.19. UNION", "7.19.2. Combine two queries and retain duplicates", cypher,
        "
        MATCH (n:Actor)
        RETURN n.name AS name
        UNION ALL MATCH (n:Movie)
        RETURN n.title AS name
    "},
    % --------------------------------------------------------------------------
    {"Chapter 7. Clauses", "7.19. UNION", "7.19.3. Combine two queries and remove duplicates", cypher,
        "
        MATCH (n:Actor)
        RETURN n.name AS name
        UNION
        MATCH (n:Movie)
        RETURN n.title AS name
    "},
    % --------------------------------------------------------------------------
    {"Chapter 8. Functions", "8.1. Predicate functions", "8.1.1. all()", cypher,
        "
        MATCH p =(a)-[*1..3]->(b)
        WHERE a.name = 'Alice' AND b.name = 'Daniel' AND ALL (x IN nodes(p) WHERE x.age > 30)
        RETURN p
    "},
    % --------------------------------------------------------------------------
    {"Chapter 8. Functions", "8.1. Predicate functions", "8.1.2. any()", cypher,
        "
        MATCH (a)
        WHERE a.name = 'Eskil' AND ANY (x IN a.array WHERE x = 'one')
        RETURN a.name, a.array
    "},
    % --------------------------------------------------------------------------
    {"Chapter 8. Functions", "8.1. Predicate functions", "8.1.3. exists()", cypher,
        "
        MATCH (n)
        WHERE exists(n.name)
        RETURN n.name AS name, exists((n)-[:MARRIED]->()) AS is_married
    "},
    % --------------------------------------------------------------------------
    {"Chapter 8. Functions", "8.1. Predicate functions", "8.1.4. none()", cypher,
        "
        MATCH p =(n)-[*1..3]->(b)
        WHERE n.name = 'Alice' AND NONE (x IN nodes(p) WHERE x.age = 25)
        RETURN p
    "},
    % --------------------------------------------------------------------------
    {"Chapter 8. Functions", "8.1. Predicate functions", "8.1.5. single()", cypher,
        "
        MATCH p =(n)-->(b)
        WHERE n.name = 'Alice' AND SINGLE (var IN nodes(p) WHERE var.eyes = 'blue')
        RETURN p
    "},
    % --------------------------------------------------------------------------
    {"Chapter 8. Functions", "8.2. Scalar functions", "8.2.1. coalesce()", cypher,
        "
        MATCH (a)
        WHERE a.name = 'Alice'
        RETURN coalesce(a.hairColor, a.eyes)
    "},
    % --------------------------------------------------------------------------
    {"Chapter 8. Functions", "8.2. Scalar functions", "8.2.2. endNode()", cypher,
        "
        MATCH (x:Developer)-[r]-()
        RETURN endNode(r)
    "},
    % --------------------------------------------------------------------------
    {"Chapter 8. Functions", "8.2. Scalar functions", "8.2.3. head()", cypher,
        "
        MATCH (a)
        WHERE a.name = 'Eskil'
        RETURN a.array, head(a.array)
    "},
    % --------------------------------------------------------------------------
    {"Chapter 8. Functions", "8.2. Scalar functions", "8.2.4. id()", cypher,
        "
        MATCH (a)
        RETURN id(a)
    "},
    % --------------------------------------------------------------------------
    {"Chapter 8. Functions", "8.2. Scalar functions", "8.2.5. last()", cypher,
        "
        MATCH (a)
        WHERE a.name = 'Eskil'
        RETURN a.array, last(a.array)
    "},
    % --------------------------------------------------------------------------
    {"Chapter 8. Functions", "8.2. Scalar functions", "8.2.6. length()", cypher,
        "
        MATCH p =(a)-->(b)-->(c)
        WHERE a.name = 'Alice'
        RETURN length(p)
    "},
    % --------------------------------------------------------------------------
    {"Chapter 8. Functions", "8.2. Scalar functions", "8.2.7. properties()", cypher,
        "
        CREATE (p:Person { name: 'Stefan', city: 'Berlin' })
        RETURN properties(p)
    "},
    % --------------------------------------------------------------------------
    {"Chapter 8. Functions", "8.2. Scalar functions", "8.2.8. size()", cypher,
        "
        RETURN size(['Alice', 'Bob'])
    "},
    % --------------------------------------------------------------------------
    {"Chapter 8. Functions", "8.2. Scalar functions", "8.2.9. size() applied to pattern expression", cypher,
        "
        MATCH (a)
        WHERE a.name = 'Alice'
        RETURN size((a)-->()-->()) AS fof
    "},
    % --------------------------------------------------------------------------
    {"Chapter 8. Functions", "8.2. Scalar functions", "8.2.10. size() applied to string", cypher,
        "
        MATCH (a)
        WHERE size(a.name)> 6
        RETURN size(a.name)
    "},
    % --------------------------------------------------------------------------
    {"Chapter 8. Functions", "8.2. Scalar functions", "8.2.11. startNode()", cypher,
        "
        MATCH (x:Developer)-[r]-()
        RETURN startNode(r)
    "},
    % --------------------------------------------------------------------------
    {"Chapter 8. Functions", "8.2. Scalar functions", "8.2.12. timestamp()", cypher,
        "
        RETURN timestamp()
    "},
    % --------------------------------------------------------------------------
    {"Chapter 8. Functions", "8.2. Scalar functions", "8.2.13. toBoolean()", cypher,
        "
        RETURN toBoolean('TRUE'), toBoolean('not a boolean')
    "},
    % --------------------------------------------------------------------------
    {"Chapter 8. Functions", "8.2. Scalar functions", "8.2.14. toFloat()", cypher,
        "
        RETURN toFloat('11.5'), toFloat('not a number')
    "},
    % --------------------------------------------------------------------------
    {"Chapter 8. Functions", "8.2. Scalar functions", "8.2.15. toInteger()", cypher,
        "
        RETURN toInteger('42'), toInteger('not a number')
    "},
    % --------------------------------------------------------------------------
    {"Chapter 8. Functions", "8.2. Scalar functions", "8.2.16. type()", cypher,
        "
        MATCH (n)-[r]->()
        WHERE n.name = 'Alice'
        RETURN type(r)
    "},
    % --------------------------------------------------------------------------
    {"Chapter 8. Functions", "8.3. Aggregating functions", "8.3.1. avg()", cypher,
        "
        MATCH (n:Person)
        RETURN avg(n.age)
    "},
    % --------------------------------------------------------------------------
    {"Chapter 8. Functions", "8.3. Aggregating functions", "8.3.2. collect()", cypher,
        "
        MATCH (n:Person)
        RETURN collect(n.age)
    "},
    % --------------------------------------------------------------------------
    {"Chapter 8. Functions", "8.3. Aggregating functions", "8.3.3. count()", cypher,
        "
        MATCH (n { name: 'A' })-->(x)
        RETURN labels(n), n.age, count(*)
    "},
    % --------------------------------------------------------------------------
    {"Chapter 8. Functions", "8.3. Aggregating functions", "8.3.3. count()", cypher,
        "
        MATCH (n { name: 'A' })-[r]->()
        RETURN type(r), count(*)
    "},
    % --------------------------------------------------------------------------
    {"Chapter 8. Functions", "8.3. Aggregating functions", "8.3.3. count()", cypher,
        "
        MATCH (n { name: 'A' })-->(x)
        RETURN count(x)
    "},
    % --------------------------------------------------------------------------
    {"Chapter 8. Functions", "8.3. Aggregating functions", "8.3.3. count()", cypher,
        "
        MATCH (n:Person)
        RETURN count(n.age)
    "},
    % --------------------------------------------------------------------------
    {"Chapter 8. Functions", "8.3. Aggregating functions", "8.3.3. count()", cypher,
        "
        MATCH (me:Person)-->(friend:Person)-->(friend_of_friend:Person)
        WHERE me.name = 'A'
        RETURN count(DISTINCT friend_of_friend), count(friend_of_friend)
    "},
    % --------------------------------------------------------------------------
    {"Chapter 8. Functions", "8.3. Aggregating functions", "8.3.4. max()", cypher,
        "
        UNWIND [1, 'a', NULL , 0.2, 'b', '1', '99'] AS val
        RETURN max(val)
    "},
    % --------------------------------------------------------------------------
    {"Chapter 8. Functions", "8.3. Aggregating functions", "8.3.4. max()", cypher,
        "
        UNWIND [[1, 'a', 89],[1, 2]] AS val
        RETURN max(val)
    "},
    % --------------------------------------------------------------------------
    {"Chapter 8. Functions", "8.3. Aggregating functions", "8.3.4. max()", cypher,
        "
        MATCH (n:Person)
        RETURN max(n.age)
    "},
    % --------------------------------------------------------------------------
    {"Chapter 8. Functions", "8.3. Aggregating functions", "8.3.5. min()", cypher,
        "
        UNWIND [1, 'a', NULL , 0.2, 'b', '1', '99'] AS val
        RETURN min(val)
    "},
    % --------------------------------------------------------------------------
    {"Chapter 8. Functions", "8.3. Aggregating functions", "8.3.5. min()", cypher,
        "
        UNWIND ['d',[1, 2],['a', 'c', 23]] AS val
        RETURN min(val)
    "},
    % --------------------------------------------------------------------------
    {"Chapter 8. Functions", "8.3. Aggregating functions", "8.3.5. min()", cypher,
        "
        MATCH (n:Person)
        RETURN min(n.age)
    "},
    % --------------------------------------------------------------------------
    {"Chapter 8. Functions", "8.3. Aggregating functions", "8.3.6. percentileCont()", cypher,
        "
        MATCH (n:Person)
        RETURN percentileCont(n.age, 0.4)
    "},
    % --------------------------------------------------------------------------
    {"Chapter 8. Functions", "8.3. Aggregating functions", "8.3.7. percentileDisc()", cypher,
        "
        MATCH (n:Person)
        RETURN percentileDisc(n.age, 0.5)
    "},
    % --------------------------------------------------------------------------
    {"Chapter 8. Functions", "8.3. Aggregating functions", "8.3.8. stDev()", cypher,
        "
        MATCH (n)
        WHERE n.name IN ['A', 'B', 'C']
        RETURN stDev(n.age)
    "},
    % --------------------------------------------------------------------------
    {"Chapter 8. Functions", "8.3. Aggregating functions", "8.3.9. stDevP()", cypher,
        "
        MATCH (n)
        WHERE n.name IN ['A', 'B', 'C']
        RETURN stDevP(n.age)
    "},
    % --------------------------------------------------------------------------
    {"Chapter 8. Functions", "8.3. Aggregating functions", "8.3.10. sum()", cypher,
        "
        MATCH (n:Person)
        RETURN sum(n.age)
    "},
    % --------------------------------------------------------------------------
    {"Chapter 8. Functions", "8.4. List functions", "8.4.1. extract()", cypher,
        "
        MATCH p =(a)-->(b)-->(c)
        WHERE a.name = 'Alice' AND b.name = 'Bob' AND c.name = 'Daniel'
        RETURN extract(n IN nodes(p)| n.age) AS extracted
    "},
    % --------------------------------------------------------------------------
    {"Chapter 8. Functions", "8.4. List functions", "8.4.2. filter()", cypher,
        "
        MATCH (a)
        WHERE a.name = 'Eskil'
        RETURN a.array, filter(x IN a.array WHERE size(x)= 3)
    "},
    % --------------------------------------------------------------------------
    {"Chapter 8. Functions", "8.4. List functions", "8.4.3. keys()", cypher,
        "
        MATCH (a)
        WHERE a.name = 'Alice'
        RETURN keys(a)
    "},
    % --------------------------------------------------------------------------
    {"Chapter 8. Functions", "8.4. List functions", "8.4.4. labels()", cypher,
        "
        MATCH (a)
        WHERE a.name = 'Alice'
        RETURN labels(a)
    "},
    % --------------------------------------------------------------------------
    {"Chapter 8. Functions", "8.4. List functions", "8.4.5. nodes()", cypher,
        "
        MATCH p =(a)-->(b)-->(c)
        WHERE a.name = 'Alice' AND c.name = 'Eskil'
        RETURN nodes(p)
    "},
    % --------------------------------------------------------------------------
    {"Chapter 8. Functions", "8.4. List functions", "8.4.6. range()", cypher,
        "
        RETURN range(0, 10), range(2, 18, 3)
    "},
%% wwe legacy - argument in reduce function
%%    % --------------------------------------------------------------------------
%%    {"Chapter 8. Functions", "8.4. List functions", "8.4.7. reduce()", cypher,
%%        "
%%        MATCH p =(a)-->(b)-->(c)
%%        WHERE a.name = 'Alice' AND b.name = 'Bob' AND c.name = 'Daniel'
%%        RETURN reduce(totalAge = 0, n IN nodes(p)| totalAge + n.age) AS reduction
%%    "},
    % --------------------------------------------------------------------------
    {"Chapter 8. Functions", "8.4. List functions", "8.4.8. relationships()", cypher,
        "
        MATCH p =(a)-->(b)-->(c)
        WHERE a.name = 'Alice' AND c.name = 'Eskil'
        RETURN relationships(p)
    "},
    % --------------------------------------------------------------------------
    {"Chapter 8. Functions", "8.4. List functions", "8.4.9. reverse()", cypher,
        "
        WITH [4923,'abc',521, NULL , 487] AS ids
        RETURN reverse(ids)
    "},
    % --------------------------------------------------------------------------
    {"Chapter 8. Functions", "8.4. List functions", "8.4.10. tail()", cypher,
        "
        MATCH (a)
        WHERE a.name = 'Eskil'
        RETURN a.array, tail(a.array)
    "},
    % --------------------------------------------------------------------------
    {"Chapter 8. Functions", "8.5. Mathematical functions - numeric", "8.5.1. abs()", cypher,
        "
        MATCH (a),(e)
        WHERE a.name = 'Alice' AND e.name = 'Eskil'
        RETURN a.age, e.age, abs(a.age - e.age)
    "},
    % --------------------------------------------------------------------------
    {"Chapter 8. Functions", "8.5. Mathematical functions - numeric", "8.5.2. ceil()", cypher,
        "
        RETURN ceil(0.1)
    "},
    % --------------------------------------------------------------------------
    {"Chapter 8. Functions", "8.5. Mathematical functions - numeric", "8.5.3. floor()", cypher,
        "
        RETURN floor(0.9)
    "},
    % --------------------------------------------------------------------------
    {"Chapter 8. Functions", "8.5. Mathematical functions - numeric", "8.5.4. rand()", cypher,
        "
        RETURN rand()
    "},
    % --------------------------------------------------------------------------
    {"Chapter 8. Functions", "8.5. Mathematical functions - numeric", "8.5.5. round()", cypher,
        "
        RETURN round(3.141592)
    "},
    % --------------------------------------------------------------------------
    {"Chapter 8. Functions", "8.5. Mathematical functions - numeric", "8.5.6. sign()", cypher,
        "
        RETURN sign(-17), sign(0.1)
    "},
    % --------------------------------------------------------------------------
    {"Chapter 8. Functions", "8.6. Mathematical functions - logarithmic", "8.6.1. e()", cypher,
        "
        RETURN e()
    "},
    % --------------------------------------------------------------------------
    {"Chapter 8. Functions", "8.6. Mathematical functions - logarithmic", "8.6.2. exp()", cypher,
        "
        RETURN exp(2)
    "},
    % --------------------------------------------------------------------------
    {"Chapter 8. Functions", "8.6. Mathematical functions - logarithmic", "8.6.3. log()", cypher,
        "
        RETURN log(27)
    "},
    % --------------------------------------------------------------------------
    {"Chapter 8. Functions", "8.6. Mathematical functions - logarithmic", "8.6.4. log10()", cypher,
        "
        RETURN log10(27)
    "},
    % --------------------------------------------------------------------------
    {"Chapter 8. Functions", "8.6. Mathematical functions - logarithmic", "8.6.5. sqrt()", cypher,
        "
        RETURN sqrt(256)
    "},
    % --------------------------------------------------------------------------
    {"Chapter 8. Functions", "8.7. Mathematical functions - trigonometric", "8.7.1. acos()", cypher,
        "
        RETURN acos(0.5)
    "},
    % --------------------------------------------------------------------------
    {"Chapter 8. Functions", "8.7. Mathematical functions - trigonometric", "8.7.2. asin()", cypher,
        "
        RETURN asin(0.5)
    "},
    % --------------------------------------------------------------------------
    {"Chapter 8. Functions", "8.7. Mathematical functions - trigonometric", "8.7.3. atan()", cypher,
        "
        RETURN atan(0.5)
    "},
    % --------------------------------------------------------------------------
    {"Chapter 8. Functions", "8.7. Mathematical functions - trigonometric", "8.7.4. atan2()", cypher,
        "
        RETURN atan2(0.5, 0.6)
    "},
    % --------------------------------------------------------------------------
    {"Chapter 8. Functions", "8.7. Mathematical functions - trigonometric", "8.7.5. cos()", cypher,
        "
        RETURN cos(0.5)
    "},
    % --------------------------------------------------------------------------
    {"Chapter 8. Functions", "8.7. Mathematical functions - trigonometric", "8.7.6. cot()", cypher,
        "
        RETURN cot(0.5)
    "},
    % --------------------------------------------------------------------------
    {"Chapter 8. Functions", "8.7. Mathematical functions - trigonometric", "8.7.7. degrees()", cypher,
        "
        RETURN degrees(3.14159)
    "},
    % --------------------------------------------------------------------------
    {"Chapter 8. Functions", "8.7. Mathematical functions - trigonometric", "8.7.8. haversin()", cypher,
        "
        RETURN haversin(0.5)
    "},
    % --------------------------------------------------------------------------
    {"Chapter 8. Functions", "8.7. Mathematical functions - trigonometric", "8.7.9. Spherical distance using the haversin() function", cypher,
        "
        CREATE (ber:City { lat: 52.5, lon: 13.4 }),(sm:City { lat: 37.5, lon: -122.3 })
        RETURN 2 * 6371 * asin(sqrt(haversin(radians(sm.lat - ber.lat))+ cos(radians(sm.lat))*
        cos(radians(ber.lat))* haversin(radians(sm.lon - ber.lon)))) AS dist
    "},
    % --------------------------------------------------------------------------
    {"Chapter 8. Functions", "8.7. Mathematical functions - trigonometric", "8.7.10. pi()", cypher,
        "
        RETURN pi()
    "},
    % --------------------------------------------------------------------------
    {"Chapter 8. Functions", "8.7. Mathematical functions - trigonometric", "8.7.11. radians()", cypher,
        "
        RETURN radians(180)
    "},
    % --------------------------------------------------------------------------
    {"Chapter 8. Functions", "8.7. Mathematical functions - trigonometric", "8.7.12. sin()", cypher,
        "
        RETURN sin(0.5)
    "},
    % --------------------------------------------------------------------------
    {"Chapter 8. Functions", "8.7. Mathematical functions - trigonometric", "8.7.13. tan()", cypher,
        "
        RETURN tan(0.5)
    "},
    % --------------------------------------------------------------------------
    {"Chapter 8. Functions", "8.8. String functions", "8.8.1. left()", cypher,
        "
        RETURN left('hello', 3)
    "},
    % --------------------------------------------------------------------------
    {"Chapter 8. Functions", "8.8. String functions", "8.8.2. ltrim()", cypher,
        "
        RETURN lTrim(' hello')
    "},
    % --------------------------------------------------------------------------
    {"Chapter 8. Functions", "8.8. String functions", "8.8.3. replace()", cypher,
        "
        RETURN replace(\"hello\", \"l\", \"w\")
    "},
    % --------------------------------------------------------------------------
    {"Chapter 8. Functions", "8.8. String functions", "8.8.4. reverse()", cypher,
        "
        RETURN reverse('anagram')
    "},
    % --------------------------------------------------------------------------
    {"Chapter 8. Functions", "8.8. String functions", "8.8.5. right()", cypher,
        "
        RETURN right('hello', 3)
    "},
    % --------------------------------------------------------------------------
    {"Chapter 8. Functions", "8.8. String functions", "8.8.6. rtrim()", cypher,
        "
        RETURN rTrim('hello ')
    "},
    % --------------------------------------------------------------------------
    {"Chapter 8. Functions", "8.8. String functions", "8.8.7. split()", cypher,
        "
        RETURN split('one,two', ',')
    "},
    % --------------------------------------------------------------------------
    {"Chapter 8. Functions", "8.8. String functions", "8.8.8. substring()", cypher,
        "
        RETURN substring('hello', 1, 3), substring('hello', 2)
    "},
    % --------------------------------------------------------------------------
    {"Chapter 8. Functions", "8.8. String functions", "8.8.9. toLower()", cypher,
        "
        RETURN toLower('HELLO')
    "},
    % --------------------------------------------------------------------------
    {"Chapter 8. Functions", "8.8. String functions", "8.8.10. toString()", cypher,
        "
        RETURN toString(11.5), toString('already a string'), toString(TRUE )
    "},
    % --------------------------------------------------------------------------
    {"Chapter 8. Functions", "8.8. String functions", "8.8.11. toUpper()", cypher,
        "
        RETURN toUpper('hello')
    "},
    % --------------------------------------------------------------------------
    {"Chapter 8. Functions", "8.8. String functions", "8.8.12. trim()", cypher,
        "
        RETURN trim(' hello ')
    "},
    % --------------------------------------------------------------------------
    {"Chapter 8. Functions", "8.9. Spatial functions", "8.9.1. distance()", cypher,
        "
        WITH point({ x: 2.3, y: 4.5, crs: 'cartesian' }) AS p1, point({ x: 1.1, y: 5.4, crs: 'cartesian' }) AS p2
        RETURN distance(p1,p2) AS dist
    "},
    % --------------------------------------------------------------------------
    {"Chapter 8. Functions", "8.9. Spatial functions", "8.9.1. distance()", cypher,
        "
        MATCH (t:TrainStation)-[:TRAVEL_ROUTE]->(o:Office)
        WITH point({ longitude: t.longitude, latitude: t.latitude }) AS trainPoint, point({ longitude:
        o.longitude, latitude: o.latitude }) AS officePoint
        RETURN round(distance(trainPoint, officePoint)) AS travelDistance
    "},
    % --------------------------------------------------------------------------
    {"Chapter 8. Functions", "8.9. Spatial functions", "8.9.1. distance()", cypher,
        "
        RETURN distance(NULL , point({ longitude: 56.7, latitude: 12.78 })) AS d
    "},
    % --------------------------------------------------------------------------
    {"Chapter 8. Functions", "8.9. Spatial functions", "8.9.2. point() - WGS 84", cypher,
        "
        RETURN point({ longitude: 56.7, latitude: 12.78 }) AS point
    "},
    % --------------------------------------------------------------------------
    {"Chapter 8. Functions", "8.9. Spatial functions", "8.9.2. point() - WGS 84", cypher,
        "
        RETURN point({ x: 2.3, y: 4.5, crs: 'WGS-84' }) AS point
    "},
    % --------------------------------------------------------------------------
    {"Chapter 8. Functions", "8.9. Spatial functions", "8.9.2. point() - WGS 84", cypher,
        "
        MATCH (p:Office)
        RETURN point({ longitude: p.longitude, latitude: p.latitude }) AS officePoint
    "},
    % --------------------------------------------------------------------------
    {"Chapter 8. Functions", "8.9. Spatial functions", "8.9.2. point() - WGS 84", cypher,
        "
        RETURN point(NULL ) AS p
    "},
    % --------------------------------------------------------------------------
    {"Chapter 8. Functions", "8.9. Spatial functions", "8.9.3. point() - cartesian 2D", cypher,
        "
        RETURN point({ x: 2.3, y: 4.5 }) AS point
    "}
%% wwe legacy - function_name not schema_name
%%    % --------------------------------------------------------------------------
%%    {"Chapter 8. Functions", "8.10. User-defined functions", "8.10.1. Call a user-defined function", cypher,
%%        "
%%        MATCH (n:Member)
%%        RETURN org.neo4j.function.example.join(collect(n.name)) AS members
%%    "},
%%    % --------------------------------------------------------------------------
%%    {"Chapter 8. Functions", "8.10. User-defined functions", "8.10.3. Call a user-defined aggregation function", cypher,
%%        "
%%        MATCH (n:Member)
%%        RETURN org.neo4j.function.example.longestString(n.name) AS member
%%    "}
]).


-define(TIMETRAP_MINUTES, 10).


























