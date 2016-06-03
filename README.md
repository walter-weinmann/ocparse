ocparse <a href="https://magnum.travis-ci.com/k2informatics/sqlparse"><img src="https://travis-ci.org/K2InformaticsGmbH/sqlparse.svg" alt="Travis-CI"></a>
=======

LALR grammar based open cypher parser.

The project is based on the work of the [Cypher Language Group](https://github.com/opencypher/openCypher).

Example use
-----------
Parsing
````erlang
1> {ok, {ParseTree, Tokens}} = ocparse:parsetree_with_tokens("profile cypher 2.2 planner=cost create index on :Actor(name)").
2> ParseTree.
{cypher_statement,
    {query_options,
        [{profile,[]},
         {cypher,
             {{version,<<"2.2">>},
              {options,
                  [{option,{'NAME',7,"planner"},{'NAME',4,"cost"}}]}}}]},
    {statement,
        {'create index on',{{'NAME',5,"Actor"},{'NAME',4,"name"}}}}}
3> Tokens.
[{'PROFILE',1},
 {'CYPHER',1},
 {'VERSION_NUMBER',1,"2.2"},
 {'NAME',7,"planner"},
 {'=',1},
 {'NAME',4,"cost"},
 {'CREATE',1},
 {'INDEX',1},
 {'ON',1},
 {':',1},
 {'NAME',5,"Actor"},
 {'(',1},
 {'NAME',4,"name"},
 {')',1},
 {';',1}]
````
Compiling
````erlang
4> ocparse:pt_to_string(ParseTree).
<<"profile cypher 2.2 planner = cost create index on :Actor (name)">>
````

Test Cases
---
* [Index](https://github.com/walter-weinmann/ocparse/blob/master/test/index.tst)
* [Query Options](https://github.com/walter-weinmann/ocparse/blob/master/test/query_options.tst)

These test cases are also documentation of the current support.

* `SET CYPHER=<test>` and run with `rebar eunit skip_deps=true` one specific <test>.tst file  from `test/` folder
* `SET LOG=0,1,2,3,4,5` and enable with `rebar eunit skip_deps=true` different types of log, one or more logs can be enabled, default 0

level|type
---|---
0|only errors
1|test opencypher
2|parse tree of test opencypher
3|fold opencypher
4|parse tree og fold opencypher
5|unused
