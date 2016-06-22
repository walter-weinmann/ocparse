ocparse <a href="https://magnum.travis-ci.com/k2informatics/sqlparse"><img src="https://travis-ci.org/K2InformaticsGmbH/sqlparse.svg" alt="Travis-CI"></a>
=======

LALR grammar based Cypher parser using the grammar rules from the [openCypher](https://github.com/opencypher/openCypher) project.

Example use
-----------
Parsing
````erlang
1> {ok, {ParseTree, Tokens}} = ocparse:parsetree_with_tokens("profile cypher 2.2 planner=cost create index on :Actor(name)").
2> ParseTree.
{cypher,
    {queryOptions,
        [{anyCypherOption,profile,[]},
         {anyCypherOption,
             {cypherOption,
                 {{versionNumber,"2.2"},
                  [{configurationOption,
                       {symbolicName,"planner"},
                       {symbolicName,"cost"}}]}}}]},
    {statement,
        {command,
            {createIndex,
                {index,
                    {{nodeLabel,{labelName,{symbolicName,"Actor"}}},
                     {propertyKeyName,{symbolicName,"name"}}}}}}}}
3> Tokens.
[{'PROFILE',1},
 {'CYPHER',1},
 {'UNSIGNED_FLOAT',1,"2.2"},
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
* [Atoms](https://github.com/walter-weinmann/ocparse/blob/master/test/atoms.tst)
* [Command CONSTRAINT](https://github.com/walter-weinmann/ocparse/blob/master/test/command_constraint.tst)
* [Command DELETE](https://github.com/walter-weinmann/ocparse/blob/master/test/command_delete.tst)
* [Command INDEX](https://github.com/walter-weinmann/ocparse/blob/master/test/command_index.tst)
* [Command MATCH](https://github.com/walter-weinmann/ocparse/blob/master/test/command_match.tst)
* [Expressions](https://github.com/walter-weinmann/ocparse/blob/master/test/expressions.tst)
* [Patterns](https://github.com/walter-weinmann/ocparse/blob/master/test/patterns.tst)
* [Query Options](https://github.com/walter-weinmann/ocparse/blob/master/test/query_options.tst)

These test cases are also documentation of the current support.

* `SET CYPHER=<test>` and run with `rebar eunit skip_deps=true` one specific <test>.tst file  from `test/` folder
* `SET LOG=0,1,2,3,4,5` and enable with `rebar eunit skip_deps=true` different types of log, one or more logs can be enabled, default 0

level|type
---|---
0|only errors
1|test cypher
2|parse tree of test cypher
3|fold cypher
4|parse tree og fold cypher
5|unused
