ocparse <a href="https://magnum.travis-ci.com/k2informatics/sqlparse"><img src="https://travis-ci.org/K2InformaticsGmbH/sqlparse.svg" alt="Travis-CI"></a>
=======

LALR grammar based open cypher parser.

The project is based on the work of the [Cypher Language Group](https://github.com/opencypher/openCypher).

Example use
-----------
Parsing
````erlang
1> {ok, {ParseTree, Tokens}} = ocparse:parsetree_with_tokens("select * from table_1").
2> ParseTree.
[{{select,[{hints,<<>>},
           {opt,<<>>},
           {fields,[<<"*">>]},
           {into,[]},
           {from,[<<"table_1">>]},
           {where,{}},
           {'hierarchical query',{}},
           {'group by',[]},
           {having,{}},
           {'order by',[]}]},
  {extra,<<>>}}]
3> Tokens.
[{'SELECT',1},
 {'*',1},
 {'FROM',1},
 {'NAME',7,"table_1"},
 {';',1}]
````
Compiling
````erlang
4> ocparse:fold(ParseTree).
"select * from table_1"
````

Test Cases
---
* [ALTER](https://github.com/K2InformaticsGmbH/ocparse/blob/master/test/alter.tst)
* [CREATE](https://github.com/K2InformaticsGmbH/ocparse/blob/master/test/create.tst)
* [DELETE](https://github.com/K2InformaticsGmbH/ocparse/blob/master/test/delete.tst)
* [GRANTS](https://github.com/K2InformaticsGmbH/ocparse/blob/master/test/grants.tst)
* [INDEX](https://github.com/K2InformaticsGmbH/ocparse/blob/master/test/index.tst)
* [INSERT](https://github.com/K2InformaticsGmbH/ocparse/blob/master/test/insert.tst)
* [JSON Path](https://github.com/K2InformaticsGmbH/ocparse/blob/master/test/jsonpath.tst)

These test cases are also documentation of the current support.

* `cypher=<test> rebar eunit skip_deps=true` to run one specific <test>.tst file  from `test/` folder
* `LOG=0,1,2,3,4,5 rebar eunit skip_deps=true` to enable different types of log, one or more logs can be enabled, default 0

level|type
---|---
0|only errors
1|test opencypher
2|parse tree of test opencypher
3|fold opencypher
4|parse tree og fold opencypher
5|unused
