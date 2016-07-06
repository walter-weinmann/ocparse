# ocparse

[![Build Status](https://travis-ci.org/walter-weinmann/ocparse.svg?branch=master)](https://travis-ci.org/walter-weinmann/ocparse)

**ocparse** is a production-ready [openCypher](https://github.com/opencypher/openCypher) parser written in pure Erlang. The [openCypher project](https://http://www.opencypher.org) aims to deliver a full and open specification of the industry’s most widely adopted graph database query language: [Cypher](http://neo4j.com/docs/developer-manual/current/#cypher-query-lang). The [openCypher](https://github.com/opencypher/openCypher) project provides with the [EBNF file](https://s3.amazonaws.com/artifacts.opencypher.org/cypher.ebnf) the basis for the definition of the grammar. In future **ocparse** will follow the [openCypher](https://github.com/opencypher/openCypher) project very closely.

## Usage

Example code:

```
MATCH (m:Movie) 
WHERE m.title = 'The Matrix' 
RETURN m
```

Parsing the example code:

```erlang
1> {ok, {ParseTree, Tokens}} = ocparse:parsetree_with_tokens("MATCH (m:Movie) WHERE m.title = 'The Matrix' RETURN m").

{ok,
 {{cypher,{},
   {statement,
    {query,
     {regularQuery,
      {singleQuery,
       [{clause,
         {match,[],
          {pattern,
           [{patternPart,
             {anonymousPatternPart,{patternElement,{...},...}}}]},
          [],
          {where,{expression,{expression12,{expression11,...},[]}}}}},
        {clause,
         {return,[],
          {returnBody,
           {returnItems,[],[{returnItem,{...}}]},
           {},{},{}}}}]},
      []}}},
   []},
  [{'MATCH',1},
   {'(',1},
   {'UNESCAPED_SYMBOLIC_NAME',1,"m"},
   {':',1},
   {'UNESCAPED_SYMBOLIC_NAME',5,"Movie"},
   {')',1},
   {'WHERE',1},
   {'UNESCAPED_SYMBOLIC_NAME',1,"m"},
   {'.',1},
   {'UNESCAPED_SYMBOLIC_NAME',5,"title"},
   {'=',1},
   {'STRING_LITERAL',1,"'The Matrix'"},
   {'RETURN',1},
   {'UNESCAPED_SYMBOLIC_NAME',1,"m"}]}}
````   

Access the parse tree of the example code:

```erlang
2> ParseTree.

{cypher,{},
 {statement,
  {query,
   {regularQuery,
    {singleQuery,
     [{clause,
       {match,[],
        {pattern,
         [{patternPart,
           {anonymousPatternPart,
            {patternElement,
             {nodePattern,{variable,...},{...},...},
             []}}}]},
        [],
        {where,
         {expression,
          {expression12,
           {expression11,{expression10,{...},...},[]},
           []}}}}},
      {clause,
       {return,[],
        {returnBody,
         {returnItems,[],
          [{returnItem,{expression,{expression12,...}}}]},
         {},{},{}}}}]},
    []}}},
 []}
```    

Access the token list of the example code:

```erlang
3> Tokens.

[{'MATCH',1},
 {'(',1},
 {'UNESCAPED_SYMBOLIC_NAME',1,"m"},
 {':',1},
 {'UNESCAPED_SYMBOLIC_NAME',5,"Movie"},
 {')',1},
 {'WHERE',1},
 {'UNESCAPED_SYMBOLIC_NAME',1,"m"},
 {'.',1},
 {'UNESCAPED_SYMBOLIC_NAME',5,"title"},
 {'=',1},
 {'STRING_LITERAL',1,"'The Matrix'"},
 {'RETURN',1},
 {'UNESCAPED_SYMBOLIC_NAME',1,"m"}]
``` 

Compile the code from a parse tree:

```erlang
4> ocparse:parsetree_to_string(ParseTree).

<<"match (m:Movie) where m .title = 'The Matrix' return m">>
``` 

The output of the parse tree in the Erlang shell is somehow shortened. The complete parse tree of the example code looks as follows:

```erlang
{cypher,{},
 {statement,
  {query,
   {regularQuery,
    {singleQuery,
     [{clause,
       {match,[],
        {pattern,
         [{patternPart,
           {anonymousPatternPart,
            {patternElement,
             {nodePattern,
              {variable,{symbolicName,"m"}},
              {nodeLabels,
               [{nodeLabel,{labelName,{symbolicName,"Movie"}}}]},
              {}},
             []}}}]},
        [],
        {where,
         {expression,
          {expression12,
           {expression11,
            {expression10,
             {expression9,
              {expression8,
               {expression7,
                {expression6,
                 {expression5,
                  {expression4,
                   {expression3,
                    {expression2,
                     {atom,{variable,{symbolicName,"m"}}},
                     [{propertyLookup,
                       {propertyKeyName,{symbolicName,"title"}},
                       []}]},
                    []},
                   []},
                  []},
                 []},
                []},
               [{partialComparisonExpression,
                 {expression7,
                  {expression6,
                   {expression5,
                    {expression4,
                     {expression3,
                      {expression2,
                       {atom,{stringLiteral,"'The Matrix'"}},
                       []},
                      []},
                     []},
                    []},
                   []},
                  []},
                 "="}]},
              []},
             []},
            []},
           []}}}}},
      {clause,
       {return,[],
        {returnBody,
         {returnItems,[],
          [{returnItem,
            {expression,
             {expression12,
              {expression11,
               {expression10,
                {expression9,
                 {expression8,
                  {expression7,
                   {expression6,
                    {expression5,
                     {expression4,
                      {expression3,
                       {expression2,
                        {atom,{variable,{symbolicName,"m"}}},
                        []},
                       []},
                      []},
                     []},
                    []},
                   []},
                  []},
                 []},
                []},
               []},
              []}}}]},
         {},{},{}}}}]},
    []}}},
 []}
``` 

## Documentation

The documentation for **ocparse** is available here: [Wiki](https://github.com/walter-weinmann/ocparse/wiki).


## Known issues

### SymbolicName

The following tokens may not be used as `SymbolicName`:

```
 ALL, ALLSHORTESTPATHS, AND, ANY, AS, ASC, ASCENDING, ASSERT, BY, CASE, COMMIT, 
 CONSTRAINT, CONTAINS, COUNT, CREATE, CSV, CYPHER, DELETE, DESC, DESCENDING, 
 DETACH, DIGIT_STRING, DISTINCT, DROP, ELSE, END, ENDS, ESCAPED_SYMBOLIC_NAME, 
 EXISTS, EXPLAIN, EXPONENT_DECIMAL_REAL, EXTRACT, FALSE, FIELDTERMINATOR, FILTER, 
 FOREACH, FROM, HEADERS, HEX_INTEGER, IN, INDEX, IS, JOIN, LIMIT, LOAD, MATCH, 
 MERGE, NODE, NONE, NOT, NULL, ON, OPTIONAL, OR, ORDER, PERIODIC, PROFILE, REDUCE, 
 REL, RELATIONSHIP, REMOVE, RETURN, SCAN, SET, SHORTESTPATH, SIGNED_DECIMAL_INTEGER, 
 SIGNED_OCTAL_INTEGER, SIGNED_REGULAR_DECIMAL_REAL, SINGLE, SKIP, START, STARTS, 
 STRING_LITERAL, THEN, TRUE, UNESCAPED_SYMBOLIC_NAME, UNION, UNIQUE, UNSIGNED_DECIMAL_INTEGER, 
 UNSIGNED_OCTAL_INTEGER, UNSIGNED_REGULAR_DECIMAL_REAL, UNWIND, USING, WHEN, WHERE, WITH, XOR
```

An exception is the use of the tokens `COUNT` and `EXISTS` as `FunctionName`.

### Unicode

Unicode is not supported with `Dash`, `LeftArrowHead`, `RightArrowHerad` or `UnescapedSymbolicName`. Hence `Dash` is limited to the hyphen (-), `LeftArrowHead` is limited to '<' and `RightArrowHead` is limited to '>'.

## Acknowledgements

This project was inspired by the [sqlparse](https://github.com/K2InformaticsGmbH/sqlparse) project of the company [K2 Informatics GmbH](http://www.k2informatics.ch).

## Contributing

1. Fork it
2. Create your feature branch (`git checkout -b my-new-feature`)
3. Commit your changes (`git commit -am 'Add some feature'`)
4. Push to the branch (`git push origin my-new-feature`)
5. Create new Pull Request

## Release Notes

### Version 1.0.0

Release Date: 07.07.2016



