# ocparse - the openCypher parser written in Erlang

[![Build Status](https://travis-ci.org/walter-weinmann/ocparse.svg?branch=master)](https://travis-ci.org/walter-weinmann/ocparse)

**ocparse** is a production-ready [openCypher](https://github.com/opencypher/openCypher) parser written in pure Erlang.  **ocparse** is closely aligned to the openCypher project and in future will be adapted on a regular basis as the openCypher project evolves. The [openCypher project](http://www.opencypher.org) aims to deliver a full and open specification of the industry’s most widely adopted graph database query language: [Cypher](https://neo4j.com/docs/developer-manual/current/#cypher-query-lang). And, with the [EBNF file](https://s3.amazonaws.com/artifacts.opencypher.org/cypher.ebnf) the project provides the basis for the definition of the LALR grammar.

## 1. Usage

### Example code:

```
MATCH (m:Movie)
WHERE m.title = 'The Matrix'
RETURN m
```

### Parsing the example code:

```erlang
1> {ok, {ParseTree, Tokens}} = ocparse:source_to_pt("MATCH (m:Movie) WHERE m.title = 'The Matrix' RETURN m").
{ok,
 {{cypher,
   {regularQuery,
    {singleQuery,
     [{clause,
       {match,[],
        {pattern,
         [{patternPart,[],
           {patternElement,
            {nodePattern,{symbolicName,"m"},{nodeLabels,[{...}]},[]},
            []}}]},
        {where,
         {orExpression,
          {xorExpression,
           {andExpression,
            {notExpression,
             {comparisonExpression,{addOrSubtractExpression,...},[...]},
             []},
            []},
           []},
          []}}}},
      {clause,
       {return,[],
        {returnBody,
         {returnItems,[],[],
          [{returnItem,
            {orExpression,{xorExpression,{...},...},[]},
            []}]},
         [],[],[]}}}]},
    []},
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

### Access the parse tree of the example code:

```erlang
2> ParseTree.

{cypher,
 {regularQuery,
  {singleQuery,
   [{clause,
     {match,[],
      {pattern,
       [{patternPart,[],
         {patternElement,
          {nodePattern,
           {symbolicName,"m"},
           {nodeLabels,[{nodeLabel,{symbolicName,...}}]},
           []},
          []}}]},
      {where,
       {orExpression,
        {xorExpression,
         {andExpression,
          {notExpression,
           {comparisonExpression,
            {addOrSubtractExpression,
             {multiplyDivideModuloExpression,{...},...},
             []},
            [{partialComparisonExpression,{...},...}]},
           []},
          []},
         []},
        []}}}},
    {clause,
     {return,[],
      {returnBody,
       {returnItems,[],[],
        [{returnItem,
          {orExpression,
           {xorExpression,{andExpression,{notExpression,...},[]},[]},
           []},
          []}]},
       [],[],[]}}}]},
  []},
 []}
```

### Access the token list of the example code:

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

### Compile the code from a parse tree:

```erlang
4> ocparse:pt_to_source_td(ParseTree).

<<"match (m :Movie) where m .title = 'The Matrix' return m">>
```

```erlang
5> ocparse:pt_to_source_bu(ParseTree).

<<"match (m :Movie) where m .title = 'The Matrix' return m">>
```

### Complete parse tree:

The output of the parse tree in the Erlang shell is shortened (cause not known). The complete parse tree of the example code looks as follows:

```erlang
{cypher,
 {regularQuery,
  {singleQuery,
   [{clause,
     {match,[],
      {pattern,
       [{patternPart,[],
         {patternElement,
          {nodePattern,
           {symbolicName,"m"},
           {nodeLabels,[{nodeLabel,{symbolicName,"Movie"}}]},
           []},
          []}}]},
      {where,
       {orExpression,
        {xorExpression,
         {andExpression,
          {notExpression,
           {comparisonExpression,
            {addOrSubtractExpression,
             {multiplyDivideModuloExpression,
              {powerOfExpression,
               {unaryAddOrSubtractExpression,
                {stringListNullOperatorExpression,
                 {propertyOrLabelsExpression,
                  {atom,{symbolicName,"m"}},
                  [{propertyLookup,{symbolicName,"title"}}]},
                 []},
                []},
               []},
              []},
             []},
            [{partialComparisonExpression,
              {addOrSubtractExpression,
               {multiplyDivideModuloExpression,
                {powerOfExpression,
                 {unaryAddOrSubtractExpression,
                  {stringListNullOperatorExpression,
                   {propertyOrLabelsExpression,
                    {atom,
                     {literal,{stringLiteral,"'The Matrix'"}}},
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
        []}}}},
    {clause,
     {return,[],
      {returnBody,
       {returnItems,[],[],
        [{returnItem,
          {orExpression,
           {xorExpression,
            {andExpression,
             {notExpression,
              {comparisonExpression,
               {addOrSubtractExpression,
                {multiplyDivideModuloExpression,
                 {powerOfExpression,
                  {unaryAddOrSubtractExpression,
                   {stringListNullOperatorExpression,
                    {propertyOrLabelsExpression,
                     {atom,{symbolicName,"m"}},
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
           []},
          []}]},
       [],[],[]}}}]},
  []},
 []}
```

## 2. Documentation

The documentation for **ocparse** is available here: [Wiki](https://github.com/walter-weinmann/ocparse/wiki).

## 3. Known issues

### Comment

The number of block comments (`/* ... */`) is limted to one per line.

### ParenthesizedExpression

`ParenthesizedExpression` is not supported due to a conflict with `NodePattern`.

### RelationshipDetail

The empty `RelationshipDetail` `[]` is not supported.

### SymbolicName

The following tokens may not be used as `SymbolicName`:

```
  ALL AND ANY AS ASC ASCENDING BY CONTAINS COUNT CREATE DECIMAL_INTEGER DELETE
  DESC DESCENDING DETACH DISTINCT ENDS ESCAPED_SYMBOLIC_NAME EXPONENT_DECIMAL_REAL
  EXTRACT FALSE FILTER HEX_INTEGER IN IS LIMIT MATCH MERGE NONE NOT NULL
  OCTAL_INTEGER ON OPTIONAL OR ORDER REGULAR_DECIMAL_REAL REMOVE RETURN SET
  SINGLE SKIP STARTS STRING_LITERAL TRUE UNESCAPED_SYMBOLIC_NAME UNION UNWIND
  WHERE WITH XOR
```

An exception is the use of the token `COUNT` as `FunctionName`.

### Unicode

Unicode is not supported with `Dash`, `LeftArrowHead`, `RightArrowHerad` or `UnescapedSymbolicName`. Hence `Dash` is limited to the hyphen (`-`), `LeftArrowHead` is limited to '`<`' and `RightArrowHead` is limited to '`>`'.

## 4. Acknowledgement

This project was inspired by the [sqlparse](https://github.com/K2InformaticsGmbH/sqlparse) project of the company [K2 Informatics GmbH](http://www.k2informatics.ch).

## 5. Contributing

1. Fork it
2. Create your feature branch (`git checkout -b my-new-feature`)
3. Commit your changes (`git commit -am 'Add some feature'`)
4. Push to the branch (`git push origin my-new-feature`)
5. Create new Pull Request
