# ocparse - the openCypher parser written in Erlang

[![Build Status](https://travis-ci.org/walter-weinmann/ocparse.svg?branch=master)](https://travis-ci.org/walter-weinmann/ocparse)

**ocparse** is a production-ready [openCypher](https://github.com/opencypher/openCypher) parser written in pure Erlang.  **ocparse** is closely aligned to the [openCypher](https://github.com/opencypher/openCypher) project and in future will be adapted on a regular basis as the [openCypher](https://github.com/opencypher/openCypher) project evolves. The [openCypher project](http://www.opencypher.org) aims to deliver a full and open specification of the industry’s most widely adopted graph database query language: [Cypher](https://neo4j.com/docs/developer-manual/current/#cypher-query-lang). And, with the [EBNF file](https://s3.amazonaws.com/artifacts.opencypher.org/cypher.ebnf) the project provides the basis for the definition of the LALR grammar. 

### Legacy version:

Not all grammar rules of Cypher the language will be standardised in their current form, meaning that they will not be part of openCypher as-is. Therefore, the openCypher grammar will not include some well-known Cypher constructs; these are called 'legacy'. To still enable tool authors or others interested in developing support for Cypher in its full current shape, these legacy rules are still present in the the legacy version of the parser. The legacy version of the parser uses `ocparse_legacy` instead of `ocparse` etc.

## 1. Usage

### Example code:

```
MATCH (m:Movie) 
WHERE m.title = 'The Matrix' 
RETURN m
```

### Parsing the example code:

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

### Access the parse tree of the example code:

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
4> ocparse:parsetree_to_string(ParseTree).

<<"match (m:Movie) where m .title = 'The Matrix' return m">>
``` 

### Complete parse tree:

The output of the parse tree in the Erlang shell is shortened (cause not known). The complete parse tree of the example code looks as follows:

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

## 2. Documentation

The documentation for **ocparse** is available here: [Wiki](https://github.com/walter-weinmann/ocparse/wiki).

## 3. Known issues

### Comment

The number of block comments (`/* ... */`) is limted to one per line.

### Expression3

The variant `[..]` is not supported.

### RangeLiteral

A `RangeLiteral` without `IntegerLiteral` is not supported.

### ParenthesizedExpression

`ParenthesizedExpression` is not supported due to a conflict with `NodePattern`.

### RegularDecimalReal (Legacy)

`RegularDecimalReal` requires at least one digit before the decimal point.

### SymbolicName

The following tokens may not be used as `SymbolicName`:

```
 ALL, AND, ANY, AS, ASC, ASCENDING, BY, CONTAINS, COUNT, CREATE, DELETE, DESC, DESCENDING, 
 DETACH, DIGIT_STRING, DISTINCT, ENDS, ESCAPED_SYMBOLIC_NAME, EXISTS, EXPONENT_DECIMAL_REAL, 
 EXTRACT, FALSE, FILTER, HEX_INTEGER, IN, IS, LIMIT, MATCH, MERGE, NONE, NOT, NULL, ON, 
 OPTIONAL, OR, ORDER, REMOVE, RETURN, SET, SIGNED_DECIMAL_INTEGER, SIGNED_OCTAL_INTEGER, 
 SIGNED_REGULAR_DECIMAL_REAL, SINGLE, SKIP, STARTS, STRING_LITERAL, TRUE, UNESCAPED_SYMBOLIC_NAME, 
 UNION, UNSIGNED_DECIMAL_INTEGER, UNSIGNED_OCTAL_INTEGER, UNSIGNED_REGULAR_DECIMAL_REAL, UNWIND, 
 WHERE, WITH, XOR
```

An exception is the use of the tokens `COUNT` and `EXISTS` as `FunctionName`.

In the legacy version of the parser the following tokens are also excluded as `SymbolicName`:

```
 ALLSHORTESTPATHS, ASSERT, CASE, COMMIT, CONSTRAINT, CSV, CYPHER, DROP, ELSE, END, EXPLAIN, 
 FIELDTERMINATOR, FOREACH, FROM, HEADERS, INDEX, JOIN, LOAD, NODE, PERIODIC, PROFILE, REDUCE, 
 REL, RELATIONSHIP, SCAN, SHORTESTPATH, START, THEN, UNIQUE, USING, WHEN
```

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

## 6. Release Notes

### Version 1.2.0

Release Date: 01.08.2016 - Grammar as of 31.07.2016

#### Grammar changes

- **VersionNumber** (Legacy)

```
New: VersionNumber = DecimalInteger, '.', DecimalInteger ;

Old: VersionNumber = DigitString, '.', DigitString ;
```

- **PeriodicCommitHint** (Legacy)

```
New: PeriodicCommitHint = (U,S,I,N,G), SP, (P,E,R,I,O,D,I,C), SP, (C,O,M,M,I,T), [SP] ;

Old: PeriodicCommitHint = (U,S,I,N,G), SP, (P,E,R,I,O,D,I,C), SP, (C,O,M,M,I,T), [SP, SignedIntegerLiteral] ;
```


- **IdentifiedIndexLookup** (Legacy)

```
New: IdentifiedIndexLookup = ':', SymbolicName, '(', SymbolicName, '=', (StringLiteral | LegacyParameter), ')' ;

Old: IdentifiedIndexLookup = ':', SymbolicName, '(', SymbolicName, '=', (StringLiteral | Parameter), ')' ;
```


- **IndexQuery** (Legacy)

```
New: IndexQuery = ':', SymbolicName, '(', (StringLiteral | LegacyParameter), ')' ;

Old: IndexQuery = ':', SymbolicName, '(', (StringLiteral | Parameter), ')' ;
```


- **IdLookup** (Legacy)

```
New: IdLookup = '(', (LiteralIds | LegacyParameter | '*'), ')' ;

Old: IdLookup = '(', (LiteralIds | Parameter | '*'), ')' ;
```


- **LiteralIds** (Legacy)

```
New: LiteralIds = { WS, ',', WS } ;

Old: LiteralIds = UnsignedIntegerLiteral, { WS, ',', WS, UnsignedIntegerLiteral } ;
```

- **Pattern**

```
New: Pattern = PatternPart, { WS, ',', WS, PatternPart } ;

Old: Pattern = PatternPart, { ',', PatternPart } ;
```

- **Properties** (Legacy)

```
New: Properties = MapLiteral | Parameter | LegacyParameter ;

Old: Properties = MapLiteral | Parameter ;
```

- **RangeLiteral**

```
New: RangeLiteral = WS, [IntegerLiteral, WS], ['..', WS, [IntegerLiteral, WS]] ;

Old: RangeLiteral = WS, [UnsignedIntegerLiteral, WS], ['..', WS, [UnsignedIntegerLiteral, WS]] ;
```

- **Atom** (Legacy)

```
New: Atom = ... | LegacyParameter | ...

Old: n/a
```

- **Atom**

```
New: Atom = ... | ParenthesizedExpression | ...

Old: Atom = ... | parenthesizedExpression | ...
```

- **ParenthesizedExpression**

```
New: ParenthesizedExpression = '(', WS, Expression, WS, ')' ;

Old: parenthesizedExpression = '(', WS, Expression, WS, ')' ;
```

- **NumberLiteral**

```
New: NumberLiteral = ... | IntegerLiteral
                   
Old: NumberLiteral = ... | SignedIntegerLiteral
```

- **LegacyParameter** (Legacy)

```
New: LegacyParameter = '{', WS, (SymbolicName | DecimalInteger), WS, '}' ;

Old: n/a
```

- **Parameter**

```
New: Parameter = '$', (SymbolicName | DecimalInteger) ;
                   
Old: Parameter = '{', WS, (SymbolicName | UnsignedDecimalInteger), WS, '}' ;
```

- **IntegerLiteral**

```
New: IntegerLiteral = ...
                   
Old: SignedIntegerLiteral = ...
```

- **UnsignedIntegerLiteral**

```
New: n/a
                   
Old: UnsignedIntegerLiteral = UnsignedDecimalInteger ;
```

- **HexInteger**

```
New: HexInteger = ('0',X), HexString ;

Old: HexInteger = ['-'], UnsignedHexInteger ;
```

- **DecimalInteger**

```
New: DecimalInteger = (('1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'), [DigitString])
                    | '0'

Old: DecimalInteger = ['-'], UnsignedDecimalInteger ;
```

- **OctalInteger**

```
New: OctalInteger = '0', OctalString ;

Old: OctalInteger = ['-'], UnsignedOctalInteger ;
```

- **UnsignedHexInteger**

```
New: n/a

Old: UnsignedHexInteger = ('0',X), HexString ;
```

- **UnsignedDecimalInteger**

```
New: n/a

Old: UnsignedDecimalInteger = (('1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'), [DigitString])
                            | '0'
```

- **UnsignedOctalInteger**

```
New: n/a

Old: UnsignedOctalInteger = '0', OctalString ;
```

- **ExponentDecimalReal**

```
New: ExponentDecimalReal = ({ Digit | '.' }- | DecimalInteger), ((E) | (E)), (DigitString | DecimalInteger) ;

Old: ExponentDecimalReal = ['-'], { Digit | '.' }-, ((E) | (E)), ['-'], DigitString ;
```

- **RegularDecimalReal**

```
New: RegularDecimalReal = ({ Digit } | DecimalInteger), '.', (DigitString | DecimalInteger) ;

Old: RegularDecimalReal = ['-'], { Digit }, '.', DigitString ;
```

### Version 1.1.1

Release Date: 11.07.2016 - Grammar as of 10.07.2016

#### Grammar changes

- **Clause** (Legacy)

```
New: Clause = ... Create CreateUnique Set ... ;

Old: Clause = ... Create Set ... ;
```
    
- **CreateUnique** (Legacy)

```
New: Create = (C,R,E,A,T,E), WS, Pattern ;
     CreateUnique = (C,R,E,A,T,E), SP, (U,N,I,Q,U,E), WS, Pattern ;
     
Old: Create = ((C,R,E,A,T,E), SP, (U,N,I,Q,U,E), WS, Pattern)
            | ((C,R,E,A,T,E), WS, Pattern) 
            ;
```
    
- **RelationshipDetail**

```
New: RelationshipDetail = '[', [Variable], ['?'], [RelationshipTypes], ['*',  RangeLiteral],  [Properties], ']' ;

Old: RelationshipDetail = '[', [Variable], ['?'], [RelationshipTypes], ['*', [RangeLiteral]], [Properties], ']' ;
```

- **RangeLiteral**

```
New: RangeLiteral = WS, [UnsignedIntegerLiteral, WS], ['..', WS, [UnsignedIntegerLiteral, WS]] ;

Old: RangeLiteral =     [UnsignedIntegerLiteral, WS],  '..',[WS,  UnsignedIntegerLiteral] ;
```

----------

### Version 1.1.0

Release Date: 08.07.2016

#### Issues

1. Correction in definition of Atom with MapLiteral and Parameter.
2. Test case removed, since multiple statements are not supported (see [openParser #113](https://github.com/opencypher/openCypher/issues/113)).
3. PropertyKeyNameExpression removed from parse tree.

#### New features

1. Support of the legacy grammar.

----------

### Version 1.0.0

Release Date: 07.07.2016
