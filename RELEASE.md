# ocparse - the openCypher parser written in Erlang

[![Build Status](https://travis-ci.org/walter-weinmann/ocparse.svg?branch=master)](https://travis-ci.org/walter-weinmann/ocparse)

# Release Notes

## Version 1.3.0

Release Date: dd.mm.yyyy - Grammar as of 01.04.2017

### Grammar changes

- **AddOrSubtractExpression**

```
New: AddOrSubtractExpression = MultiplyDivideModuloExpression, { ([SP], '+', [SP], MultiplyDivideModuloExpression) | ([SP], '-', [SP], MultiplyDivideModuloExpression) } ;

Old: Expression7 = Expression6, { ([SP], '+', [SP], Expression6) | ([SP], '-', [SP], Expression6) } ;
```

- **AndExpression**

```
New: AndExpression = NotExpression, { SP, (A,N,D), SP, NotExpression } ;

Old: Expression10 = Expression9, { SP, (A,N,D), SP, Expression9 } ;
```

- **Atom**

```
New: Atom = Literal
	      | Parameter
	      | ((C,O,U,N,T), [SP], '(', [SP], '*', [SP], ')')
	      | ListComprehension
	      | PatternComprehension
	      | ((F,I,L,T,E,R), [SP], '(', [SP], FilterExpression, [SP], ')')
          ...

Old: Atom = Literal
	      | Parameter
	      | ((C,O,U,N,T), [SP], '(', [SP], '*', [SP], ')')
	      | ListComprehension
	      | ((F,I,L,T,E,R), [SP], '(', [SP], FilterExpression, [SP], ')')
          ...
```

- **ComparisonExpression**

```
New: ComparisonExpression = AddOrSubtractExpression, { [SP], PartialComparisonExpression } ;

Old: Expression8 = Expression7, { [SP], PartialComparisonExpression } ;
```

- **Expression**

```
New: Expression = OrExpression ;

Old: Expression = Expression12 ;
```

- **HexInteger**

```
New: HexInteger = '0x', { HexDigit }- ;

Old: HexInteger = ('0',X), { HexDigit }- ;
```

- **MultiplyDivideModuloExpression**

```
New: MultiplyDivideModuloExpression = PowerOfExpression, { ([SP], '*', [SP], PowerOfExpression) | ([SP], '/', [SP], PowerOfExpression) | ([SP], '%', [SP], PowerOfExpression) } ;

Old: Expression6 = Expression5, { ([SP], '*', [SP], Expression5) | ([SP], '/', [SP], Expression5) | ([SP], '%', [SP], Expression5) } ;
```

- **NodeLabel**

```
New: NodeLabel = ':', [SP], LabelName ;

Old: NodeLabel = ':', LabelName ;
```

- **NotExpression**

```
New: NotExpression = { (N,O,T), [SP] }, ComparisonExpression ;

Old: Expression9 = { (N,O,T), [SP] }, Expression8 ;
```

- **OrExpression**

```
New: OrExpression = XorExpression, { SP, (O,R), SP, XorExpression } ;

Old: Expression12 = Expression11, { SP, (O,R), SP, Expression11 } ;
```

- **PartialComparisonExpression**

```
New: PartialComparisonExpression = ('=', [SP], AddOrSubtractExpression)
	                             | ('<>', [SP], AddOrSubtractExpression)
	                             | ('!=', [SP], AddOrSubtractExpression)
	                             | ('<', [SP], AddOrSubtractExpression)
	                             | ('>', [SP], AddOrSubtractExpression)
	                             | ('<=', [SP], AddOrSubtractExpression)
	                             | ('>=', [SP], AddOrSubtractExpression)
	                             ;

Old: PartialComparisonExpression = ('=', [SP], Expression7)
	                             | ('<>', [SP], Expression7)
	                             | ('!=', [SP], Expression7)
	                             | ('<', [SP], Expression7)
	                             | ('>', [SP], Expression7)
	                             | ('<=', [SP], Expression7)
	                             | ('>=', [SP], Expression7)
	                             ;
```

- **PatternComprehension**

```
New: PatternComprehension = '[', [SP], [Variable, [SP], '=', [SP]], RelationshipsPattern, [SP], [(W,H,E,R,E), [SP], Expression, [SP]], '|', [SP], Expression, [SP], ']' ;

Old: n/a
```

- **PowerOfExpression**

```
New: PowerOfExpression = UnaryAddOrSubtractExpression, { [SP], '^', [SP], UnaryAddOrSubtractExpression } ;

Old: Expression5 = Expression4, { [SP], '^', [SP], Expression4 } ;
```

- **PropertyLookup**

```
New: PropertyLookup = '.', [SP], (PropertyKeyName) ;

Old: PropertyLookup = [SP], '.', [SP], ((PropertyKeyName, ('?' | '!')) | PropertyKeyName) ;
```

- **PropertyOrLabelsExpression**

```
New: PropertyOrLabelsExpression = Atom, { [SP], (PropertyLookup | NodeLabels) } ;

Old: Expression2 = Atom, { PropertyLookup | NodeLabels } ;
```

- **RelationshipDetail**

```
New: RelationshipDetail = '[', [SP], [Variable, [SP]], [RelationshipTypes, [SP]], [RangeLiteral], [Properties, [SP]], ']' ;

Old: RelationshipDetail = '[', [Variable], ['?'], [RelationshipTypes], [RangeLiteral], [Properties], ']' ;
```

- **RelationshipTypes**

```
New: RelationshipTypes = ':', [SP], RelTypeName, { [SP], '|', [':'], [SP], RelTypeName } ;

Old: RelationshipTypes = ':', RelTypeName, { [SP], '|', [':'], [SP], RelTypeName } ;
```

- **Set**

```
New: Set = (S,E,T), [SP], SetItem, { ',', SetItem } ;

Old: Set = (S,E,T), SetItem, { ',', SetItem } ;
```

- **SetItem**

```
New: SetItem = (PropertyExpression, [SP], '=', [SP], Expression)
  	         | (Variable, [SP], '=', [SP], Expression)
	         | (Variable, [SP], '+=', [SP], Expression)
	         | (Variable, [SP], NodeLabels)
	         ;

Old: SetItem = (PropertyExpression, '=', Expression)
		     | (Variable, '=', Expression)
		     | (Variable, '+=', Expression)
		     | (Variable, NodeLabels)
		     ;
```

- **StringListNullOperatorExpression**

```
New: StringListNullOperatorExpression = PropertyOrLabelsExpression, { ([SP], '[', Expression, ']') | ([SP], '[', [Expression], '..', [Expression], ']') | ((([SP], '=~') | (SP, (I,N)) | (SP, (S,T,A,R,T,S), SP, (W,I,T,H)) | (SP, (E,N,D,S), SP, (W,I,T,H)) | (SP, (C,O,N,T,A,I,N,S))), [SP], PropertyOrLabelsExpression) | (SP, (I,S), SP, (N,U,L,L)) | (SP, (I,S), SP, (N,O,T), SP, (N,U,L,L)) } ;

Old: Expression3 = Expression2, { ([SP], '[', Expression, ']') | ([SP], '[', [Expression], '..', [Expression], ']') | ((([SP], '=~') | (SP, (I,N)) | (SP, (S,T,A,R,T,S), SP, (W,I,T,H)) | (SP, (E,N,D,S), SP, (W,I,T,H)) | (SP, (C,O,N,T,A,I,N,S))), [SP], Expression2) | (SP, (I,S), SP, (N,U,L,L)) | (SP, (I,S), SP, (N,O,T), SP, (N,U,L,L)) } ;
```

- **UnaryAddOrSubtractExpression**

```
New: UnaryAddOrSubtractExpression = { ('+' | '-'), [SP] }, StringListNullOperatorExpression ;

Old: Expression4 = { ('+' | '-'), [SP] }, Expression3 ;
```

- **XorExpression**

```
New: XorExpression = AndExpression, { SP, (X,O,R), SP, AndExpression } ;

Old: Expression11 = Expression10, { SP, (X,O,R), SP, Expression10 } ;
```

### New features

- ???.

### Omitted features

- Legacy version support is discontinued.


## Version 1.2.4 (OpenCypher 1.0.0-M04)

Release Date: 31.12.2016 - Grammar as of 20.12.2016

### Grammar changes

- **Atom**

```
New: Atom = Literal
          | Parameter
          | ((C,O,U,N,T), [SP], '(', [SP], '*', [SP], ')')
          | ListComprehension
          | ((F,I,L,T,E,R), [SP], '(', [SP], FilterExpression, [SP], ')')
          ...

Old: Atom = NumberLiteral
          | StringLiteral
          | Parameter
          | (T,R,U,E)
          | (F,A,L,S,E)
          | (N,U,L,L)
          | ((C,O,U,N,T), [SP], '(', [SP], '*', [SP], ')')
          | MapLiteral
          | ListComprehension
          | ('[', [SP], Expression, [SP], { ',', [SP], Expression, [SP] }, ']')
          | ((F,I,L,T,E,R), [SP], '(', [SP], FilterExpression, [SP], ')')
          ...     
```

- **BooleanLiteral**

```
New: BooleanLiteral = (T,R,U,E)
                    | (F,A,L,S,E)
                    ;

Old: n/a
```

- **DecimalInteger**

```
New: DecimalInteger = ZeroDigit
                    | (NonZeroDigit, { Digit })
                    ;

Old: DecimalInteger = (('1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'), [DigitString])
                    | '0'
                    ;
```

- **Delete**

```
New: Delete = [(D,E,T,A,C,H), SP], (D,E,L,E,T,E), [SP], Expression, { [SP], ',', [SP], Expression } ;

Old: Delete = ((D,E,L,E,T,E), Expression, { ',', Expression })
            | ((D,E,T,A,C,H), SP, (D,E,L,E,T,E), Expression, { ',', Expression })
            ;
```

- **Digit**

```
New: Digit = ZeroDigit
           | NonZeroDigit
           ;

Old: Digit = '0'
           | '1'
           | '2'
           | '3'
           | '4'
           | '5'
           | '6'
           | '7'
           | '8'
           | '9'
           ;
```

- **DigitString**

```
New: n/a

Old: DigitString = { Digit }- ;
```

- **ExponentDecimalReal**

```
New: ExponentDecimalReal = ({ Digit }- | ({ Digit }-, '.', { Digit }-) | ('.', { Digit }-)), ((E) | (E)), ['-'], { Digit }- ;

Old: ExponentDecimalReal = ({ Digit | '.' }- | DecimalInteger), ((E) | (E)), (DigitString | DecimalInteger) ;
```

- **HexDigit**

```
New: HexDigit = Digit
              | HexLetter
              ;

Old: HexDigit = '0'
              | '1'
              | '2'
              | '3'
              | '4'
              | '5'
              | '6'
              | '7'
              | '8'
              | '9'
              | (A)
              | (B)
              | (C)
              | (D)
              | (E)
              | (F)
              ;
```

- **HexInteger**

```
New: HexInteger = ('0',X), { HexDigit }- ;

Old: HexInteger = ('0',X), HexString ;
```

- **HexLetter**

```
New: HexLetter = (A)
               | (B)
               | (C)
               | (D)
               | (E)
               | (F)
               ;

Old: n/a
```

- **HexString**

```
New: n/a

Old: HexString = { HexDigit }- ;
```

- **ListComprehension**

```
New: ListComprehension = '[', [SP], FilterExpression, [[SP], '|', [SP], Expression], [SP], ']' ;

Old: ListComprehension = '[', FilterExpression, [[SP], '|', Expression], ']' ;
```

- **ListLiteral**

```
New: ListLiteral = '[', [SP], [Expression, [SP], { ',', [SP], Expression, [SP] }], ']' ;

Old: n/a
```

- **Literal**

```
New: Literal = NumberLiteral
             | StringLiteral
             | BooleanLiteral
             | (N,U,L,L)
             | MapLiteral
             | ListLiteral
             ;
        
Old: n/a
```

- **NonZeroDigit**

```
New: NonZeroDigit = NonZeroOctDigit
                  | '8'
                  | '9'
                  ;

Old: n/a
```

- **NonZeroOctDigit**

```
New: NonZeroOctDigit = '1'
                     | '2'
                     | '3'
                     | '4'
                     | '5'
                     | '6'
                     | '7'
                     ;

Old: n/a
```

- **OctalInteger**

```
New: OctalInteger = ZeroDigit, { OctDigit }- ;

Old: OctalInteger = '0', OctalString ;
```

- **OctalString**

```
New: n/a

Old: OctalString = { OctDigit }- ;
```

- **OctDigit**

```
New: OctDigit = ZeroDigit
              | NonZeroOctDigit
              ;

Old: OctDigit = '0'
              | '1'
              | '2'
              | '3'
              | '4'
              | '5'
              | '6'
              | '7'
              ;
```

- **RegularDecimalReal**

```
New: RegularDecimalReal = { Digit }, '.', { Digit }- ;

Old: RegularDecimalReal = ({ Digit } | DecimalInteger), '.', (DigitString | DecimalInteger) ;
```

- **Return**

```
New: Return = (R,E,T,U,R,N), [[SP], (D,I,S,T,I,N,C,T)], SP, ReturnBody ;

Old: Return = ((R,E,T,U,R,N), SP, (D,I,S,T,I,N,C,T), SP, ReturnBody)
            | ((R,E,T,U,R,N), SP, ReturnBody)
            ;
```

- **VersionNumber** (Legacy)

```
New: VersionNumber = RegularDecimalReal ;

Old: VersionNumber = DecimalInteger, '.', DecimalInteger ;
```

- **With**

```
New: With = (W,I,T,H), [[SP], (D,I,S,T,I,N,C,T)], SP, ReturnBody, [[SP], Where] ;


Old: With = ((W,I,T,H), (D,I,S,T,I,N,C,T), SP, ReturnBody, [Where])
          | ((W,I,T,H), SP, ReturnBody, [Where])
          ;
```

- **ZeroDigit**

```
New: ZeroDigit = '0' ;


Old: n/a
```

## Version 1.2.3

Release Date: 15.12.2016 - Grammar as of 14.12.2016

### Grammar changes

- **Expression9**

```
New: Expression9 = { (N,O,T), [SP] }, Expression8 ;

Old: Expression9 = { SP, (N,O,T), SP }, Expression8 ;
```

- **LiteralIds** (Legacy)

```
New: LiteralIds = IntegerLiteral, { [SP], ',', [SP], IntegerLiteral } ;

Old: LiteralIds = { [SP], ',', [SP] } ;
```

- **NodeLookup** (Legacy)

```
New: NodeLookup = (N,O,D,E), [SP], (IdentifiedIndexLookup | IndexQuery | IdLookup) ;

Old: NodeLookup = (N,O,D,E), (IdentifiedIndexLookup | IndexQuery | IdLookup) ;
```

- **PeriodicCommitHint** (Legacy)

```
New: PeriodicCommitHint = (U,S,I,N,G), SP, (P,E,R,I,O,D,I,C), SP, (C,O,M,M,I,T), [SP, IntegerLiteral] ;

Old: PeriodicCommitHint = (U,S,I,N,G), SP, (P,E,R,I,O,D,I,C), SP, (C,O,M,M,I,T), [SP] ;
```

- **SortItem**

```
New: SortItem = Expression, [[SP], ((A,S,C,E,N,D,I,N,G) | (A,S,C) | (D,E,S,C,E,N,D,I,N,G) | (D,E,S,C))] ;

Old: SortItem = (Expression, ((D,E,S,C,E,N,D,I,N,G) | (D,E,S,C)))
              | (Expression, [(A,S,C,E,N,D,I,N,G) | (A,S,C)])
              ;
```

## Version 1.2.2

Release Date: 11.11.2016 - Grammar as of 10.11.2016

### Grammar changes

There are no relevant grammar changes available.

### New features

- Support of rebar3.


## Version 1.2.1

Release Date: 03.10.2016 - Grammar as of 02.10.2016

### Grammar changes

- `WS` has been replaced by `[SP]`.

- **Union**

```
New: Union = ((U,N,I,O,N), SP, (A,L,L), [SP], SingleQuery)
           | ((U,N,I,O,N), [SP], SingleQuery)
           ;

Old: Union = ((U,N,I,O,N), SP, (A,L,L), SingleQuery)
           | ((U,N,I,O,N), SingleQuery)
           ;
```

- **Atom**

```
New: Atom = ...
          | ((C,O,U,N,T), [SP], '(', [SP], '*', [SP], ')')
          ...

Old: Atom = ...
          | ((C,O,U,N,T), '(', '*', ')')
          ...
```

- **FunctionInvocation**

```
New: FunctionInvocation = FunctionName, [SP], '(', [SP], [(D,I,S,T,I,N,C,T), [SP]], [Expression, [SP], { ',', [SP], Expression, [SP] }], ')' ;

Old: FunctionInvocation = FunctionName, WS, '(', WS, [(D,I,S,T,I,N,C,T), WS], [Expression, { ',', WS, Expression }, WS], ')' ;
```

- **WS**

```
New: n/a

Old: WS = { whitespace } ;
```

## Version 1.2.0

Release Date: 29.08.2016 - Grammar as of 28.08.2016

### Grammar changes

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

- **RelationshipDetail**

```
New: RelationshipDetail = '[', [Variable], ['?'], [RelationshipTypes], [RangeLiteral], [Properties], ']' ;

Old: RelationshipDetail = '[', [Variable], ['?'], [RelationshipTypes], ['*', RangeLiteral], [Properties], ']' ;
```

- **Properties** (Legacy)

```
New: Properties = MapLiteral | Parameter | LegacyParameter ;

Old: Properties = MapLiteral | Parameter ;
```

- **RangeLiteral**

```
New: RangeLiteral = '*', WS, [IntegerLiteral, WS], ['..', WS, [IntegerLiteral, WS]] ;

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

- **FunctionInvocation**

```
New: FunctionInvocation = FunctionName, WS, '(', WS, [(D,I,S,T,I,N,C,T), WS], [Expression, { ',', WS, Expression }, WS], ')' ;

Old: FunctionInvocation = FunctionName, WS, '(', WS, [D,I,S,T,I,N,C,T], [Expression, { ',', WS, Expression }], WS, ')' ;
```

- **CaseExpression** (Legacy)

```
New: CaseExpression = (((C,A,S,E), { WS, CaseAlternatives }-) | ((C,A,S,E), WS, Expression, { WS, CaseAlternatives }-)), [WS, (E,L,S,E), WS, Expression], WS, (E,N,D) ;

Old: CaseExpression = (((C,A,S,E), { WS, CaseAlternatives }-) | ((C,A,S,E), Expression, { WS, CaseAlternatives }-)), [WS, (E,L,S,E), WS, Expression], WS, (E,N,D) ;
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

### New features

1. Test data generator for generating generic tests.
2. Script for generic stress tests.

## Version 1.1.1

Release Date: 11.07.2016 - Grammar as of 10.07.2016

### Grammar changes

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

## Version 1.1.0

Release Date: 08.07.2016

### Issues

1. Correction in definition of Atom with MapLiteral and Parameter.
2. Test case removed, since multiple statements are not supported (see [openParser #113](https://github.com/opencypher/openCypher/issues/113)).
3. PropertyKeyNameExpression removed from parse tree.

### New features

1. Support of the legacy grammar.

----------

## Version 1.0.0

Release Date: 07.07.2016