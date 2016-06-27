ocparse <a href="https://magnum.travis-ci.com/k2informatics/sqlparse"><img src="https://travis-ci.org/K2InformaticsGmbH/sqlparse.svg" alt="Travis-CI"></a>
=======

ToDo List
---------

## Open Issues ##

#### 1. Dash -- ####
    
#### 2. Refinement SymbolicName ####

    SymbolicName = UnescapedSymbolicName
                 | EscapedSymbolicName
                 ;
    
    UnescapedSymbolicName = IdentifierStart, { IdentifierPart } ;
    
    IdentifierStart = ID_Start
                    | Sc
                    | '_'
                    | '‿'
                    | '⁀'
                    | '⁔'
                    | '︳'
                    | '︴'
                    | '﹍'
                    | '﹎'
                    | '﹏'
                    | '＿'
                    ;
    
    IdentifierPart = ID_Continue
                   | Sc
                   ;
    
    EscapedSymbolicName = { '`', { ANY - ('`') }, '`' }- ;
    
#### 3. Test cases: complex versions ####
    
#### 4. Test cases: based on NEO4j documentation ####
    
#### 5. Test cases: based on book "Graph Databases" ####
    
#### 6. Test cases: based on book "NEO4j in Action" ####
    
#### 7. Test cases: based on book "Learning Cypher" ####
    
#### 8. Unary Operators ####

#### 9. Minimising code of ocparse_fold.erl ####

#### 10. Finishing ocparse wiki ####
    
#### 11. Unicode support for atoms
    

