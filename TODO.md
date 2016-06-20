ocparse <a href="https://magnum.travis-ci.com/k2informatics/sqlparse"><img src="https://travis-ci.org/K2InformaticsGmbH/sqlparse.svg" alt="Travis-CI"></a>
=======

ToDo List
---------

## Open Issues ##

#### 1. DELETE ####

#### 2. FOR EACH ####

#### 3. UNWIND ####

#### 4. LOAD CSV ####

#### 5. CREATE ####

#### 6. MATCH ####

#### 7. START ####

#### 8. MERGE ####

#### 9. SET ####

#### 10. REMOVE ####

#### 11. WITH ####

#### 12. RETURN ####

#### 13. Refinement SymbolicName ####

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
    
#### 14. Unary Operators ####
