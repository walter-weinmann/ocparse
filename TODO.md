ocparse <a href="https://magnum.travis-ci.com/k2informatics/sqlparse"><img src="https://travis-ci.org/K2InformaticsGmbH/sqlparse.svg" alt="Travis-CI"></a>
=======

ToDo List
---------

## Open Issues ##

#### 1. CREATE / DROP RELATIONSHIP PROPERTY EXISTENCE CONSTRAINT ####

#### 2. DELETE ####

#### 3. FOR EACH ####

#### 4. UNWIND ####

#### 5. LOAD CSV ####

#### 6. CREATE ####

#### 7. MATCH ####

#### 8. START ####

#### 9. MERGE ####

#### 10. SET ####

#### 11. REMOVE ####

#### 12. WITH ####

#### 13. RETURN ####

#### 14. Refinement SymbolicName ####

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
    
#### 17. Unary Operators ####
