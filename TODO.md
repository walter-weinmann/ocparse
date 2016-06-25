ocparse <a href="https://magnum.travis-ci.com/k2informatics/sqlparse"><img src="https://travis-ci.org/K2InformaticsGmbH/sqlparse.svg" alt="Travis-CI"></a>
=======

ToDo List
---------

## Open Issues ##

#### 1. SET ####

#### 2. REMOVE ####

#### 3. WITH ####

#### 4. RETURN ####

#### 5. Refinement SymbolicName ####

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
    
#### 6. Unary Operators ####
    
#### 7. Reduce-Reduce & Reduce-Shift Conflicts ####
    
#### 8. Dash -- ####
    
#### 9. Unicode support for atoms
