ocparse <a href="https://magnum.travis-ci.com/k2informatics/sqlparse"><img src="https://travis-ci.org/K2InformaticsGmbH/sqlparse.svg" alt="Travis-CI"></a>
=======

ToDo List
---------

## Open Issues ##

#### 1. Dash -- ####
    
#### 2. Minimising code of ocparse_fold.erl ####
    
#### 3. Refinement SymbolicName ####

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
    
#### 4. Unary Operators ####
    
#### 5. Unicode support for atoms
