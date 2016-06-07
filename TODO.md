ocparse <a href="https://magnum.travis-ci.com/k2informatics/sqlparse"><img src="https://travis-ci.org/K2InformaticsGmbH/sqlparse.svg" alt="Travis-CI"></a>
=======

ToDo List
---------

## Open Issues ##

#### 2. CREATE / DROP UNIQUE CONSTRAINT ####

    Cypher = WS, QueryOptions, Statement, [WS, ';'], WS ;
    
    Statement = Command | ... ;
    
    Command = ... | CreateUniqueConstraint | DropUniqueConstraint | ...;
    
    CreateUniqueConstraint = (C,R,E,A,T,E), WS, UniqueConstraint ;
    
    DropUniqueConstraint = (D,R,O,P), SP, UniqueConstraint ;

#### 3. CREATE / DROP NODE PROPERTY EXISTENCE CONSTRAINT ####

#### 4. DELETE ####

#### 5. FOR EACH ####

#### 6. UNWIND ####

#### 7. LOAD CSV ####

#### 8. CREATE ####

#### 9. MATCH ####

#### 10. START ####

#### 11. MERGE ####

#### 12. SET ####

#### 13. REMOVE ####

#### 14. WITH ####

#### 15. RETURN ####

#### 16. Refinement SymbolicName ####

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

## Closed Issues ##

#### Done at: 02.06.2016 : 1. CREATE / DROP INDEX ####

    Cypher = WS, QueryOptions, Statement, [WS, ';'], WS ;
    
    Statement = Command | ... ;
    
    Command = CreateIndex | DropIndex | ...;
    
    CreateIndex = (C,R,E,A,T,E), SP, Index ;
    
    DropIndex = (D,R,O,P), SP, Index ;
    

