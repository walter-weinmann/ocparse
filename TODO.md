ocparse <a href="https://magnum.travis-ci.com/k2informatics/sqlparse"><img src="https://travis-ci.org/K2InformaticsGmbH/sqlparse.svg" alt="Travis-CI"></a>
=======

ToDo List
---------

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
    
#### 3. CREATE / DROP UNIQUE CONSTRAINT ####

    Cypher = WS, QueryOptions, Statement, [WS, ';'], WS ;
    
    Statement = Command | ... ;
    
    Command = ... | CreateUniqueConstraint | DropUniqueConstraint | ...;
    
    CreateUniqueConstraint = (C,R,E,A,T,E), WS, UniqueConstraint ;
    
    DropUniqueConstraint = (D,R,O,P), SP, UniqueConstraint ;

#### 4. CREATE / DROP NODE PROPERTY EXISTENCE CONSTRAINT ####

#### 5. DELETE ####

#### 6. FOR EACH ####

#### 7. UNWIND ####

#### 8. LOAD CSV ####

#### 9. CREATE ####

#### 10. MATCH ####

#### 11. START ####

#### 12. MERGE ####

#### 13. SET ####

#### 14. REMOVE ####

#### 15. WITH ####

#### 16. RETURN ####

#### Done at: 02.06.2016 : 1. CREATE / DROP INDEX ####

    Cypher = WS, QueryOptions, Statement, [WS, ';'], WS ;
    
    Statement = Command | ... ;
    
    Command = CreateIndex | DropIndex | ...;
    
    CreateIndex = (C,R,E,A,T,E), SP, Index ;
    
    DropIndex = (D,R,O,P), SP, Index ;
    

