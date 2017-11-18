%% -----------------------------------------------------------------------------
%%
%% ocparse.yrl: opencypher - parser definition.
%%
%% Copyright (c) 2017 Walter Weinmann.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -----------------------------------------------------------------------------

%% -*- erlang -*-
Header "%% Copyright (C) Walter Weinmann"
"%% @private"
"%% @Author Walter Weinmann".

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Nonterminals
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 add_or_subtract_expression
 and_expression
 and_expression_addon_list
 anonymous_pattern_part
 atom
 boolean_literal
 case_alternatives
 case_alternatives_list
 case_expression
 comparison_expression
 create
 cypher
 delete
 double_literal
 explicit_procedure_invocation
 expression
 expression_commalist
 filter_expression
 function_invocation
 function_name
 integer_literal
 id_in_coll
 implicit_procedure_invocation
 in_query_call
 label_name
 limit
 list_comprehension
 list_literal
 literal
 map_literal
 match
 merge
 merge_action
 merge_action_list
 multiply_divide_modulo_expression
 multiply_divide_modulo_expression_addon_list
 multi_part_query
 namespace
 node_label
 node_label_list
 node_labels
 node_labels_property_lookup_list
 node_pattern
 not_addon_list
 not_expression
 not_expression_addon_list
 number_literal
 or_expression
 order
 parameter
 parenthesized_expression
 partial_comparison_expression
 partial_comparison_expression_addon_list
 pattern
 pattern_comprehension
 pattern_element
 pattern_element_chain
 pattern_element_chain_list
 pattern_part
 pattern_part_commalist
 power_of_expression
 power_of_expression_addon_list
 procedure_name
 procedure_result_field
 properties
 property_expression
 property_key_name
 property_key_name_expression
 property_key_name_expression_commalist
 property_lookup
 property_lookup_list
 property_or_labels_expression
 property_or_labels_expression_addon
 property_or_labels_expression_addon_list
 query
 range_literal
 read_only_end
 read_part
 read_part_updating_part_with_list
 read_update_end
 reading_clause
 reading_clause_list
 regular_query
 rel_type_name
 rel_type_name_verticalbarlist
 relationship_detail
 relationship_pattern
 relationship_types
 relationships_pattern
 remove
 remove_item
 remove_item_commalist
 reserved_word
 return
 return_body
 return_item
 return_item_commalist
 return_items
 schema_name
 set
 set_item
 set_item_commalist
 single_part_query
 single_query
 skip
 sort_item
 sort_item_commalist
 stand_alone_call
 statement
 string_list_null_operator_expression
 symbolic_name
 unary_add_or_subtract
 unary_add_or_subtract_expression
 unary_add_or_subtract_expression_addon_list
 unary_add_or_subtract_list
 union
 union_list
 unwind
 updating_clause
 updating_clause_list
 updating_end
 updating_part
 updating_start_clause
 variable
 where
 with
 xor_expression
 xor_expression_addon_list
 yield_item
 yield_item_commalist
 yield_items
 .

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Terminals
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 ADD
 ALL
 AND
 ANY
 AS
 ASC
 ASCENDING
 BY
 CALL
 CASE
 CONSTRAINT
 CONTAINS
 COUNT
 CREATE
 DECIMAL_INTEGER
 DELETE
 DESC
 DESCENDING
 DETACH
 DISTINCT
 DO
 DROP
 ELSE
 END
 ENDS
 ESCAPED_SYMBOLIC_NAME
 EXPONENT_DECIMAL_REAL
 EXISTS
 EXTRACT
 FALSE
 FILTER
 FOR
 HEX_INTEGER
 HEX_LETTER
 IN
 IS
 LIMIT
 MANDATORY
 MATCH
 MERGE
 NONE
 NOT
 NULL
 OCTAL_INTEGER
 OF
 ON
 OPTIONAL
 OR
 ORDER
 REGULAR_DECIMAL_REAL
 REMOVE
 REQUIRE
 RETURN
 SCALAR
 SET
 SINGLE
 SKIP
 STARTS
 STRING_LITERAL
 THEN
 TRUE
 UNESCAPED_SYMBOLIC_NAME
 UNION
 UNIQUE
 UNWIND
 WHEN
 WHERE
 WITH
 XOR
 YIELD
 '='
 '=~'
 '<'
 '>'
 '<='
 '>='
 '<>'
 '-'
 '+'
 '+='
 '*'
 '/'
 '%'
 ':'
 ','
 ';'
 '^'
 '|'
 '('
 ')'
 '{'
 '}'
 '['
 ']'
 '.'
 '..'
 '$'
.

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Rootsymbol
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 cypher.

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Endsymbol
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 '$end'.

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Operator precedences.
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Nonassoc    100 properties.                                                                     % atom vs. properties / literal vs. properties

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Grammar rules.
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Cypher = [SP], Statement, [[SP], ';'], [SP], EOI ;

cypher -> statement                                                                             : {cypher, '$1', []}.
cypher -> statement ';'                                                                         : {cypher, '$1', ";"}.

%% =============================================================================
%% Helper definitions - test purposes.
%% -----------------------------------------------------------------------------
% cypher -> explicit_procedure_invocation                                                         : '$1'.
%% =============================================================================

%% Statement = Query ;

statement -> query                                                                              : {statement, '$1'}.

%% Query = RegularQuery
%%       | StandaloneCall
%%       ;

query -> regular_query                                                                          : {query, '$1'}.
query -> stand_alone_call                                                                       : {query, '$1'}.

%% RegularQuery = SingleQuery, { [SP], Union } ;

regular_query -> single_query                                                                   : {regularQuery, '$1', []}.
regular_query -> single_query union_list                                                        : {regularQuery, '$1', '$2'}.

%% =============================================================================
%% Helper definitions.
%% -----------------------------------------------------------------------------
union_list ->            union                                                                  :         ['$1'].
union_list -> union_list union                                                                  : '$1' ++ ['$2'].
%% =============================================================================

%% Union = ((U,N,I,O,N), SP, (A,L,L), [SP], SingleQuery)
%%       | ((U,N,I,O,N), [SP], SingleQuery)
%%       ;

union -> UNION     single_query                                                                 : {union, [],    '$2'}.
union -> UNION ALL single_query                                                                 : {union, "all", '$3'}.

%% SingleQuery = SinglePartQuery
%%             | MultiPartQuery
%%             ;

single_query -> single_part_query                                                               : {singleQuery, '$1'}.
single_query -> multi_part_query                                                                : {singleQuery, '$1'}.

%% SinglePartQuery = ReadOnlyEnd
%%                 | ReadUpdateEnd
%%                 | UpdatingEnd
%%                 ;

single_part_query -> read_only_end                                                              : {singlePartQuery, '$1'}.
single_part_query -> read_update_end                                                            : {singlePartQuery, '$1'}.
single_part_query -> updating_end                                                               : {singlePartQuery, '$1'}.

%% ReadOnlyEnd = ReadPart, Return ;

read_only_end ->           return                                                               : {readOnlyEnd, {readPart, []}, '$1'}.
read_only_end -> read_part return                                                               : {readOnlyEnd, '$1',           '$2'}.

%% ReadUpdateEnd = ReadingClause, { [SP], ReadingClause }, { [SP], UpdatingClause }-, [[SP], Return] ;

read_update_end -> reading_clause_list updating_clause_list                                     : {readUpdateEnd, '$1', '$2', []}.
read_update_end -> reading_clause_list updating_clause_list return                              : {readUpdateEnd, '$1', '$2', '$3'}.

%% =============================================================================
%% Helper definitions.
%% -----------------------------------------------------------------------------
reading_clause_list ->                     reading_clause                                       :         ['$1'].
reading_clause_list -> reading_clause_list reading_clause                                       : '$1' ++ ['$2'].

updating_clause_list ->                      updating_clause                                    :         ['$1'].
updating_clause_list -> updating_clause_list updating_clause                                    : '$1' ++ ['$2'].
%% -----------------------------------------------------------------------------

%% UpdatingEnd = UpdatingStartClause, { [SP], UpdatingClause }, [[SP], Return] ;

updating_end -> updating_start_clause                                                           : {updatingEnd, '$1', [],   []}.
updating_end -> updating_start_clause                      return                               : {updatingEnd, '$1', [],   '$2'}.
updating_end -> updating_start_clause updating_clause_list                                      : {updatingEnd, '$1', '$2', []}.
updating_end -> updating_start_clause updating_clause_list return                               : {updatingEnd, '$1', '$2', '$3'}.

%% MultiPartQuery = (ReadPart | (UpdatingStartClause, [SP], UpdatingPart)), With, [SP], { ReadPart, UpdatingPart, With, [SP] }, SinglePartQuery ;

multi_part_query ->                                     with                                   single_part_query
                                                                                                : {multiPartQuery, {readPart, []}, [],                 '$1', [],   '$2'}.
multi_part_query ->                                     with read_part_updating_part_with_list single_part_query
                                                                                                : {multiPartQuery, {readPart, []}, [],                 '$1', '$2', '$3'}.
multi_part_query -> read_part                           with                                   single_part_query
                                                                                                : {multiPartQuery, '$1',           [],                 '$2', [],   '$3'}.
multi_part_query -> read_part                           with read_part_updating_part_with_list single_part_query
                                                                                                : {multiPartQuery, '$1',           [],                 '$2', '$3', '$4'}.
multi_part_query -> updating_start_clause               with                                   single_part_query
                                                                                                : {multiPartQuery, '$1',           {updatingPart, []}, '$2', [],   '$3'}.
multi_part_query -> updating_start_clause               with read_part_updating_part_with_list single_part_query
                                                                                                : {multiPartQuery, '$1',           {updatingPart, []}, '$2', '$3', '$4'}.
multi_part_query -> updating_start_clause updating_part with                                   single_part_query
                                                                                                : {multiPartQuery, '$1',           '$2',               '$3', [],   '$4'}.
multi_part_query -> updating_start_clause updating_part with read_part_updating_part_with_list single_part_query
                                                                                                : {multiPartQuery, '$1',           '$2',               '$3', '$4', '$5'}.

%% =============================================================================
%% Helper definitions.
%% -----------------------------------------------------------------------------
read_part_updating_part_with_list ->                                                           with
                                                                                                :         [{{readPart, []}, {updatingPart, []}, '$1'}].
read_part_updating_part_with_list ->                                             updating_part with
                                                                                                :         [{{readPart, []}, '$1',               '$2'}].
read_part_updating_part_with_list ->                                   read_part               with
                                                                                                :         [{'$1',           {updatingPart, []}, '$2'}].
read_part_updating_part_with_list ->                                   read_part updating_part with
                                                                                                :         [{'$1',           '$2',               '$3'}].
read_part_updating_part_with_list -> read_part_updating_part_with_list                         with
                                                                                                : '$1' ++ [{{readPart, []}, {updatingPart, []}, '$2'}].
read_part_updating_part_with_list -> read_part_updating_part_with_list           updating_part with
                                                                                                : '$1' ++ [{{readPart, []}, '$2',               '$3'}].
read_part_updating_part_with_list -> read_part_updating_part_with_list read_part               with
                                                                                                : '$1' ++ [{'$2',           {updatingPart, []}, '$3'}].
read_part_updating_part_with_list -> read_part_updating_part_with_list read_part updating_part with
                                                                                                : '$1' ++ [{'$2',           '$3',               '$4'}].
%% -----------------------------------------------------------------------------

% ReadPart = { ReadingClause, [SP] } ;

read_part -> reading_clause_list                                                                : {readPart, '$1'}.

%% UpdatingPart = { UpdatingClause, [SP] } ;

updating_part -> updating_clause_list                                                           : {updatingPart, '$1'}.

%% UpdatingStartClause = Create
%%                     | Merge
%%                     ;

updating_start_clause -> create                                                                 : {updatingStartClause, '$1'}.
updating_start_clause -> merge                                                                  : {updatingStartClause, '$1'}.

%% UpdatingClause = Create
%%                | Merge
%%                | Delete
%%                | Set
%%                | Remove
%%                ;

updating_clause -> updating_start_clause                                                        : {updatingClause, '$1'}.
updating_clause -> delete                                                                       : {updatingClause, '$1'}.
updating_clause -> set                                                                          : {updatingClause, '$1'}.
updating_clause -> remove                                                                       : {updatingClause, '$1'}.

%% ReadingClause = Match
%%               | Unwind
%%               | InQueryCall
%%               ;

reading_clause -> match                                                                         : {readingClause, '$1'}.
reading_clause -> unwind                                                                        : {readingClause, '$1'}.
reading_clause -> in_query_call                                                                 : {readingClause, '$1'}.

%% Match = [(O,P,T,I,O,N,A,L), SP], (M,A,T,C,H), [SP], Pattern, [[SP], Where] ;

match ->          MATCH pattern                                                                 : {match, [],         '$2', []}.
match ->          MATCH pattern where                                                           : {match, [],         '$2', '$3'}.
match -> OPTIONAL MATCH pattern                                                                 : {match, "optional", '$3', []}.
match -> OPTIONAL MATCH pattern where                                                           : {match, "optional", '$3', '$4'}.

%% Unwind = (U,N,W,I,N,D), [SP], Expression, SP, (A,S), SP, Variable ;

unwind -> UNWIND expression AS variable                                                         : {unwind, '$2', '$4'}.

%% Merge = (M,E,R,G,E), [SP], PatternPart, { SP, MergeAction } ;

merge -> MERGE pattern_part                                                                     : {merge, '$2', []}.
merge -> MERGE pattern_part merge_action_list                                                   : {merge, '$2', '$3'}.

%% =============================================================================
%% Helper definitions.
%% -----------------------------------------------------------------------------
merge_action_list ->                   merge_action                                             :         ['$1'].
merge_action_list -> merge_action_list merge_action                                             : '$1' ++ ['$2'].
%% -----------------------------------------------------------------------------

%% MergeAction = ((O,N), SP, (M,A,T,C,H), SP, Set)
%%             | ((O,N), SP, (C,R,E,A,T,E), SP, Set)
%%             ;

merge_action -> ON MATCH  set                                                                   : {mergeAction, "match",  '$3'}.
merge_action -> ON CREATE set                                                                   : {mergeAction, "create", '$3'}.

%% Create = (C,R,E,A,T,E), [SP], Pattern ;

create -> CREATE pattern                                                                        : {create, '$2'}.

%% Set = (S,E,T), [SP], SetItem, { ',', SetItem } ;

set -> SET set_item_commalist                                                                   : {set, '$2'}.

%% =============================================================================
%% Helper definitions.
%% -----------------------------------------------------------------------------
set_item_commalist -> set_item                                                                  : ['$1'].
set_item_commalist -> set_item ',' set_item_commalist                                           : ['$1' | '$3'].
%% -----------------------------------------------------------------------------

%% SetItem = (PropertyExpression, [SP], '=', [SP], Expression)
%%         | (Variable, [SP], '=', [SP], Expression)
%%         | (Variable, [SP], '+=', [SP], Expression)
%%         | (Variable, [SP], NodeLabels)
%%         ;

set_item -> property_expression '='  expression                                                 : {setItem, '$1', "=",  '$3'}.
set_item -> variable            '='  expression                                                 : {setItem, '$1', "=",  '$3'}.
set_item -> variable            '+=' expression                                                 : {setItem, '$1', "+=", '$3'}.
set_item -> variable                 node_labels                                                : {setItem, '$1', [],   '$2'}.

%% Delete = [(D,E,T,A,C,H), SP], (D,E,L,E,T,E), [SP], Expression, { [SP], ',', [SP], Expression } ;

delete ->        DELETE expression_commalist                                                    : {delete, [],       '$2'}.
delete -> DETACH DELETE expression_commalist                                                    : {delete, "detach", '$3'}.

%% =============================================================================
%% Helper definitions.
%% -----------------------------------------------------------------------------
expression_commalist -> expression                                                              : ['$1'].
expression_commalist -> expression ',' expression_commalist                                     : ['$1' | '$3'].
%% -----------------------------------------------------------------------------

%% Remove = (R,E,M,O,V,E), SP, RemoveItem, { [SP], ',', [SP], RemoveItem } ;

remove -> REMOVE remove_item_commalist                                                          : {remove, '$2'}.

%% =============================================================================
%% Helper definitions.
%% -----------------------------------------------------------------------------
remove_item_commalist -> remove_item                                                            : ['$1'].
remove_item_commalist -> remove_item ',' remove_item_commalist                                  : ['$1' | '$3'].
%% -----------------------------------------------------------------------------

%% RemoveItem = (Variable, NodeLabels)
%%            | PropertyExpression
%%            ;

remove_item -> variable            node_labels                                                  : {removeItem, '$1', '$2'}.
remove_item -> property_expression                                                              : {removeItem, '$1'}.

%% InQueryCall = (C,A,L,L), SP, ExplicitProcedureInvocation, [[SP], (Y,I,E,L,D), SP, YieldItems] ;

in_query_call -> CALL explicit_procedure_invocation                                             : {inQueryCall, '$2', []}.
in_query_call -> CALL explicit_procedure_invocation YIELD yield_items                           : {inQueryCall, '$2', '$4'}.

%% StandaloneCall = (C,A,L,L), SP, (ExplicitProcedureInvocation | ImplicitProcedureInvocation), [SP, (Y,I,E,L,D), SP, YieldItems] ;

stand_alone_call -> CALL explicit_procedure_invocation                                          : {standaloneCall, '$2', []}.
stand_alone_call -> CALL explicit_procedure_invocation YIELD yield_items                        : {standaloneCall, '$2', '$4'}.
stand_alone_call -> CALL implicit_procedure_invocation                                          : {standaloneCall, '$2', []}.
stand_alone_call -> CALL implicit_procedure_invocation YIELD yield_items                        : {standaloneCall, '$2', '$4'}.

%% YieldItems = (YieldItem, { [SP], ',', [SP], YieldItem })
%%            | '-'
%%            ;

yield_items -> yield_item_commalist                                                             : {yieldItems, '$1'}.
yield_items -> '-'                                                                              : {yieldItems, "-"}.

%% =============================================================================
%% Helper definitions.
%% -----------------------------------------------------------------------------
yield_item_commalist -> yield_item                                                              : ['$1'].
yield_item_commalist -> yield_item ',' yield_item_commalist                                     : ['$1' | '$3'].
%% -----------------------------------------------------------------------------

%% YieldItem = [ProcedureResultField, SP, (A,S), SP], Variable ;

yield_item ->                           variable                                                : {yieldItem, [],   '$1'}.
yield_item -> procedure_result_field AS variable                                                : {yieldItem, '$1', '$3'}.

%% With = (W,I,T,H), [[SP], (D,I,S,T,I,N,C,T)], SP, ReturnBody, [[SP], Where] ;

with -> WITH          return_body                                                               : {with, [],         '$2', []}.
with -> WITH          return_body where                                                         : {with, [],         '$2', '$3'}.
with -> WITH DISTINCT return_body                                                               : {with, "distinct", '$3', []}.
with -> WITH DISTINCT return_body where                                                         : {with, "distinct", '$3', '$4'}.

%% Return = (R,E,T,U,R,N), [[SP], (D,I,S,T,I,N,C,T)], SP, ReturnBody ;

return -> RETURN          return_body                                                           : {return, [],         '$2'}.
return -> RETURN DISTINCT return_body                                                           : {return, "distinct", '$3'}.

% ReturnBody = ReturnItems, [SP, Order], [SP, Skip], [SP, Limit] ;

return_body -> return_items                                                                     : {returnBody, '$1', [],   [],   []}.
return_body -> return_items            limit                                                    : {returnBody, '$1', [],   [],   '$2'}.
return_body -> return_items       skip                                                          : {returnBody, '$1', [],   '$2', []}.
return_body -> return_items       skip limit                                                    : {returnBody, '$1', [],   '$2', '$3'}.
return_body -> return_items order                                                               : {returnBody, '$1', '$2', [],   []}.
return_body -> return_items order      limit                                                    : {returnBody, '$1', '$2', [],   '$3'}.
return_body -> return_items order skip                                                          : {returnBody, '$1', '$2', '$3', []}.
return_body -> return_items order skip limit                                                    : {returnBody, '$1', '$2', '$3', '$4'}.

%% ReturnItems = ('*', { [SP], ',', [SP], ReturnItem })
%%             | (ReturnItem, { [SP], ',', [SP], ReturnItem })
%%             ;

return_items -> '*'                                                                             : {returnItems, "*", [],  []}.
return_items -> '*' ',' return_item_commalist                                                   : {returnItems, "*", ",", '$3'}.
return_items ->         return_item_commalist                                                   : {returnItems, [],  [],  '$1'}.

%% =============================================================================
%% Helper definitions.
%% -----------------------------------------------------------------------------
return_item_commalist -> return_item                                                            : ['$1'].
return_item_commalist -> return_item ',' return_item_commalist                                  : ['$1' | '$3'].
%% -----------------------------------------------------------------------------

%% ReturnItem = (Expression, SP, (A,S), SP, Variable)
%%            | Expression
%%            ;

return_item -> expression AS variable                                                           : {returnItem, '$1', '$3'}.
return_item -> expression                                                                       : {returnItem, '$1', []}.

%% Order = (O,R,D,E,R), SP, (B,Y), SP, SortItem, { ',', [SP], SortItem } ;

order -> ORDER BY sort_item_commalist                                                           : {order, '$3'}.

%% =============================================================================
%% Helper definitions.
%% -----------------------------------------------------------------------------
sort_item_commalist -> sort_item                                                                : ['$1'].
sort_item_commalist -> sort_item ',' sort_item_commalist                                        : ['$1' | '$3'].
%% -----------------------------------------------------------------------------

%% Skip = (S,K,I,P), SP, Expression ;

skip -> SKIP expression                                                                         : {skip, '$2'}.

%% Limit = (L,I,M,I,T), SP, Expression ;

limit -> LIMIT expression                                                                       : {limit, '$2'}.

%% SortItem = Expression, [[SP], ((A,S,C,E,N,D,I,N,G) | (A,S,C) | (D,E,S,C,E,N,D,I,N,G) | (D,E,S,C))] ;

sort_item -> expression                                                                         : {sortItem, '$1', []}.
sort_item -> expression ASCENDING                                                               : {sortItem, '$1', "ascending"}.
sort_item -> expression ASC                                                                     : {sortItem, '$1', "asc"}.
sort_item -> expression DESCENDING                                                              : {sortItem, '$1', "descending"}.
sort_item -> expression DESC                                                                    : {sortItem, '$1', "desc"}.

%% Where = (W,H,E,R,E), SP, Expression ;

where -> WHERE expression                                                                       : {where, '$2'}.

%% Pattern = PatternPart, { [SP], ',', [SP], PatternPart } ;

pattern -> pattern_part_commalist                                                               : {pattern, '$1'}.

%% =============================================================================
%% Helper definitions.
%% -----------------------------------------------------------------------------
pattern_part_commalist -> pattern_part                                                          : ['$1'].
pattern_part_commalist -> pattern_part ',' pattern_part_commalist                               : ['$1' | '$3'].
%% =============================================================================

%% PatternPart = (Variable, [SP], '=', [SP], AnonymousPatternPart)
%%             | AnonymousPatternPart
%%             ;

pattern_part -> variable '=' anonymous_pattern_part                                             : {patternPart, '$1', '$3'}.
pattern_part ->              anonymous_pattern_part                                             : {patternPart, [],   '$1'}.

%% AnonymousPatternPart = PatternElement ;

anonymous_pattern_part -> pattern_element                                                       : {anonymousPatternPart, '$1'}.

%% PatternElement = (NodePattern, { [SP], PatternElementChain })
%%                | ('(', PatternElement, ')')
%%                ;

pattern_element -> node_pattern                                                                 : {patternElement, '$1', []}.
pattern_element -> node_pattern pattern_element_chain_list                                      : {patternElement, '$1', '$2'}.
pattern_element -> '(' pattern_element ')'                                                      : {patternElement, '$2', "("}.

%% =============================================================================
%% Helper definitions.
%% -----------------------------------------------------------------------------
pattern_element_chain_list ->                            pattern_element_chain                  :         ['$1'].
pattern_element_chain_list -> pattern_element_chain_list pattern_element_chain                  : '$1' ++ ['$2'].
%% =============================================================================

%% NodePattern = '(', [SP], [Variable, [SP]], [NodeLabels, [SP]], [Properties, [SP]], ')' ;

node_pattern -> '('                                 ')'                                         : {nodePattern, [],   [],   []}.
node_pattern -> '('                      properties ')'                                         : {nodePattern, [],   [],   '$2'}.
node_pattern -> '('          node_labels            ')'                                         : {nodePattern, [],   '$2', []}.
node_pattern -> '('          node_labels properties ')'                                         : {nodePattern, [],   '$2', '$3'}.
node_pattern -> '(' variable                        ')'                                         : {nodePattern, '$2', [],   []}.
node_pattern -> '(' variable             properties ')'                                         : {nodePattern, '$2', [],   '$3'}.
node_pattern -> '(' variable node_labels            ')'                                         : {nodePattern, '$2', '$3', []}.
node_pattern -> '(' variable node_labels properties ')'                                         : {nodePattern, '$2', '$3', '$4'}.

%% PatternElementChain = RelationshipPattern, [SP], NodePattern ;

pattern_element_chain -> relationship_pattern node_pattern                                      : {patternElementChain, '$1', '$2'}.

%% RelationshipPattern = (LeftArrowHead, [SP], Dash, [SP], [RelationshipDetail], [SP], Dash, [SP], RightArrowHead)
%%                     | (LeftArrowHead, [SP], Dash, [SP], [RelationshipDetail], [SP], Dash)
%%                     | (Dash, [SP], [RelationshipDetail], [SP], Dash, [SP], RightArrowHead)
%%                     | (Dash, [SP], [RelationshipDetail], [SP], Dash)
%%                     ;

relationship_pattern ->     '-'                     '-'                                         : {relationshipPattern, "--",  [],   []}.
relationship_pattern ->     '-'                     '-' '>'                                     : {relationshipPattern, "-->", [],   []}.
relationship_pattern ->     '-' relationship_detail '-'                                         : {relationshipPattern, "-",   '$2', "-"}.
relationship_pattern ->     '-' relationship_detail '-' '>'                                     : {relationshipPattern, "-",   '$2', "->"}.
relationship_pattern -> '<' '-'                     '-'                                         : {relationshipPattern, "<--", [],   []}.
relationship_pattern -> '<' '-'                     '-' '>'                                     : {relationshipPattern, "<-->",[],   []}.
relationship_pattern -> '<' '-' relationship_detail '-'                                         : {relationshipPattern, "<-",  '$3', "-"}.
relationship_pattern -> '<' '-' relationship_detail '-' '>'                                     : {relationshipPattern, "<-",  '$3', "->"}.

%% RelationshipDetail = '[', [SP], [Variable, [SP]], [RelationshipTypes, [SP]], [RangeLiteral], [Properties, [SP]], ']' ;

relationship_detail -> '['                                                       ']'            : {relationshipDetail, [],   [],   [],   []}.
relationship_detail -> '['                                            properties ']'            : {relationshipDetail, [],   [],   [],   '$2'}.
relationship_detail -> '['                              range_literal            ']'            : {relationshipDetail, [],   [],   '$2', []}.
relationship_detail -> '['                              range_literal properties ']'            : {relationshipDetail, [],   [],   '$2', '$3'}.
relationship_detail -> '['           relationship_types                          ']'            : {relationshipDetail, [],   '$2', [],   []}.
relationship_detail -> '['           relationship_types               properties ']'            : {relationshipDetail, [],   '$2', [],   '$3'}.
relationship_detail -> '['           relationship_types range_literal            ']'            : {relationshipDetail, [],   '$2', '$3', []}.
relationship_detail -> '['           relationship_types range_literal properties ']'            : {relationshipDetail, [],   '$2', '$3', '$4'}.
relationship_detail -> '[' variable                                              ']'            : {relationshipDetail, '$2', [],   [],   []}.
relationship_detail -> '[' variable                                   properties ']'            : {relationshipDetail, '$2', [],   [],   '$3'}.
relationship_detail -> '[' variable                     range_literal            ']'            : {relationshipDetail, '$2', [],   '$3', []}.
relationship_detail -> '[' variable                     range_literal properties ']'            : {relationshipDetail, '$2', [],   '$3', '$4'}.
relationship_detail -> '[' variable  relationship_types                          ']'            : {relationshipDetail, '$2', '$3', [],   []}.
relationship_detail -> '[' variable  relationship_types               properties ']'            : {relationshipDetail, '$2', '$3', [],   '$4'}.
relationship_detail -> '[' variable  relationship_types range_literal            ']'            : {relationshipDetail, '$2', '$3', '$4', []}.
relationship_detail -> '[' variable  relationship_types range_literal properties ']'            : {relationshipDetail, '$2', '$3', '$4', '$5'}.

%% Properties = MapLiteral
%%            | Parameter
%%            ;

properties -> map_literal                                                                       : {properties, '$1'}.
properties -> parameter                                                                         : {properties, '$1'}.

%% RelationshipTypes = ':', [SP], RelTypeName, { [SP], '|', [':'], [SP], RelTypeName } ;

relationship_types -> ':' rel_type_name                                                         : {relationshipTypes, '$2', []}.
relationship_types -> ':' rel_type_name rel_type_name_verticalbarlist                           : {relationshipTypes, '$2', '$3'}.

%% =============================================================================
%% Helper definitions.
%% -----------------------------------------------------------------------------
rel_type_name_verticalbarlist ->                               '|'     rel_type_name            : [{'$2', []}].
rel_type_name_verticalbarlist ->                               '|' ':' rel_type_name            : [{'$3', ":"}].
rel_type_name_verticalbarlist -> rel_type_name_verticalbarlist '|'     rel_type_name            : '$1' ++ [{'$3', []}].
rel_type_name_verticalbarlist -> rel_type_name_verticalbarlist '|' ':' rel_type_name            : '$1' ++ [{'$4', ":"}].
%% =============================================================================

%% NodeLabels = NodeLabel, { [SP], NodeLabel } ;

node_labels -> node_label_list                                                                  : {nodeLabels, '$1'}.

%% =============================================================================
%% Helper definitions.
%% -----------------------------------------------------------------------------
node_label_list ->                 node_label                                                   :         ['$1'].
node_label_list -> node_label_list node_label                                                   : '$1' ++ ['$2'].
%% =============================================================================

%% NodeLabel = ':', [SP], LabelName ;

node_label -> ':' label_name                                                                    : {nodeLabel, '$2'}.

%% RangeLiteral = '*', [SP], [IntegerLiteral, [SP]], ['..', [SP], [IntegerLiteral, [SP]]] ;

range_literal -> '*'                                                                            : {rangeLiteral, [],   [],   []}.
range_literal -> '*'                 '..'                                                       : {rangeLiteral, [],   "..", []}.
range_literal -> '*'                 '..' integer_literal                                       : {rangeLiteral, [],   "..", '$3'}.
range_literal -> '*' integer_literal                                                            : {rangeLiteral, '$2', [],   []}.
range_literal -> '*' integer_literal '..'                                                       : {rangeLiteral, '$2', "..", []}.
range_literal -> '*' integer_literal '..' integer_literal                                       : {rangeLiteral, '$2', "..", '$4'}.

%% LabelName = SchemaName ;

label_name -> schema_name                                                                       : {labelName, '$1'}.

%% RelTypeName = SchemaName ;

rel_type_name -> schema_name                                                                    : {relTypeName, '$1'}.

%% Expression = OrExpression ;

expression -> or_expression                                                                     : {expression, '$1'}.

%% OrExpression = XorExpression, { SP, (O,R), SP, XorExpression } ;

or_expression -> xor_expression                                                                 : {orExpression, '$1', []}.
or_expression -> xor_expression xor_expression_addon_list                                       : {orExpression, '$1', '$2'}.

%% =============================================================================
%% Helper definitions.
%% -----------------------------------------------------------------------------
xor_expression_addon_list ->                           OR xor_expression                        :         [{"or", '$2'}].
xor_expression_addon_list -> xor_expression_addon_list OR xor_expression                        : '$1' ++ [{"or", '$3'}].
%% =============================================================================

%% XorExpression = AndExpression, { SP, (X,O,R), SP, AndExpression } ;

xor_expression -> and_expression                                                                : {xorExpression, '$1', []}.
xor_expression -> and_expression and_expression_addon_list                                      : {xorExpression, '$1', '$2'}.

%% =============================================================================
%% Helper definitions.
%% -----------------------------------------------------------------------------
and_expression_addon_list ->                           XOR and_expression                       :         [{"xor", '$2'}].
and_expression_addon_list -> and_expression_addon_list XOR and_expression                       : '$1' ++ [{"xor", '$3'}].
%% =============================================================================

%% AndExpression = NotExpression, { SP, (A,N,D), SP, NotExpression } ;

and_expression -> not_expression                                                                : {andExpression, '$1', []}.
and_expression -> not_expression not_expression_addon_list                                      : {andExpression, '$1', '$2'}.

%% =============================================================================
%% Helper definitions.
%% -----------------------------------------------------------------------------
not_expression_addon_list ->                           AND not_expression                       :         [{"and", '$2'}].
not_expression_addon_list -> not_expression_addon_list AND not_expression                       : '$1' ++ [{"and", '$3'}].
%% =============================================================================

%% NotExpression = { (N,O,T), [SP] }, ComparisonExpression ;

not_expression ->                comparison_expression                                          : {notExpression, '$1', []}.
not_expression -> not_addon_list comparison_expression                                          : {notExpression, '$2', '$1'}.

%% =============================================================================
%% Helper definitions.
%% -----------------------------------------------------------------------------
not_addon_list ->                NOT                                                            :         [{"not"}].
not_addon_list -> not_addon_list NOT                                                            : '$1' ++ [{"not"}].
%% =============================================================================

%% ComparisonExpression = AddOrSubtractExpression, { [SP], PartialComparisonExpression } ;

comparison_expression -> add_or_subtract_expression                                             : {comparisonExpression, '$1', []}.
comparison_expression -> add_or_subtract_expression partial_comparison_expression_addon_list    : {comparisonExpression, '$1', '$2'}.

%% =============================================================================
%% Helper definitions.
%% -----------------------------------------------------------------------------
partial_comparison_expression_addon_list ->                                          partial_comparison_expression
                                                                                                :         ['$1'].
partial_comparison_expression_addon_list -> partial_comparison_expression_addon_list partial_comparison_expression
                                                                                                : '$1' ++ ['$2'].
%% =============================================================================

%% AddOrSubtractExpression = MultiplyDivideModuloExpression, { ([SP], '+', [SP], MultiplyDivideModuloExpression) | ([SP], '-', [SP], MultiplyDivideModuloExpression) } ;

add_or_subtract_expression -> multiply_divide_modulo_expression                                 : {addOrSubtractExpression, '$1', []}.
add_or_subtract_expression -> multiply_divide_modulo_expression multiply_divide_modulo_expression_addon_list
                                                                                                : {addOrSubtractExpression, '$1', '$2'}.

%% =============================================================================
%% Helper definitions.
%% -----------------------------------------------------------------------------
multiply_divide_modulo_expression_addon_list ->                                              '+' multiply_divide_modulo_expression
                                                                                                :         [{"+", '$2'}].
multiply_divide_modulo_expression_addon_list ->                                              '-' multiply_divide_modulo_expression
                                                                                                :         [{"-", '$2'}].
multiply_divide_modulo_expression_addon_list -> multiply_divide_modulo_expression_addon_list '+' multiply_divide_modulo_expression
                                                                                                : '$1' ++ [{"+", '$3'}].
multiply_divide_modulo_expression_addon_list -> multiply_divide_modulo_expression_addon_list '-' multiply_divide_modulo_expression
                                                                                                : '$1' ++ [{"-", '$3'}].
%% =============================================================================

%% MultiplyDivideModuloExpression = PowerOfExpression, { ([SP], '*', [SP], PowerOfExpression) | ([SP], '/', [SP], PowerOfExpression) | ([SP], '%', [SP], PowerOfExpression) } ;

multiply_divide_modulo_expression -> power_of_expression                                        : {multiplyDivideModuloExpression, '$1', []}.
multiply_divide_modulo_expression -> power_of_expression power_of_expression_addon_list         : {multiplyDivideModuloExpression, '$1', '$2'}.

%% =============================================================================
%% Helper definitions.
%% -----------------------------------------------------------------------------
power_of_expression_addon_list ->                                '*' power_of_expression        :         [{"*", '$2'}].
power_of_expression_addon_list ->                                '/' power_of_expression        :         [{"/", '$2'}].
power_of_expression_addon_list ->                                '%' power_of_expression        :         [{"%", '$2'}].
power_of_expression_addon_list -> power_of_expression_addon_list '*' power_of_expression        : '$1' ++ [{"*", '$3'}].
power_of_expression_addon_list -> power_of_expression_addon_list '/' power_of_expression        : '$1' ++ [{"/", '$3'}].
power_of_expression_addon_list -> power_of_expression_addon_list '%' power_of_expression        : '$1' ++ [{"%", '$3'}].
%% =============================================================================

%% PowerOfExpression = UnaryAddOrSubtractExpression, { [SP], '^', [SP], UnaryAddOrSubtractExpression } ;

power_of_expression -> unary_add_or_subtract_expression                                         : {powerOfExpression, '$1', []}.
power_of_expression -> unary_add_or_subtract_expression unary_add_or_subtract_expression_addon_list
                                                                                                : {powerOfExpression, '$1', '$2'}.

%% =============================================================================
%% Helper definitions.
%% -----------------------------------------------------------------------------
unary_add_or_subtract_expression_addon_list ->                                             '^' unary_add_or_subtract_expression
                                                                                                :         [{"^", '$2'}].
unary_add_or_subtract_expression_addon_list -> unary_add_or_subtract_expression_addon_list '^' unary_add_or_subtract_expression
                                                                                                : '$1' ++ [{"^", '$3'}].
%% =============================================================================

%% UnaryAddOrSubtractExpression = { ('+' | '-'), [SP] }, StringListNullOperatorExpression ;

unary_add_or_subtract_expression ->                            string_list_null_operator_expression
                                                                                                : {unaryAddOrSubtractExpression, '$1', []}.
unary_add_or_subtract_expression -> unary_add_or_subtract_list string_list_null_operator_expression
                                                                                                : {unaryAddOrSubtractExpression, '$2', '$1'}.

%% =============================================================================
%% Helper definitions.
%% -----------------------------------------------------------------------------
unary_add_or_subtract_list ->                            unary_add_or_subtract                  :         ['$1'].
unary_add_or_subtract_list -> unary_add_or_subtract_list unary_add_or_subtract                  : '$1' ++ ['$2'].

unary_add_or_subtract -> '+'                                                                    : {"+"}.
unary_add_or_subtract -> '-'                                                                    : {"-"}.
%% =============================================================================

%% StringListNullOperatorExpression = PropertyOrLabelsExpression, { ([SP], '[', Expression, ']')
%%                                                                | ([SP], '[', [Expression], '..', [Expression], ']')
%%                                                                | ((([SP], '=~')
%%                                                                   | (SP, (I,N))
%%                                                                   | (SP, (S,T,A,R,T,S), SP, (W,I,T,H))
%%                                                                   | (SP, (E,N,D,S), SP, (W,I,T,H))
%%                                                                   | (SP, (C,O,N,T,A,I,N,S))),
%%                                                                   [SP], PropertyOrLabelsExpression)
%%                                                                | (SP, (I,S), SP, (N,U,L,L))
%%                                                                | (SP, (I,S), SP, (N,O,T), SP, (N,U,L,L)) } ;

string_list_null_operator_expression -> property_or_labels_expression                           : {stringListNullOperatorExpression, '$1', []}.
string_list_null_operator_expression -> property_or_labels_expression property_or_labels_expression_addon_list
                                                                                                : {stringListNullOperatorExpression, '$1', '$2'}.

%% =============================================================================
%% Helper definitions.
%% -----------------------------------------------------------------------------
property_or_labels_expression_addon_list ->                                          property_or_labels_expression_addon
                                                                                                :         ['$1'].
property_or_labels_expression_addon_list -> property_or_labels_expression_addon_list property_or_labels_expression_addon
                                                                                                : '$1' ++ ['$2'].

property_or_labels_expression_addon -> '[' expression                 ']'                       : {"[",           '$2'}.
property_or_labels_expression_addon -> '['            '..'            ']'                       : {"[",           [],   []}.
property_or_labels_expression_addon -> '['            '..' expression ']'                       : {"[",           [],   '$3'}.
property_or_labels_expression_addon -> '[' expression '..'            ']'                       : {"[",           '$2', []}.
property_or_labels_expression_addon -> '[' expression '..' expression ']'                       : {"[",           '$2', '$4'}.
property_or_labels_expression_addon -> '=~'        property_or_labels_expression                : {"=~",          '$2'}.
property_or_labels_expression_addon -> IN          property_or_labels_expression                : {"in",          '$2'}.
property_or_labels_expression_addon -> STARTS WITH property_or_labels_expression                : {"starts with", '$3'}.
property_or_labels_expression_addon -> ENDS   WITH property_or_labels_expression                : {"ends with",   '$3'}.
property_or_labels_expression_addon -> CONTAINS    property_or_labels_expression                : {"contains",    '$2'}.
property_or_labels_expression_addon -> IS     NULL                                              : {"is null"}.
property_or_labels_expression_addon -> IS NOT NULL                                              : {"is not null"}.
%% =============================================================================

%% PropertyOrLabelsExpression = Atom, { [SP], (PropertyLookup | NodeLabels) } ;

property_or_labels_expression -> atom                                                           : {propertyOrLabelsExpression, '$1', []}.
property_or_labels_expression -> atom node_labels_property_lookup_list                          : {propertyOrLabelsExpression, '$1', '$2'}.

%% =============================================================================
%% Helper definitions.
%% -----------------------------------------------------------------------------
node_labels_property_lookup_list ->                                  property_lookup            :         ['$1'].
node_labels_property_lookup_list ->                                  node_labels                :         ['$1'].
node_labels_property_lookup_list -> node_labels_property_lookup_list property_lookup            : '$1' ++ ['$2'].
node_labels_property_lookup_list -> node_labels_property_lookup_list node_labels                : '$1' ++ ['$2'].
%% =============================================================================

%% Atom = Literal
%%      | Parameter
%%      | CaseExpression
%%      | ((C,O,U,N,T), [SP], '(', [SP], '*', [SP], ')')
%%      | ListComprehension
%%      | PatternComprehension
%%      | ((F,I,L,T,E,R), [SP], '(', [SP], FilterExpression, [SP], ')')
%%      | ((E,X,T,R,A,C,T), [SP], '(', [SP], FilterExpression, [SP], [[SP], '|', Expression], ')')
%%      | ((A,L,L), [SP], '(', [SP], FilterExpression, [SP], ')')
%%      | ((A,N,Y), [SP], '(', [SP], FilterExpression, [SP], ')')
%%      | ((N,O,N,E), [SP], '(', [SP], FilterExpression, [SP], ')')
%%      | ((S,I,N,G,L,E), [SP], '(', [SP], FilterExpression, [SP], ')')
%%      | RelationshipsPattern
%%      | ParenthesizedExpression
%%      | FunctionInvocation
%%      | Variable
%%      ;

atom -> literal                                                                                 : {atom, '$1'}.
atom -> parameter                                                                               : {atom, '$1'}.
atom -> case_expression                                                                         : {atom, '$1'}.
atom -> COUNT   '(' '*'               ')'                                                       : {atom, {'count',   []}}.
atom -> list_comprehension                                                                      : {atom, '$1'}.
atom -> pattern_comprehension                                                                   : {atom, '$1'}.
atom -> FILTER  '(' filter_expression ')'                                                       : {atom, {'filter',  '$3'}}.
atom -> EXTRACT '(' filter_expression                ')'                                        : {atom, {'extract', '$3', []}}.
atom -> EXTRACT '(' filter_expression '|' expression ')'                                        : {atom, {'extract', '$3', '$5'}}.
atom -> ALL     '(' filter_expression ')'                                                       : {atom, {'all',     '$3'}}.
atom -> ANY     '(' filter_expression ')'                                                       : {atom, {'any',     '$3'}}.
atom -> NONE    '(' filter_expression ')'                                                       : {atom, {'none',    '$3'}}.
atom -> SINGLE  '(' filter_expression ')'                                                       : {atom, {'single',  '$3'}}.
atom -> relationships_pattern                                                                   : {atom, '$1'}.
atom -> parenthesized_expression                                                                : {atom, '$1'}.
atom -> function_invocation                                                                     : {atom, '$1'}.
atom -> variable                                                                                : {atom, '$1'}.

%% Literal = NumberLiteral
%%         | StringLiteral
%%         | BooleanLiteral
%%         | (N,U,L,L)
%%         | MapLiteral
%%         | ListLiteral
%%         ;

literal -> number_literal                                                                       : {literal, '$1'}.
literal -> STRING_LITERAL                                                                       : {literal, {stringLiteral, unwrap('$1')}}.
literal -> boolean_literal                                                                      : {literal, '$1'}.
literal -> NULL                                                                                 : {literal, {terminal, "null"}}.
literal -> map_literal                                                                          : {literal, '$1'}.
literal -> list_literal                                                                         : {literal, '$1'}.

%% BooleanLiteral = (T,R,U,E)
%%                | (F,A,L,S,E)
%%                ;

boolean_literal -> FALSE                                                                        : {booleanLiteral, {terminal, "false"}}.
boolean_literal -> TRUE                                                                         : {booleanLiteral, {terminal, "true"}}.

%% ListLiteral = '[', [SP], [Expression, [SP], { ',', [SP], Expression, [SP] }], ']' ;

list_literal -> '['                      ']'                                                    : {listLiteral, []}.
list_literal -> '[' expression_commalist ']'                                                    : {listLiteral, '$2'}.

%% PartialComparisonExpression = ('=', [SP], AddOrSubtractExpression)
%%                             | ('<>', [SP], AddOrSubtractExpression)
%%                             | ('<', [SP], AddOrSubtractExpression)
%%                             | ('>', [SP], AddOrSubtractExpression)
%%                             | ('<=', [SP], AddOrSubtractExpression)
%%                             | ('>=', [SP], AddOrSubtractExpression)
%%                             ;

partial_comparison_expression -> '='  add_or_subtract_expression                                : {partialComparisonExpression, '$2', "="}.
partial_comparison_expression -> '<>' add_or_subtract_expression                                : {partialComparisonExpression, '$2', "<>"}.
partial_comparison_expression -> '<'  add_or_subtract_expression                                : {partialComparisonExpression, '$2', "<"}.
partial_comparison_expression -> '>'  add_or_subtract_expression                                : {partialComparisonExpression, '$2', ">"}.
partial_comparison_expression -> '<=' add_or_subtract_expression                                : {partialComparisonExpression, '$2', "<="}.
partial_comparison_expression -> '>=' add_or_subtract_expression                                : {partialComparisonExpression, '$2', ">="}.

%% ParenthesizedExpression = '(', [SP], Expression, [SP], ')' ;

parenthesized_expression -> '(' expression ')'                                                  : {parenthesizedExpression, '$2'}.

%% RelationshipsPattern = NodePattern, { [SP], PatternElementChain }- ;

relationships_pattern -> node_pattern pattern_element_chain_list                                : {relationshipsPattern, '$1', '$2'}.

%% FilterExpression = IdInColl, [[SP], Where] ;

filter_expression -> id_in_coll                                                                 : {filterExpression, '$1', []}.
filter_expression -> id_in_coll where                                                           : {filterExpression, '$1', '$2'}.

%% IdInColl = Variable, SP, (I,N), SP, Expression ;

id_in_coll -> variable IN expression                                                            : {idInColl, '$1', '$3'}.

%% FunctionInvocation = FunctionName, [SP], '(', [SP], [(D,I,S,T,I,N,C,T), [SP]], [Expression, [SP], { ',', [SP], Expression, [SP] }], ')' ;

function_invocation -> function_name '('                               ')'                      : {functionInvocation, '$1', [],         []}.
function_invocation -> function_name '('          expression_commalist ')'                      : {functionInvocation, '$1', [],         '$3'}.
function_invocation -> function_name '(' DISTINCT                      ')'                      : {functionInvocation, '$1', "distinct", []}.
function_invocation -> function_name '(' DISTINCT expression_commalist ')'                      : {functionInvocation, '$1', "distinct", '$4'}.

%% =============================================================================
%% Helper definitions.
%% -----------------------------------------------------------------------------
function_invocation -> COUNT         '('                               ')'                      : {functionInvocation, {functionName, {symbolicName, "count"}}, [],         []}.
function_invocation -> COUNT         '('          expression_commalist ')'                      : {functionInvocation, {functionName, {symbolicName, "count"}}, [],         '$3'}.
function_invocation -> COUNT         '(' DISTINCT                      ')'                      : {functionInvocation, {functionName, {symbolicName, "count"}}, "distinct", []}.
function_invocation -> COUNT         '(' DISTINCT expression_commalist ')'                      : {functionInvocation, {functionName, {symbolicName, "count"}}, "distinct", '$4'}.
% =============================================================================

%% FunctionName = SymbolicName
%%              | (E,X,I,S,T,S)
%%              ;

function_name -> symbolic_name                                                                  : {functionName, '$1'}.
function_name -> EXISTS                                                                         : {functionName, "exists"}.

%% ExplicitProcedureInvocation = ProcedureName, [SP], '(', [SP], [Expression, [SP], { ',', [SP], Expression, [SP] }], ')' ;

explicit_procedure_invocation -> procedure_name '('                      ')'                    : {explicitProcedureInvocation, '$1', []}.
explicit_procedure_invocation -> procedure_name '(' expression_commalist ')'                    : {explicitProcedureInvocation, '$1', '$3'}.

%% ImplicitProcedureInvocation = ProcedureName ;

implicit_procedure_invocation -> procedure_name                                                 : {explicitProcedureInvocation, '$1', []}.

%% ProcedureResultField = SymbolicName ;

procedure_result_field -> symbolic_name                                                         : {procedureResultField, '$1'}.

%% ProcedureName = Namespace, SymbolicName ;

procedure_name ->           symbolic_name                                                       : {procedureName, [],   '$1'}.
procedure_name -> namespace symbolic_name                                                       : {procedureName, '$1', '$2'}.

%% Namespace = { SymbolicName, '.' } ;

namespace ->           symbolic_name '.'                                                        :         ['$1'].
namespace -> namespace symbolic_name '.'                                                        : '$1' ++ ['$2'].

%% ListComprehension = '[', [SP], FilterExpression, [[SP], '|', [SP], Expression], [SP], ']' ;

list_comprehension -> '[' filter_expression                ']'                                  : {listComprehension, '$2', []}.
list_comprehension -> '[' filter_expression '|' expression ']'                                  : {listComprehension, '$2', '$4'}.

%% PatternComprehension = '[', [SP], [Variable, [SP], '=', [SP]], RelationshipsPattern, [SP], [(W,H,E,R,E), [SP], Expression, [SP]], '|', [SP], Expression, [SP], ']' ;

pattern_comprehension -> '['              relationships_pattern                  '|' expression ']'
                                                                                                : {patternComprehension, [],   '$2', [],   '$4'}.
pattern_comprehension -> '['              relationships_pattern WHERE expression '|' expression ']'
                                                                                                : {patternComprehension, [],   '$2', '$4', '$6'}.
pattern_comprehension -> '[' variable '=' relationships_pattern                  '|' expression ']'
                                                                                                : {patternComprehension, '$2', '$4', [],   '$6'}.
pattern_comprehension -> '[' variable '=' relationships_pattern WHERE expression '|' expression ']'
                                                                                                : {patternComprehension, '$2', '$4', '$6', '$8'}.

%% PropertyLookup = '.', [SP], (PropertyKeyName) ;

property_lookup -> '.' property_key_name                                                        : {propertyLookup, '$2'}.

%% CaseExpression = (((C,A,S,E), { [SP], CaseAlternatives }-) | ((C,A,S,E), [SP], Expression, { [SP], CaseAlternatives }-)), [[SP], (E,L,S,E), [SP], Expression], [SP], (E,N,D) ;

case_expression -> CASE            case_alternatives_list                 END                   : {caseExpression, [],   '$2', []}.
case_expression -> CASE            case_alternatives_list ELSE expression END                   : {caseExpression, [],   '$2', '$4'}.
case_expression -> CASE expression case_alternatives_list                 END                   : {caseExpression, '$2', '$3', []}.
case_expression -> CASE expression case_alternatives_list ELSE expression END                   : {caseExpression, '$2', '$3', '$5'}.

%% =============================================================================
%% Helper definitions.
%% -----------------------------------------------------------------------------
case_alternatives_list ->                        case_alternatives                              :         ['$1'].
case_alternatives_list -> case_alternatives_list case_alternatives                              : '$1' ++ ['$2'].
%% =============================================================================

%% CaseAlternatives = (W,H,E,N), [SP], Expression, [SP], (T,H,E,N), [SP], Expression ;

case_alternatives -> WHEN expression THEN expression                                            : {caseAlternatives, '$2', '$4'}.

%% Variable = SymbolicName ;

variable -> symbolic_name                                                                       : {variable, '$1'}.

%% StringLiteral = ('"', { ANY - ('"' | '\') | EscapedChar }, '"')
%%               | ("'", { ANY - ("'" | '\') | EscapedChar }, "'")
%%               ;

%% ==> see lexer definition <===

%% EscapedChar = '\', ('\' | "'" | '"' | (B) | (F) | (N) | (R) | (T) | ((U), 4 * HexDigit) | ((U), 8 * HexDigit)) ;

%% ===> not relevant <===

%% NumberLiteral = DoubleLiteral
%%               | IntegerLiteral
%%               ;

number_literal -> double_literal                                                                : {numberLiteral, '$1'}.
number_literal -> integer_literal                                                               : {numberLiteral, '$1'}.

%% MapLiteral = '{', [SP], [PropertyKeyName, [SP], ':', [SP], Expression, [SP], { ',', [SP], PropertyKeyName, [SP], ':', [SP], Expression, [SP] }], '}' ;

map_literal -> '{'                                        '}'                                   : {mapLiteral, []}.
map_literal -> '{' property_key_name_expression_commalist '}'                                   : {mapLiteral, '$2'}.

%% =============================================================================
%% Helper definitions.
%% -----------------------------------------------------------------------------
property_key_name_expression -> property_key_name ':' expression                                : {'$1', '$3'}.

property_key_name_expression_commalist -> property_key_name_expression                          : ['$1'].
property_key_name_expression_commalist -> property_key_name_expression ',' property_key_name_expression_commalist
                                                                                                : ['$1' | '$3'].
%% =============================================================================

%% Parameter = '$', (SymbolicName | DecimalInteger) ;

parameter -> '$' symbolic_name                                                                  : {parameter, '$2'}.
parameter -> '$' DECIMAL_INTEGER                                                                : {parameter, unwrap('$2')}.

%% PropertyExpression = Atom, { [SP], PropertyLookup }- ;

property_expression -> atom property_lookup_list                                                : {propertyExpression, '$1', '$2'}.

%% =============================================================================
%% Helper definitions.
%% -----------------------------------------------------------------------------
property_lookup_list ->                      property_lookup                                    :         ['$1'].
property_lookup_list -> property_lookup_list property_lookup                                    : '$1' ++ ['$2'].
%% =============================================================================

%% PropertyKeyName = SchemaName ;

property_key_name -> schema_name                                                                : {propertyKeyName, '$1'}.

%% IntegerLiteral = HexInteger
%%                | OctalInteger
%%                | DecimalInteger
%%                ;

integer_literal -> HEX_INTEGER                                                                  : {integerLiteral, unwrap('$1')}.
integer_literal -> OCTAL_INTEGER                                                                : {integerLiteral, unwrap('$1')}.
integer_literal -> DECIMAL_INTEGER                                                              : {integerLiteral, unwrap('$1')}.

%% HexInteger = '0x', { HexDigit }- ;

%% ==> see lexer definition <===

%% DecimalInteger = ZeroDigit
%%                | (NonZeroDigit, { Digit })
%%                ;

%% ==> see lexer definition <===

%% OctalInteger = ZeroDigit, { OctDigit }- ;

%% ==> see lexer definition <===

%% HexLetter = (A)
%%           | (B)
%%           | (C)
%%           | (D)
%%           | (E)
%%           | (F)
%%           ;

%% ==> see lexer definition <===

%% HexDigit = Digit
%%          | HexLetter
%%          ;

%% ===> not relevant <===

%% Digit = ZeroDigit
%%       | NonZeroDigit
%%       ;

%% ===> not relevant <===

%% NonZeroDigit = NonZeroOctDigit
%%              | '8'
%%              | '9'
%%              ;

%% ===> not relevant <===

%% NonZeroOctDigit = '1'
%%                 | '2'
%%                 | '3'
%%                 | '4'
%%                 | '5'
%%                 | '6'
%%                 | '7'
%%                 ;

%% ===> not relevant <===

%% OctDigit = ZeroDigit
%%          | NonZeroOctDigit
%%          ;

%% ===> not relevant <===

%% ZeroDigit = '0' ;

%% ===> not relevant <===

%% DoubleLiteral = ExponentDecimalReal
%%               | RegularDecimalReal
%%               ;

double_literal -> EXPONENT_DECIMAL_REAL                                                         : {doubleLiteral, unwrap('$1')}.
double_literal -> REGULAR_DECIMAL_REAL                                                          : {doubleLiteral, unwrap('$1')}.

%% ExponentDecimalReal = ({ Digit }- | ({ Digit }-, '.', { Digit }-) | ('.', { Digit }-)), ((E) | (E)), ['-'], { Digit }- ;

%% ==> see lexer definition <===

%% RegularDecimalReal = { Digit }, '.', { Digit }- ;

%% ==> see lexer definition <===

%% SchemaName = SymbolicName
%%            | ReservedWord
%%            ;

schema_name -> symbolic_name                                                                    : {schemaName, '$1'}.
schema_name -> reserved_word                                                                    : {schemaName, '$1'}.

%% ReservedWord = (A,L,L)
%%              | (A,S,C)
%%              | (A,S,C,E,N,D,I,N,G)
%%              | (B,Y)
%%              | (C,R,E,A,T,E)
%%              | (D,E,L,E,T,E)
%%              | (D,E,S,C)
%%              | (D,E,S,C,E,N,D,I,N,G)
%%              | (D,E,T,A,C,H)
%%              | (E,X,I,S,T,S)
%%              | (L,I,M,I,T)
%%              | (M,A,T,C,H)
%%              | (M,E,R,G,E)
%%              | (O,N)
%%              | (O,P,T,I,O,N,A,L)
%%              | (O,R,D,E,R)
%%              | (R,E,M,O,V,E)
%%              | (R,E,T,U,R,N)
%%              | (S,E,T)
%%              | (S,K,I,P)
%%              | (W,H,E,R,E)
%%              | (W,I,T,H)
%%              | (U,N,I,O,N)
%%              | (U,N,W,I,N,D)
%%              | (A,N,D)
%%              | (A,S)
%%              | (C,O,N,T,A,I,N,S)
%%              | (D,I,S,T,I,N,C,T)
%%              | (E,N,D,S)
%%              | (I,N)
%%              | (I,S)
%%              | (N,O,T)
%%              | (O,R)
%%              | (S,T,A,R,T,S)
%%              | (X,O,R)
%%              | (F,A,L,S,E)
%%              | (T,R,U,E)
%%              | (N,U,L,L)
%%              | (C,O,N,S,T,R,A,I,N,T)
%%              | (D,O)
%%              | (F,O,R)
%%              | (R,E,Q,U,I,R,E)
%%              | (U,N,I,Q,U,E)
%%              | (C,A,S,E)
%%              | (W,H,E,N)
%%              | (T,H,E,N)
%%              | (E,L,S,E)
%%              | (E,N,D)
%%              | (M,A,N,D,A,T,O,R,Y)
%%              | (S,C,A,L,A,R)
%%              | (O,F)
%%              | (A,D,D)
%%              | (D,R,O,P)
%%              ;

reserved_word -> ALL                                                                            : {reservedWord, "all"}.
reserved_word -> ASC                                                                            : {reservedWord, "asc"}.
reserved_word -> ASCENDING                                                                      : {reservedWord, "ascending"}.
reserved_word -> BY                                                                             : {reservedWord, "by"}.
reserved_word -> CREATE                                                                         : {reservedWord, "create"}.
reserved_word -> DELETE                                                                         : {reservedWord, "delete"}.
reserved_word -> DESC                                                                           : {reservedWord, "desc"}.
reserved_word -> DESCENDING                                                                     : {reservedWord, "descending"}.
reserved_word -> DETACH                                                                         : {reservedWord, "detach"}.
reserved_word -> EXISTS                                                                         : {reservedWord, "exists"}.
reserved_word -> LIMIT                                                                          : {reservedWord, "limit"}.
reserved_word -> MATCH                                                                          : {reservedWord, "all"}.
reserved_word -> MERGE                                                                          : {reservedWord, "all"}.
reserved_word -> ON                                                                             : {reservedWord, "on"}.
reserved_word -> OPTIONAL                                                                       : {reservedWord, "optional"}.
reserved_word -> ORDER                                                                          : {reservedWord, "order"}.
reserved_word -> REMOVE                                                                         : {reservedWord, "remove"}.
reserved_word -> RETURN                                                                         : {reservedWord, "return"}.
reserved_word -> SET                                                                            : {reservedWord, "set"}.
reserved_word -> SKIP                                                                           : {reservedWord, "skip"}.
reserved_word -> WHERE                                                                          : {reservedWord, "where"}.
reserved_word -> WITH                                                                           : {reservedWord, "with"}.
reserved_word -> UNION                                                                          : {reservedWord, "union"}.
reserved_word -> UNWIND                                                                         : {reservedWord, "unwind"}.
reserved_word -> AND                                                                            : {reservedWord, "and"}.
reserved_word -> AS                                                                             : {reservedWord, "as"}.
reserved_word -> CONTAINS                                                                       : {reservedWord, "contains"}.
reserved_word -> DISTINCT                                                                       : {reservedWord, "distinct"}.
reserved_word -> ENDS                                                                           : {reservedWord, "ends"}.
reserved_word -> IN                                                                             : {reservedWord, "in"}.
reserved_word -> IS                                                                             : {reservedWord, "is"}.
reserved_word -> NOT                                                                            : {reservedWord, "not"}.
reserved_word -> OR                                                                             : {reservedWord, "or"}.
reserved_word -> STARTS                                                                         : {reservedWord, "starts"}.
reserved_word -> XOR                                                                            : {reservedWord, "xor"}.
reserved_word -> FALSE                                                                          : {reservedWord, "false"}.
reserved_word -> TRUE                                                                           : {reservedWord, "true"}.
reserved_word -> NULL                                                                           : {reservedWord, "null"}.
reserved_word -> CONSTRAINT                                                                     : {reservedWord, "constraint"}.
reserved_word -> DO                                                                             : {reservedWord, "do"}.
reserved_word -> FOR                                                                            : {reservedWord, "for"}.
reserved_word -> REQUIRE                                                                        : {reservedWord, "require"}.
reserved_word -> UNIQUE                                                                         : {reservedWord, "unique"}.
reserved_word -> CASE                                                                           : {reservedWord, "case"}.
reserved_word -> WHEN                                                                           : {reservedWord, "when"}.
reserved_word -> THEN                                                                           : {reservedWord, "then"}.
reserved_word -> ELSE                                                                           : {reservedWord, "else"}.
reserved_word -> END                                                                            : {reservedWord, "end"}.
reserved_word -> MANDATORY                                                                      : {reservedWord, "mandatory"}.
reserved_word -> SCALAR                                                                         : {reservedWord, "scalar"}.
reserved_word -> OF                                                                             : {reservedWord, "of"}.
reserved_word -> ADD                                                                            : {reservedWord, "add"}.
reserved_word -> DROP                                                                           : {reservedWord, "drop"}.

%% SymbolicName = UnescapedSymbolicName
%%              | EscapedSymbolicName
%%              | HexLetter
%%              | (C,O,U,N,T)
%%              | (F,I,L,T,E,R)
%%              | (E,X,T,R,A,C,T)
%%              | (A,N,Y)
%%              | (N,O,N,E)
%%              | (S,I,N,G,L,E)
%%              ;

symbolic_name -> ESCAPED_SYMBOLIC_NAME                                                          : {symbolicName, unwrap('$1')}.
symbolic_name -> UNESCAPED_SYMBOLIC_NAME                                                        : {symbolicName, unwrap('$1')}.
symbolic_name -> HEX_LETTER                                                                     : {symbolicName, unwrap('$1')}.
symbolic_name -> COUNT                                                                          : {symbolicName, "count"}.
symbolic_name -> FILTER                                                                         : {symbolicName, "filter"}.
symbolic_name -> EXTRACT                                                                        : {symbolicName, "extract"}.
symbolic_name -> ANY                                                                            : {symbolicName, "any"}.
symbolic_name -> NONE                                                                           : {symbolicName, "none"}.
symbolic_name -> SINGLE                                                                         : {symbolicName, "single"}.

%% UnescapedSymbolicName = IdentifierStart, { IdentifierPart } ;

%% ==> see lexer definition <===

%% Rest of grammar definition except of 'comment' (see lexer definition) is not relevant for this parser

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Expect 2.
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Erlang code.

%% -----------------------------------------------------------------------------
%%
%% ocparse.erl: opencypher - parser.
%%
%% Copyright (c) 2017 Walter Weinmann.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -----------------------------------------------------------------------------

% parser and compiler interface
-export([
    is_reserved/1,
    pt_to_source_bu/1,
    pt_to_source_td/1,
    source_to_pt/1
]).

-define(NODEBUG, true).
-include_lib("eunit/include/eunit.hrl").

-define(Dbg(__Rule, __Production),
    begin
        io:format(user, "__ "??__Rule" (~p)~n", [__Production]),
        __Production
    end).

%% -----------------------------------------------------------------------------
%%  Parser helper functions.
%% -----------------------------------------------------------------------------

-spec is_reserved(binary() | atom() | list()) -> true
                                               | false.

is_reserved(Word) when is_binary(Word) ->
    is_reserved(erlang:binary_to_list(Word));
is_reserved(Word) when is_atom(Word) ->
    is_reserved(erlang:atom_to_list(Word));
is_reserved(Word) when is_list(Word) ->
    lists:member(erlang:list_to_atom(string:to_upper(Word)),
        oclexer:reserved_keywords()).

unwrap({_, _, X}) ->
    X.

%% -----------------------------------------------------------------------------
%% Compiler.
%% -----------------------------------------------------------------------------

-spec pt_to_source_bu(tuple()| list()) -> {error, term()}
                                        | binary().

pt_to_source_bu(PTree) ->
    fold_bu(fun(_, _) ->
        null_fun end, null_fun, PTree).

-spec pt_to_source_td(tuple()| list()) -> {error, term()}
                                        | binary().

pt_to_source_td(PTree) ->
    fold_td(fun(_, _) ->
        null_fun end, null_fun, PTree).

-spec fold_bu(fun(), term(), tuple()) -> {error, term()}
                                       | binary().

fold_bu(Fun, Ctx, PTree) when is_function(Fun, 2) ->
    try ocparse_util:pt_to_source(bottom_up, Fun, Ctx, 0, PTree) of
        {error, _} = Error ->
            Error;
        {Source, null_fun = Ctx} ->
            list_to_binary(string:strip(Source));
        {_Output, NewCtx} ->
            NewCtx
    catch
        _:Error ->
            {error, Error}
    end.

-spec fold_td(fun(), term(), tuple() | list()) -> {error, term()}
                                                | binary().

fold_td(Fun, Ctx, PTree) when is_function(Fun, 2) ->
    try ocparse_util:pt_to_source(top_down, Fun, Ctx, 0, PTree) of
        {error, _} = Error ->
            Error;
        {Source, null_fun = Ctx} ->
            list_to_binary(string:strip(Source));
        {_Output, NewCtx} ->
            NewCtx
    catch
        _:Error ->
            {error, Error}
    end.

%% -----------------------------------------------------------------------------
%% Parser.
%% -----------------------------------------------------------------------------

-spec source_to_pt(binary()|list()) -> {parse_error, term()}
                                     | {lex_error, term()}
                                     | {ok, {[tuple()], list()}}.

source_to_pt([]) ->
    {parse_error, invalid_string};
source_to_pt(<<>>) ->
    {parse_error, invalid_string};
source_to_pt(Source0) ->
    Source = re:replace(Source0, "(^[ \r\n]+)|([ \r\n]+$)", "",
        [global, {return, list}]),
    [C | _] = lists:reverse(Source),
    NSource = if C =:= $; ->
        Source;
                  true ->
                      string:strip(Source)
              end,
    case oclexer:string(NSource) of
        {ok, Toks, _} ->
            case ocparse:parse(Toks) of
                {ok, PTree} ->
                    {ok, {PTree, Toks}};
                {error, {N, ?MODULE, ErrorTerms}} ->
                    {parse_error, {lists:flatten([integer_to_list(N), ": ", ErrorTerms]), Toks}};
                {error, Error} ->
                    {parse_error, {Error, Toks}}
            end;
        {error, Error, _} ->
            {lex_error, Error}
    end.
