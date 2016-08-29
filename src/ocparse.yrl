%% -*- erlang -*-
Header "%% Copyright (C) Walter Weinmann"
"%% @private"
"%% @Author Walter Weinmann".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Nonterminals
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 anonymous_pattern_part
 atom
 clause
 clause_list
 create
 cypher
 delete
 double_literal
 expression
 expression_commalist
 expression_10
 expression_10_addon_list
 expression_11
 expression_11_addon_list
 expression_12
 expression_12_addon_list
 expression_2
 expression_2_addon_list
 expression_3
 expression_3_addon
 expression_3_addon_list
 expression_4
 expression_4_addon
 expression_4_addon_list
 expression_5
 expression_5_addon_list
 expression_6
 expression_6_addon_list
 expression_7
 expression_7_addon_list
 expression_8
 expression_8_addon_list
 expression_9
 expression_9_addon_list
 filter_expression
 function_invocation
 function_name
 integer_literal
 id_in_coll
 label_name
 limit
 list_comprehension
 map_literal
 match
 merge
 merge_action
 merge_action_list
 node_label
 node_label_list
 node_labels
 node_pattern
 number_literal
 order
 parameter
 parenthesized_expression
 partial_comparison_expression
 pattern
 pattern_element
 pattern_element_chain
 pattern_element_chain_list
 pattern_part
 pattern_part_commalist
 properties
 property_expression
 property_key_name
 property_key_name_expression
 property_key_name_expression_commalist
 property_lookup
 property_lookup_list
 query
 range_literal
 regular_query
 rel_type_name
 rel_type_verticalbarlist
 relationship_detail
 relationship_pattern
 relationship_types
 relationships_pattern
 remove
 remove_item
 remove_item_commalist
 return
 return_body
 return_item
 return_item_commalist
 return_items
 set
 set_item
 set_item_commalist
 single_query
 skip
 sort_item
 sort_item_commalist
 statement
 symbolic_name
 union
 union_list
 unwind
 variable
 where
 with
 .

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Terminals
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 ALL
 AND
 ANY
 AS
 ASC
 ASCENDING
 BY
 CONTAINS
 COUNT
 CREATE
 DECIMAL_INTEGER
 DELETE
 DESC
 DESCENDING
 DETACH
 DISTINCT
 ENDS
 ESCAPED_SYMBOLIC_NAME
 EXPONENT_DECIMAL_REAL
 EXTRACT
 FALSE
 FILTER
 HEX_INTEGER
 IN
 IS
 LIMIT
 MATCH
 MERGE
 NONE
 NOT
 NULL
 OCTAL_INTEGER
 ON
 OPTIONAL
 OR
 ORDER
 REGULAR_DECIMAL_REAL
 REMOVE
 RETURN
 SET
 SINGLE
 SKIP
 STARTS
 STRING_LITERAL
 TRUE
 UNESCAPED_SYMBOLIC_NAME
 UNION
 UNWIND
 WHERE
 WITH
 XOR
 '='
 '=~'
 '!='
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
 '!'
 '?'
 '$'
 '<-->'
 '<--'
 '-->'
 '--'
 '[..]'
.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Rootsymbol
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 cypher.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Endsymbol
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 '$end'.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Operator precedences.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Right       010 properties.

Left        100 'OR'.
Left        110 'XOR'.
Left        120 'AND'.
Left        130 'NOT'.

Right       200 '='.
Left        210 '<>' '!=' '<' '>' '<=' '>='.

Left        300 '+' '-'.
Left        310 '*' '/' '%'.

Left        400 '^'.
Left        410 expression_4_addon.                                                             % Unary operator

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Grammar rules.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cypher -> statement                                                                             : {cypher, '$1', []}.
cypher -> statement ';'                                                                         : {cypher, '$1', ";"}.

%% =====================================================================================================================
%% Helper definitions - test purposes.
%% ---------------------------------------------------------------------------------------------------------------------
% cypher -> expression                                                                            : '$1'.
%% =====================================================================================================================

statement -> query                                                                              : {statement, '$1'}.

query -> regular_query                                                                          : {query, '$1'}.

regular_query -> single_query                                                                   : {regularQuery, '$1', []}.
regular_query -> single_query union_list                                                        : {regularQuery, '$1', '$2'}.

%% =====================================================================================================================
%% Helper definitions.
%% ---------------------------------------------------------------------------------------------------------------------
union_list ->            union                                                                  :         ['$1'].
union_list -> union_list union                                                                  : '$1' ++ ['$2'].
%% =====================================================================================================================

single_query -> clause_list                                                                     : {singleQuery, '$1'}.

%% =====================================================================================================================
%% Helper definitions.
%% ---------------------------------------------------------------------------------------------------------------------
clause_list ->             clause                                                               :         ['$1'].
clause_list -> clause_list clause                                                               : '$1' ++ ['$2'].
%% =====================================================================================================================

union -> UNION     single_query                                                                 : {union, [],    '$2'}.
union -> UNION ALL single_query                                                                 : {union, "all", '$3'}.

clause -> match                                                                                 : {clause, '$1'}.
clause -> unwind                                                                                : {clause, '$1'}.
clause -> merge                                                                                 : {clause, '$1'}.
clause -> create                                                                                : {clause, '$1'}.
clause -> set                                                                                   : {clause, '$1'}.
clause -> delete                                                                                : {clause, '$1'}.
clause -> remove                                                                                : {clause, '$1'}.
clause -> with                                                                                  : {clause, '$1'}.
clause -> return                                                                                : {clause, '$1'}.

match ->          MATCH pattern                                                                 : {match, [],         '$2', []}.
match ->          MATCH pattern where                                                           : {match, [],         '$2', '$3'}.
match -> OPTIONAL MATCH pattern                                                                 : {match, "optional", '$3', []}.
match -> OPTIONAL MATCH pattern where                                                           : {match, "optional", '$3', '$4'}.

unwind -> UNWIND expression AS variable                                                         : {unwind, '$2', '$4'}.

merge -> MERGE pattern_part                                                                     : {merge, '$2', []}.
merge -> MERGE pattern_part merge_action_list                                                   : {merge, '$2', '$3'}.

%% =====================================================================================================================
%% Helper definitions.
%% ---------------------------------------------------------------------------------------------------------------------
merge_action_list ->                   merge_action                                             :         ['$1'].
merge_action_list -> merge_action_list merge_action                                             : '$1' ++ ['$2'].
%% ---------------------------------------------------------------------------------------------------------------------

merge_action -> ON MATCH  set                                                                   : {mergeAction, "match",  '$3'}.
merge_action -> ON CREATE set                                                                   : {mergeAction, "create", '$3'}.

create -> CREATE pattern                                                                        : {create, '$2'}.

set -> SET set_item_commalist                                                                   : {set, '$2'}.

%% =====================================================================================================================
%% Helper definitions.
%% ---------------------------------------------------------------------------------------------------------------------
set_item_commalist -> set_item                                                                  : ['$1'].
set_item_commalist -> set_item ',' set_item_commalist                                           : ['$1' | '$3'].
%% ---------------------------------------------------------------------------------------------------------------------

set_item -> property_expression '='  expression                                                 : {setItem, '$1', "=",  '$3'}.
set_item -> variable            '='  expression                                                 : {setItem, '$1', "=",  '$3'}.
set_item -> variable            '+=' expression                                                 : {setItem, '$1', "+=", '$3'}.
set_item -> variable                 node_labels                                                : {setItem, '$1', [],   '$2'}.

delete ->        DELETE expression_commalist                                                    : {delete, [],       '$2'}.
delete -> DETACH DELETE expression_commalist                                                    : {delete, "detach", '$3'}.

%% =====================================================================================================================
%% Helper definitions.
%% ---------------------------------------------------------------------------------------------------------------------
expression_commalist -> expression                                                              : ['$1'].
expression_commalist -> expression ',' expression_commalist                                     : ['$1' | '$3'].
%% ---------------------------------------------------------------------------------------------------------------------

remove -> REMOVE remove_item_commalist                                                          : {remove, '$2'}.

%% =====================================================================================================================
%% Helper definitions.
%% ---------------------------------------------------------------------------------------------------------------------
remove_item_commalist -> remove_item                                                            : ['$1'].
remove_item_commalist -> remove_item ',' remove_item_commalist                                  : ['$1' | '$3'].
%% ---------------------------------------------------------------------------------------------------------------------

remove_item -> variable node_labels                                                             : {removeItem, '$1', '$2'}.
remove_item -> property_expression                                                              : {removeItem, '$1'}.

with -> WITH          return_body                                                               : {with, [],         '$2', []}.
with -> WITH          return_body where                                                         : {with, [],         '$2', '$3'}.
with -> WITH DISTINCT return_body                                                               : {with, "distinct", '$3', []}.
with -> WITH DISTINCT return_body where                                                         : {with, "distinct", '$3', '$4'}.

return -> RETURN          return_body                                                           : {return, [],         '$2'}.
return -> RETURN DISTINCT return_body                                                           : {return, "distinct", '$3'}.

return_body -> return_items                                                                     : {returnBody, '$1', [],   [],   []}.
return_body -> return_items            limit                                                    : {returnBody, '$1', [],   [],   '$2'}.
return_body -> return_items       skip                                                          : {returnBody, '$1', [],   '$2', []}.
return_body -> return_items       skip limit                                                    : {returnBody, '$1', [],   '$2', '$3'}.
return_body -> return_items order                                                               : {returnBody, '$1', '$2', [],   []}.
return_body -> return_items order      limit                                                    : {returnBody, '$1', '$2', [],   '$3'}.
return_body -> return_items order skip                                                          : {returnBody, '$1', '$2', '$3', []}.
return_body -> return_items order skip limit                                                    : {returnBody, '$1', '$2', '$3', '$4'}.

return_items -> '*'                                                                             : {returnItems, "*", [],  []}.
return_items -> '*' ',' return_item_commalist                                                   : {returnItems, "*", ",", '$3'}.
return_items ->         return_item_commalist                                                   : {returnItems, [],  [],  '$1'}.

%% =====================================================================================================================
%% Helper definitions.
%% ---------------------------------------------------------------------------------------------------------------------
return_item_commalist -> return_item                                                            : ['$1'].
return_item_commalist -> return_item ',' return_item_commalist                                  : ['$1' | '$3'].
%% ---------------------------------------------------------------------------------------------------------------------

return_item -> expression                                                                       : {returnItem, '$1', []}.
return_item -> expression AS variable                                                           : {returnItem, '$1', '$3'}.

order -> ORDER BY sort_item_commalist                                                           : {order, '$3'}.

%% =====================================================================================================================
%% Helper definitions.
%% ---------------------------------------------------------------------------------------------------------------------
sort_item_commalist -> sort_item                                                                : ['$1'].
sort_item_commalist -> sort_item ',' sort_item_commalist                                        : ['$1' | '$3'].
%% ---------------------------------------------------------------------------------------------------------------------

skip -> SKIP expression                                                                         : {skip, '$2'}.

limit -> LIMIT expression                                                                       : {limit, '$2'}.

sort_item -> expression DESCENDING                                                              : {sortItem, '$1', "descending"}.
sort_item -> expression DESC                                                                    : {sortItem, '$1', "desc"}.
sort_item -> expression ASCENDING                                                               : {sortItem, '$1', "ascending"}.
sort_item -> expression ASC                                                                     : {sortItem, '$1', "asc"}.
sort_item -> expression                                                                         : {sortItem, '$1', []}.

where -> WHERE expression                                                                       : {where, '$2'}.

pattern -> pattern_part_commalist                                                               : {pattern, '$1'}.

%% =====================================================================================================================
%% Helper definitions.
%% ---------------------------------------------------------------------------------------------------------------------
pattern_part_commalist -> pattern_part                                                          : ['$1'].
pattern_part_commalist -> pattern_part ',' pattern_part_commalist                               : ['$1' | '$3'].
%% =====================================================================================================================

pattern_part -> variable '=' anonymous_pattern_part                                             : {patternPart, '$1', '$3'}.
pattern_part ->              anonymous_pattern_part                                             : {patternPart, [],   '$1'}.

anonymous_pattern_part -> pattern_element                                                       : {anonymousPatternPart, '$1'}.

pattern_element -> node_pattern                                                                 : {patternElement, '$1', []}.
pattern_element -> node_pattern pattern_element_chain_list                                      : {patternElement, '$1', '$2'}.
pattern_element -> '(' pattern_element ')'                                                      : {patternElement, '$2', "("}.

%% =====================================================================================================================
%% Helper definitions.
%% ---------------------------------------------------------------------------------------------------------------------
pattern_element_chain_list ->                            pattern_element_chain                  :         ['$1'].
pattern_element_chain_list -> pattern_element_chain_list pattern_element_chain                  : '$1' ++ ['$2'].
%% =====================================================================================================================

node_pattern -> '('                                 ')'                                         : {nodePattern, [],   [],   []}.
node_pattern -> '('          node_labels            ')'                                         : {nodePattern, [],   '$2', []}.
node_pattern -> '(' variable                        ')'                                         : {nodePattern, '$2', [],   []}.
node_pattern -> '(' variable node_labels            ')'                                         : {nodePattern, '$2', '$3', []}.
node_pattern -> '('                      properties ')'                                         : {nodePattern, [],   [],   '$2'}.
node_pattern -> '('          node_labels properties ')'                                         : {nodePattern, [],   '$2', '$3'}.
node_pattern -> '(' variable             properties ')'                                         : {nodePattern, '$2', [],   '$3'}.
node_pattern -> '(' variable node_labels properties ')'                                         : {nodePattern, '$2', '$3', '$4'}.

pattern_element_chain -> relationship_pattern node_pattern                                      : {patternElementChain, '$1', '$2'}.

relationship_pattern -> '<-->'                                                                  : {relationshipPattern, "<-->, [],   []"}.
relationship_pattern -> '<--'                                                                   : {relationshipPattern, "<--", [],   [] }.
relationship_pattern ->  '-->'                                                                  : {relationshipPattern, "-->",  [],  []}.
relationship_pattern ->  '--'                                                                   : {relationshipPattern, "--",  [],   []}.
relationship_pattern -> '<' '-' relationship_detail '-' '>'                                     : {relationshipPattern, "<-",  '$3', "->"}.
relationship_pattern -> '<' '-' relationship_detail '-'                                         : {relationshipPattern, "<-",  '$3', "-"}.
relationship_pattern ->     '-' relationship_detail '-' '>'                                     : {relationshipPattern, "-",   '$2', "->"}.
relationship_pattern ->     '-' relationship_detail '-'                                         : {relationshipPattern, "-",   '$2', "-"}.

relationship_detail -> '['                                                          ']'         : {relationshipDetail, [],   [],   [],   [],   []}.
relationship_detail -> '['                                 range_literal            ']'         : {relationshipDetail, [],   [],   [],   '$2', []}.
relationship_detail -> '['              relationship_types                          ']'         : {relationshipDetail, [],   [],   '$2', [],   []}.
relationship_detail -> '['              relationship_types range_literal            ']'         : {relationshipDetail, [],   [],   '$2', '$3', []}.
relationship_detail -> '['          '?'                                             ']'         : {relationshipDetail, [],   "?",  [],   [],   []}.
relationship_detail -> '['          '?'                    range_literal            ']'         : {relationshipDetail, [],   "?",  [],   '$3', []}.
relationship_detail -> '['          '?' relationship_types                          ']'         : {relationshipDetail, [],   "?",  '$3', [],   []}.
relationship_detail -> '['          '?' relationship_types range_literal            ']'         : {relationshipDetail, [],   "?",  '$3', '$4', []}.
relationship_detail -> '[' variable                                                 ']'         : {relationshipDetail, '$2', [],   [],   [],   []}.
relationship_detail -> '[' variable                        range_literal            ']'         : {relationshipDetail, '$2', [],   [],   '$3', []}.
relationship_detail -> '[' variable     relationship_types                          ']'         : {relationshipDetail, '$2', [],   '$3', [],   []}.
relationship_detail -> '[' variable     relationship_types range_literal            ']'         : {relationshipDetail, '$2', [],   '$3', '$4', []}.
relationship_detail -> '[' variable '?'                                             ']'         : {relationshipDetail, '$2', "?",  [],   [],   []}.
relationship_detail -> '[' variable '?'                    range_literal            ']'         : {relationshipDetail, '$2', "?",  [],   '$4', []}.
relationship_detail -> '[' variable '?' relationship_types                          ']'         : {relationshipDetail, '$2', "?",  '$4', [],   []}.
relationship_detail -> '[' variable '?' relationship_types range_literal            ']'         : {relationshipDetail, '$2', "?",  '$4', '$5', []}.
relationship_detail -> '['                                               properties ']'         : {relationshipDetail, [],   [],   [],   [],   '$2'}.
relationship_detail -> '['                                 range_literal properties ']'         : {relationshipDetail, [],   [],   [],   '$2', '$3'}.
relationship_detail -> '['              relationship_types               properties ']'         : {relationshipDetail, [],   [],   '$2', [],   '$3'}.
relationship_detail -> '['              relationship_types range_literal properties ']'         : {relationshipDetail, [],   [],   '$2', '$3', '$4'}.
relationship_detail -> '['          '?'                                  properties ']'         : {relationshipDetail, [],   "?",  [],   [],   '$3'}.
relationship_detail -> '['          '?'                    range_literal properties ']'         : {relationshipDetail, [],   "?",  [],   '$3', '$4'}.
relationship_detail -> '['          '?' relationship_types               properties ']'         : {relationshipDetail, [],   "?",  '$3', [],   '$4'}.
relationship_detail -> '['          '?' relationship_types range_literal properties ']'         : {relationshipDetail, [],   "?",  '$3', '$4', '$5'}.
relationship_detail -> '[' variable                                      properties ']'         : {relationshipDetail, '$2', [],   [],   [],   '$3'}.
relationship_detail -> '[' variable                        range_literal properties ']'         : {relationshipDetail, '$2', [],   [],   '$3', '$4'}.
relationship_detail -> '[' variable     relationship_types               properties ']'         : {relationshipDetail, '$2', [],   '$3', [],   '$4'}.
relationship_detail -> '[' variable     relationship_types range_literal properties ']'         : {relationshipDetail, '$2', [],   '$3', '$4', '$5'}.
relationship_detail -> '[' variable '?'                                  properties ']'         : {relationshipDetail, '$2', "?",  [],   [],   '$4'}.
relationship_detail -> '[' variable '?'                    range_literal properties ']'         : {relationshipDetail, '$2', "?",  [],   '$4', '$5'}.
relationship_detail -> '[' variable '?' relationship_types               properties ']'         : {relationshipDetail, '$2', "?",  '$4', [],   '$5'}.
relationship_detail -> '[' variable '?' relationship_types range_literal properties ']'         : {relationshipDetail, '$2', "?",  '$4', '$5', '$6'}.

properties -> map_literal                                                                       : {properties, '$1'}.
properties -> parameter                                                                         : {properties, '$1'}.

relationship_types -> ':' rel_type_name                                                         : {relationshipTypes, '$2', []}.
relationship_types -> ':' rel_type_name rel_type_verticalbarlist                                : {relationshipTypes, '$2', '$3'}.

%% =====================================================================================================================
%% Helper definitions.
%% ---------------------------------------------------------------------------------------------------------------------
rel_type_verticalbarlist ->                          '|' ':' rel_type_name                      : [{'$3', ":"}].
rel_type_verticalbarlist ->                          '|'     rel_type_name                      : [{'$2', []}].
rel_type_verticalbarlist -> rel_type_verticalbarlist '|' ':' rel_type_name                      : '$1' ++ [{'$4', ":"}].
rel_type_verticalbarlist -> rel_type_verticalbarlist '|'     rel_type_name                      : '$1' ++ [{'$3', []}].
%% =====================================================================================================================

node_labels -> node_label_list                                                                  : {nodeLabels, '$1'}.

%% =====================================================================================================================
%% Helper definitions.
%% ---------------------------------------------------------------------------------------------------------------------
node_label_list ->                 node_label                                                   :         ['$1'].
node_label_list -> node_label_list node_label                                                   : '$1' ++ ['$2'].
%% =====================================================================================================================

node_label -> ':' label_name                                                                    : {nodeLabel, '$2'}.

range_literal -> '*'                                                                            : {rangeLiteral, [],   [],   []}.
range_literal -> '*'                '..'                                                        : {rangeLiteral, [],   "..", []}.
range_literal -> '*'                '..' integer_literal                                        : {rangeLiteral, [],   "..", '$3'}.
range_literal -> '*' integer_literal                                                            : {rangeLiteral, '$2', [],   []}.
range_literal -> '*' integer_literal '..'                                                       : {rangeLiteral, '$2', "..", []}.
range_literal -> '*' integer_literal '..' integer_literal                                       : {rangeLiteral, '$2', "..", '$4'}.

label_name -> symbolic_name                                                                     : {labelName, '$1'}.

rel_type_name -> symbolic_name                                                                  : {relTypeName, '$1'}.

expression -> expression_12                                                                     : {expression, '$1'}.

expression_12 -> expression_11                                                                  : {expression12, '$1', []}.
expression_12 -> expression_11 expression_12_addon_list                                         : {expression12, '$1', '$2'}.

%% =====================================================================================================================
%% Helper definitions.
%% ---------------------------------------------------------------------------------------------------------------------
expression_12_addon_list ->                          OR expression_11                           :         [{"or", '$2'}].
expression_12_addon_list -> expression_12_addon_list OR expression_11                           : '$1' ++ [{"or", '$3'}].
%% =====================================================================================================================

expression_11 -> expression_10                                                                  : {expression11, '$1', []}.
expression_11 -> expression_10 expression_11_addon_list                                         : {expression11, '$1', '$2'}.

%% =====================================================================================================================
%% Helper definitions.
%% ---------------------------------------------------------------------------------------------------------------------
expression_11_addon_list ->                          XOR expression_10                           :         [{"xor", '$2'}].
expression_11_addon_list -> expression_11_addon_list XOR expression_10                           : '$1' ++ [{"xor", '$3'}].
%% =====================================================================================================================

expression_10 -> expression_9                                                                   : {expression10, '$1', []}.
expression_10 -> expression_9 expression_10_addon_list                                          : {expression10, '$1', '$2'}.

%% =====================================================================================================================
%% Helper definitions.
%% ---------------------------------------------------------------------------------------------------------------------
expression_10_addon_list ->                          AND expression_9                           :         [{"and", '$2'}].
expression_10_addon_list -> expression_10_addon_list AND expression_9                           : '$1' ++ [{"and", '$3'}].
%% =====================================================================================================================

expression_9 ->                         expression_8                                            : {expression9, '$1', []}.
expression_9 -> expression_9_addon_list expression_8                                            : {expression9, '$2', '$1'}.

%% =====================================================================================================================
%% Helper definitions.
%% ---------------------------------------------------------------------------------------------------------------------
expression_9_addon_list ->                         NOT                                          :         [{"not"}].
expression_9_addon_list -> expression_9_addon_list NOT                                          : '$1' ++ [{"not"}].
%% =====================================================================================================================

expression_8 -> expression_7                                                                    : {expression8, '$1', []}.
expression_8 -> expression_7 expression_8_addon_list                                            : {expression8, '$1', '$2'}.

%% =====================================================================================================================
%% Helper definitions.
%% ---------------------------------------------------------------------------------------------------------------------
expression_8_addon_list ->                         partial_comparison_expression                :         ['$1'].
expression_8_addon_list -> expression_8_addon_list partial_comparison_expression                : '$1' ++ ['$2'].
%% =====================================================================================================================

expression_7 -> expression_6                                                                    : {expression7, '$1', []}.
expression_7 -> expression_6 expression_7_addon_list                                            : {expression7, '$1', '$2'}.

%% =====================================================================================================================
%% Helper definitions.
%% ---------------------------------------------------------------------------------------------------------------------
expression_7_addon_list ->                         '+' expression_6                             :         [{"+", '$2'}].
expression_7_addon_list ->                         '-' expression_6                             :         [{"-", '$2'}].
expression_7_addon_list -> expression_7_addon_list '+' expression_6                             : '$1' ++ [{"+", '$3'}].
expression_7_addon_list -> expression_7_addon_list '-' expression_6                             : '$1' ++ [{"-", '$3'}].
%% =====================================================================================================================

expression_6 -> expression_5                                                                    : {expression6, '$1', []}.
expression_6 -> expression_5 expression_6_addon_list                                            : {expression6, '$1', '$2'}.

%% =====================================================================================================================
%% Helper definitions.
%% ---------------------------------------------------------------------------------------------------------------------
expression_6_addon_list ->                         '*' expression_5                             :         [{"*", '$2'}].
expression_6_addon_list ->                         '/' expression_5                             :         [{"/", '$2'}].
expression_6_addon_list ->                         '%' expression_5                             :         [{"%", '$2'}].
expression_6_addon_list -> expression_6_addon_list '*' expression_5                             : '$1' ++ [{"*", '$3'}].
expression_6_addon_list -> expression_6_addon_list '/' expression_5                             : '$1' ++ [{"/", '$3'}].
expression_6_addon_list -> expression_6_addon_list '%' expression_5                             : '$1' ++ [{"%", '$3'}].
%% =====================================================================================================================

expression_5 -> expression_4                                                                    : {expression5, '$1', []}.
expression_5 -> expression_4 expression_5_addon_list                                            : {expression5, '$1', '$2'}.

%% =====================================================================================================================
%% Helper definitions.
%% ---------------------------------------------------------------------------------------------------------------------
expression_5_addon_list ->                         '^' expression_4                             :         [{"^", '$2'}].
expression_5_addon_list -> expression_5_addon_list '^' expression_4                             : '$1' ++ [{"^", '$3'}].
%% =====================================================================================================================

expression_4 ->                         expression_3                                            : {expression4, '$1', [] }.
expression_4 -> expression_4_addon_list expression_3                                            : {expression4, '$2', '$1'}.

%% =====================================================================================================================
%% Helper definitions.
%% ---------------------------------------------------------------------------------------------------------------------
expression_4_addon_list ->                         expression_4_addon                           :         ['$1'].
expression_4_addon_list -> expression_4_addon_list expression_4_addon                           : '$1' ++ ['$2'].

expression_4_addon -> '+'                                                                       : {"+"}.
expression_4_addon -> '-'                                                                       : {"-"}.
%% =====================================================================================================================

expression_3 -> expression_2                                                                    : {expression3, '$1', []}.
expression_3 -> expression_2 expression_3_addon_list                                            : {expression3, '$1', '$2'}.

%% =====================================================================================================================
%% Helper definitions.
%% ---------------------------------------------------------------------------------------------------------------------
expression_3_addon_list ->                         expression_3_addon                           :         ['$1'].
expression_3_addon_list -> expression_3_addon_list expression_3_addon                           : '$1' ++ ['$2'].

expression_3_addon -> '[' expression ']'                                                        : {"[",           '$2'}.
expression_3_addon -> '['            '..' expression ']'                                        : {"[",           [],   '$3'}.
expression_3_addon -> '[' expression '..'            ']'                                        : {"[",           '$2', []}.
expression_3_addon -> '[' expression '..' expression ']'                                        : {"[",           '$2', '$4'}.
expression_3_addon -> '=~'        expression_2                                                  : {"=~",          '$2'}.
expression_3_addon -> IN          expression_2                                                  : {"in",          '$2'}.
expression_3_addon -> STARTS WITH expression_2                                                  : {"starts with", '$3'}.
expression_3_addon -> ENDS WITH   expression_2                                                  : {"ends with",   '$3'}.
expression_3_addon -> CONTAINS    expression_2                                                  : {"contains",    '$2'}.
expression_3_addon -> IS NULL                                                                   : {"is null"}.
expression_3_addon -> IS NOT NULL                                                               : {"is not null"}.
%% =====================================================================================================================

expression_2 -> atom                                                                            : {expression2, '$1', []}.
expression_2 -> atom expression_2_addon_list                                                    : {expression2, '$1', '$2'}.

%% =====================================================================================================================
%% Helper definitions.
%% ---------------------------------------------------------------------------------------------------------------------
expression_2_addon_list ->                         node_labels                                  :         ['$1'].
expression_2_addon_list ->                         property_lookup                              :         ['$1'].
expression_2_addon_list -> expression_2_addon_list node_labels                                  : '$1' ++ ['$2'].
expression_2_addon_list -> expression_2_addon_list property_lookup                              : '$1' ++ ['$2'].
%% =====================================================================================================================

atom -> number_literal                                                                          : {atom, '$1'}.
atom -> STRING_LITERAL                                                                          : {atom, {stringLiteral, unwrap('$1')}}.
atom -> parameter                                                                               : {atom, '$1'}.
atom -> TRUE                                                                                    : {atom, {terminal, "true"}}.
atom -> FALSE                                                                                   : {atom, {terminal, "false"}}.
atom -> NULL                                                                                    : {atom, {terminal, "null"}}.
atom -> COUNT '(' '*' ')'                                                                       : {atom, {terminal, "count(*)"}}.
atom -> map_literal                                                                             : {atom, '$1'}.
atom -> list_comprehension                                                                      : {atom, '$1'}.
atom -> '['                expression_commalist ']'                                             : {atom, '$2', [],   "]"}.
atom -> '[' id_in_coll ',' expression_commalist ']'                                             : {atom, '$2', '$4', "]"}.
atom -> FILTER  '(' filter_expression ')'                                                       : {atom, {'filter',  '$3'}}.
atom -> EXTRACT '(' filter_expression '|' expression ')'                                        : {atom, {'extract', '$3', '$5'}}.
atom -> EXTRACT '(' filter_expression ')'                                                       : {atom, {'extract', '$3', []}}.
atom -> ALL     '(' filter_expression ')'                                                       : {atom, {'all',     '$3'}}.
atom -> ANY     '(' filter_expression ')'                                                       : {atom, {'any',     '$3'}}.
atom -> NONE    '(' filter_expression ')'                                                       : {atom, {'none',    '$3'}}.
atom -> SINGLE  '(' filter_expression ')'                                                       : {atom, {'single',  '$3'}}.
atom -> relationships_pattern                                                                   : {atom, '$1'}.
atom -> parenthesized_expression                                                                : {atom, '$1'}.
atom -> function_invocation                                                                     : {atom, '$1'}.
atom -> variable                                                                                : {atom, '$1'}.

partial_comparison_expression -> '='  expression_7                                              : {partialComparisonExpression, '$2', "="}.
partial_comparison_expression -> '<>' expression_7                                              : {partialComparisonExpression, '$2', "<>"}.
partial_comparison_expression -> '!=' expression_7                                              : {partialComparisonExpression, '$2', "!="}.
partial_comparison_expression -> '<'  expression_7                                              : {partialComparisonExpression, '$2', "<"}.
partial_comparison_expression -> '>'  expression_7                                              : {partialComparisonExpression, '$2', ">"}.
partial_comparison_expression -> '<=' expression_7                                              : {partialComparisonExpression, '$2', "<="}.
partial_comparison_expression -> '>=' expression_7                                              : {partialComparisonExpression, '$2', ">="}.

parenthesized_expression -> '(' expression ')'                                                  : {parenthesizedExpression, '$2'}.

relationships_pattern -> node_pattern pattern_element_chain_list                                : {relationshipsPattern, '$1', '$2'}.

filter_expression -> id_in_coll                                                                 : {filterExpression, '$1', []}.
filter_expression -> id_in_coll where                                                           : {filterExpression, '$1', '$2'}.

id_in_coll -> variable IN expression                                                            : {idInColl, '$1', '$3'}.

function_invocation -> function_name '('                               ')'                      : {functionInvocation, '$1', [],         []}.
function_invocation -> function_name '('          expression_commalist ')'                      : {functionInvocation, '$1', [],         '$3'}.
function_invocation -> function_name '(' DISTINCT                      ')'                      : {functionInvocation, '$1', "distinct", []}.
function_invocation -> function_name '(' DISTINCT expression_commalist ')'                      : {functionInvocation, '$1', "distinct", '$4'}.

%% =====================================================================================================================
%% Helper definitions.
%% ---------------------------------------------------------------------------------------------------------------------
function_invocation -> COUNT         '('                               ')'                     : {functionInvocation, {functionName, {symbolicName, "count"}}, [],         []}.
function_invocation -> COUNT         '('          expression_commalist ')'                     : {functionInvocation, {functionName, {symbolicName, "count"}}, [],         '$3'}.
function_invocation -> COUNT         '(' DISTINCT                      ')'                     : {functionInvocation, {functionName, {symbolicName, "count"}}, "distinct", []}.
function_invocation -> COUNT         '(' DISTINCT expression_commalist ')'                     : {functionInvocation, {functionName, {symbolicName, "count"}}, "distinct", '$4'}.
%% =====================================================================================================================

function_name -> symbolic_name                                                                  : {functionName, '$1'}.

list_comprehension -> '[' filter_expression                ']'                                  : {listComprehension, '$2', []}.
list_comprehension -> '[' filter_expression '|' expression ']'                                  : {listComprehension, '$2', '$4'}.

property_lookup -> '.' property_key_name '?'                                                    : {propertyLookup, '$2', "?"}.
property_lookup -> '.' property_key_name '!'                                                    : {propertyLookup, '$2', "!"}.
property_lookup -> '.' property_key_name                                                        : {propertyLookup, '$2', []}.

variable -> symbolic_name                                                                       : {variable, '$1'}.

number_literal -> double_literal                                                                : {numberLiteral, '$1'}.
number_literal -> integer_literal                                                               : {numberLiteral, '$1'}.

map_literal -> '{'                                        '}'                                   : {mapLiteral, []}.
map_literal -> '{' property_key_name_expression_commalist '}'                                   : {mapLiteral, '$2'}.

%% =====================================================================================================================
%% Helper definitions.
%% ---------------------------------------------------------------------------------------------------------------------
property_key_name_expression_commalist -> property_key_name_expression                          : ['$1'].
property_key_name_expression_commalist -> property_key_name_expression ',' property_key_name_expression_commalist
                                                                                                : ['$1' | '$3'].

property_key_name_expression -> property_key_name ':' expression                                : {'$1', '$3'}.
%% =====================================================================================================================

parameter -> '$' symbolic_name                                                                  : {parameter, '$2'}.
parameter -> '$' DECIMAL_INTEGER                                                                : {parameter, unwrap('$2')}.

property_expression -> atom property_lookup_list                                                : {propertyExpression, '$1', '$2'}.

%% =====================================================================================================================
%% Helper definitions.
%% ---------------------------------------------------------------------------------------------------------------------
property_lookup_list ->                      property_lookup                                    :         ['$1'].
property_lookup_list -> property_lookup_list property_lookup                                    : '$1' ++ ['$2'].
%% =====================================================================================================================

property_key_name -> symbolic_name                                                              : {propertyKeyName, '$1'}.

integer_literal -> HEX_INTEGER                                                                  : {integerLiteral, unwrap('$1')}.
integer_literal -> OCTAL_INTEGER                                                                : {integerLiteral, unwrap('$1')}.
integer_literal -> DECIMAL_INTEGER                                                              : {integerLiteral, unwrap('$1')}.

double_literal -> EXPONENT_DECIMAL_REAL                                                         : {doubleLiteral, unwrap('$1')}.
double_literal -> REGULAR_DECIMAL_REAL                                                          : {doubleLiteral, unwrap('$1')}.

symbolic_name -> ESCAPED_SYMBOLIC_NAME                                                          : {symbolicName, unwrap('$1')}.
symbolic_name -> UNESCAPED_SYMBOLIC_NAME                                                        : {symbolicName, unwrap('$1')}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Expect 2.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Erlang code.

-behaviour(application).
-behaviour(supervisor).

% application callbacks
-export([start/0, start/2, stop/1, stop/0]).

% Supervisor callbacks
-export([init/1]).

% parser and compiler interface
-export([fold/3,
         fold_bu/3,
         fold_td/3,
         is_reserved/1,
         parsetree/1,
         parsetree_to_string/1,
         parsetree_to_string_bu/1,
         parsetree_to_string_td/1,
         parsetree_with_tokens/1
        ]).

%-define(NODEBUG, true).
-include_lib("eunit/include/eunit.hrl").

-define(Dbg(__Rule, __Production),
begin
    io:format(user, "__ "??__Rule" (~p)~n", [__Production]),
    __Production
end).

%%-----------------------------------------------------------------------------
%%                          dummy application interface
%%-----------------------------------------------------------------------------

start() ->
    application:start(?MODULE).
stop() ->
    application:stop(?MODULE).

start(_Type, _Args) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).
stop(_State) ->
    ok.

init([]) ->
     {ok, { {one_for_one, 5, 10}, []} }.

%%-----------------------------------------------------------------------------
%%                          parser helper functions
%%-----------------------------------------------------------------------------

unwrap({_,_,X}) ->
    X.

%%-----------------------------------------------------------------------------


%%-----------------------------------------------------------------------------
%%                                  PARSER
%%-----------------------------------------------------------------------------
-spec parsetree(binary()|list()) ->
    {parse_error, term()} | {lex_error, term()} | {ok, [tuple()]}.
parsetree(Cypher) ->
   case parsetree_with_tokens(Cypher) of
       {ok, {ParseTree, _Tokens}} -> {ok, ParseTree};
       Error -> Error
   end.

-spec parsetree_with_tokens(binary()|list()) ->
    {parse_error, term()} | {lex_error, term()} | {ok, {[tuple()], list()}}.
parsetree_with_tokens([]) -> {parse_error, invalid_string};
parsetree_with_tokens(<<>>) -> {parse_error, invalid_string};
parsetree_with_tokens(Cypher0) ->
    Cypher = re:replace(Cypher0, "(^[ \r\n]+)|([ \r\n]+$)", "",
                     [global, {return, list}]),
    [C|_] = lists:reverse(Cypher),
    NCypher = if C =:= $; -> Cypher; true -> string:strip(Cypher) end,
    case oclexer:string(NCypher) of
        {ok, Toks, _} ->
            case ocparse:parse(Toks) of
                {ok, PTree} -> {ok, {PTree, Toks}};
                {error,{N,?MODULE,ErrorTerms}} ->
                    {parse_error, {lists:flatten([integer_to_list(N), ": ", ErrorTerms]), Toks}};
                {error,Error} -> {parse_error, {Error, Toks}}
            end;
        {error,Error,_} -> {lex_error, Error}
    end.

-spec is_reserved(binary() | atom() | list()) ->
    true | false.
is_reserved(Word) when is_binary(Word) ->
    is_reserved(erlang:binary_to_list(Word));
is_reserved(Word) when is_atom(Word) ->
    is_reserved(erlang:atom_to_list(Word));
is_reserved(Word) when is_list(Word) ->
    lists:member(erlang:list_to_atom(string:to_upper(Word)),
                 oclexer:reserved_keywords()).

%%-----------------------------------------------------------------------------


%%-----------------------------------------------------------------------------
%%                                  COMPILER
%%-----------------------------------------------------------------------------

-spec parsetree_to_string(tuple()| list()) ->
    {error, term()} | binary().
parsetree_to_string(PTree) ->
    parsetree_to_string_td(PTree).

-spec parsetree_to_string_bu(tuple()| list()) ->
    {error, term()} | binary().
parsetree_to_string_bu(PTree) ->
    fold_bu(fun(_,_) -> null_fun end, null_fun, PTree).

-spec parsetree_to_string_td(tuple()| list()) ->
    {error, term()} | binary().
parsetree_to_string_td(PTree) ->
    fold_td(fun(_,_) -> null_fun end, null_fun, PTree).

-spec fold(fun(), term(), tuple()) ->
    {error, term()} | binary().
fold(Fun, Ctx, PTree) when is_function(Fun, 2) ->
    fold_td(Fun, Ctx, PTree).

-spec fold_bu(fun(), term(), tuple()) ->
    {error, term()} | binary().
fold_bu(Fun, Ctx, PTree) when is_function(Fun, 2) ->
    try ocparse_fold:fold(bottom_up, Fun, Ctx, 0, PTree) of
        {error,_} = Error -> Error;
        {Cypher, null_fun = Ctx} -> list_to_binary(string:strip(Cypher));
        {_Output, NewCtx} -> NewCtx
    catch
        _:Error -> {error, Error}
    end.

-spec fold_td(fun(), term(), tuple() | list()) ->
    {error, term()} | binary().
fold_td(Fun, Ctx, PTree) when is_function(Fun, 2) ->
    try ocparse_fold:fold(top_down, Fun, Ctx, 0, PTree) of
        {error,_} = Error -> Error;
        {Cypher, null_fun = Ctx} ->
            list_to_binary(string:strip(Cypher));
        {_Output, NewCtx} ->
            NewCtx
    catch
        _:Error -> {error, Error}
    end.
