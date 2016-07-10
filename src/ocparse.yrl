%% -*- erlang -*-
Header "%% Copyright (C) Walter Weinmann"
"%% @private"
"%% @Author Walter Weinmann".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Nonterminals
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 all_opt
 anonymous_pattern_part
 atom
 char_opt
 char_question_mark_opt
 char_semicolon_opt 
 char_vertical_bar_expression
 char_vertical_bar_expression_opt
 clause
 clause_list
 comparison
 create
 cypher
 decimal_integer
 delete
 detach_opt
 distinct_opt
 double_literal
 expression
 expression_commalist
 expression_commalist_opt
 expression_opt
 expression_10
 expression_10_addon
 expression_10_addon_list
 expression_10_addon_list_opt
 expression_11
 expression_11_addon
 expression_11_addon_list
 expression_11_addon_list_opt
 expression_12
 expression_12_addon
 expression_12_addon_list
 expression_12_addon_list_opt
 expression_2
 expression_2_addon
 expression_2_addon_list
 expression_2_addon_list_opt
 expression_3
 expression_3_addon
 expression_3_addon_list
 expression_3_addon_list_opt
 expression_4
 expression_4_addon
 expression_4_addon_list
 expression_4_addon_list_opt
 expression_5
 expression_5_addon
 expression_5_addon_list
 expression_5_addon_list_opt
 expression_6
 expression_6_addon
 expression_6_addon_list
 expression_6_addon_list_opt
 expression_7
 expression_7_addon
 expression_7_addon_list
 expression_7_addon_list_opt
 expression_8
 expression_8_addon
 expression_8_addon_list
 expression_8_addon_list_opt
 expression_9
 expression_9_addon
 expression_9_addon_list
 expression_9_addon_list_opt
 filter_expression
 function_invocation
 function_name
 hex_integer
 id_in_coll
 label_name
 limit
 limit_opt
 list_comprehension
 map_literal
 match
 merge
 merge_action
 merge_action_list
 merge_action_list_opt
 node_label
 node_labels
 node_pattern
 number_literal
 octal_integer
 optional_opt
 order
 order_opt
 parameter
 parenthesized_expression
 partial_comparison_expression
 pattern
 pattern_element
 pattern_element_chain
 pattern_element_chain_list
 pattern_element_chain_list_opt
 pattern_part
 pattern_part_commalist
 properties
 property_expression
 property_key_name
 property_key_name_expression
 property_key_name_expression_commalist
 property_key_name_expression_commalist_opt
 property_lookup
 property_lookup_list
 query
 range_literal
 range_opt
 regular_query
 rel_type_name
 relationship_detail
 relationship_detail_opt
 relationship_pattern
 relationship_types
 relationship_types_opt
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
 signed_integer_literal
 single_query
 skip
 skip_opt
 sort_item
 sort_item_commalist
 statement
 symbolic_name
 union
 union_list
 union_list_opt
 unsigned_integer_literal
 unsigned_integer_literal_opt
 unwind
 variable
 where
 where_opt
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
 DELETE
 DESC
 DESCENDING
 DETACH
 DIGIT_STRING
 DISTINCT
 ENDS
 ESCAPED_SYMBOLIC_NAME
 EXISTS
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
 ON
 OPTIONAL
 OR
 ORDER
 REMOVE
 RETURN
 SET
 SIGNED_DECIMAL_INTEGER
 SIGNED_OCTAL_INTEGER
 SIGNED_REGULAR_DECIMAL_REAL
 SINGLE
 SKIP
 STARTS
 STRING_LITERAL
 TRUE
 UNESCAPED_SYMBOLIC_NAME
 UNION
 UNSIGNED_DECIMAL_INTEGER
 UNSIGNED_OCTAL_INTEGER
 UNSIGNED_REGULAR_DECIMAL_REAL
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
 '<-->'
 '<--'
 '-->'
 '--'
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

Left        110 'OR'.
Left        120 'XOR'.
Left        130 'AND'.
Left        140 'NOT'.
Nonassoc    200 '=' '<>' '!=' '<' '>' '<=' '>='.        %% comparison.
Left        300 '+' '-'.
Left        400 '*' '/' '%'.
Left        500 '^'.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Grammar rules.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cypher -> statement char_semicolon_opt                                                          : {cypher, {statement, '$1'}, '$2'}.

%% =====================================================================================================================
%% Helper definitions.
%% ---------------------------------------------------------------------------------------------------------------------
char_semicolon_opt -> '$empty'                                                                  : [].
char_semicolon_opt -> ';'                                                                       : ";".
%% =====================================================================================================================

statement -> query                                                                              : '$1'.

query -> regular_query                                                                          : {query, '$1'}.

regular_query -> single_query union_list_opt                                                    : {regularQuery, '$1', '$2'}.

%% =====================================================================================================================
%% Helper definitions.
%% ---------------------------------------------------------------------------------------------------------------------
union_list_opt -> '$empty'                                                                      : [].
union_list_opt -> union_list                                                                    : '$1'.

union_list -> union_list union                                                                  : '$1' ++ ['$2'].
union_list -> union                                                                             : ['$1'].
%% =====================================================================================================================

single_query -> clause_list                                                                     : {singleQuery, '$1'}.

%% =====================================================================================================================
%% Helper definitions.
%% ---------------------------------------------------------------------------------------------------------------------
clause_list -> clause_list clause                                                               : '$1' ++ ['$2'].
clause_list -> clause                                                                           : ['$1'].
%% =====================================================================================================================

union -> UNION all_opt single_query                                                             : {union, '$2', '$3'}.

%% =====================================================================================================================
%% Helper definitions.
%% ---------------------------------------------------------------------------------------------------------------------
all_opt -> '$empty'                                                                             : [].
all_opt -> ALL                                                                                  : "all".
%% =====================================================================================================================

clause -> match                                                                                 : {clause, '$1'}.
clause -> unwind                                                                                : {clause, '$1'}.
clause -> merge                                                                                 : {clause, '$1'}.
clause -> create                                                                                : {clause, '$1'}.
clause -> set                                                                                   : {clause, '$1'}.
clause -> delete                                                                                : {clause, '$1'}.
clause -> remove                                                                                : {clause, '$1'}.
clause -> with                                                                                  : {clause, '$1'}.
clause -> return                                                                                : {clause, '$1'}.

match -> optional_opt MATCH pattern where_opt                                                   : {match, '$1', '$3', '$4'}.

%% =====================================================================================================================
%% Helper definitions.
%% ---------------------------------------------------------------------------------------------------------------------
optional_opt -> '$empty'                                                                        : [].
optional_opt -> OPTIONAL                                                                        : "optional".

where_opt -> '$empty'                                                                           : {}.
where_opt -> where                                                                              : '$1'.
%% =====================================================================================================================

unwind -> UNWIND expression AS variable                                                         : {unwind, '$2', '$4'}.

merge -> MERGE pattern_part merge_action_list_opt                                               : {merge, '$2', '$3'}.

%% =====================================================================================================================
%% Helper definitions.
%% ---------------------------------------------------------------------------------------------------------------------
merge_action_list_opt -> '$empty'                                                               : [].
merge_action_list_opt -> merge_action_list                                                      : '$1'.

merge_action_list -> merge_action_list merge_action                                             : '$1' ++ ['$2'].
merge_action_list -> merge_action                                                               : ['$1'].
%% ---------------------------------------------------------------------------------------------------------------------

merge_action -> ON MATCH set                                                                    : {mergeAction, "match", '$3'}.
merge_action -> ON CREATE set                                                                   : {mergeAction, "create", '$3'}.

create -> CREATE pattern                                                                        : {create, '$2'}.

set -> SET set_item_commalist                                                                   : {set, '$2'}.

%% =====================================================================================================================
%% Helper definitions.
%% ---------------------------------------------------------------------------------------------------------------------
set_item_commalist -> set_item                                                                  : ['$1'].
set_item_commalist -> set_item ',' set_item_commalist                                           : ['$1' | '$3'].
%% ---------------------------------------------------------------------------------------------------------------------

set_item -> property_expression '=' expression                                                  : {setItem, '$1', "=", '$3'}.
set_item -> variable '=' expression                                                             : {setItem, '$1', "=", '$3'}.
set_item -> variable '+=' expression                                                            : {setItem, '$1', "+=", '$3'}.
set_item -> variable node_labels                                                                : {setItem, '$1', {nodeLabels, '$2'}}.

delete -> detach_opt DELETE expression_commalist                                                : {delete, '$1', '$3'}.

%% =====================================================================================================================
%% Helper definitions.
%% ---------------------------------------------------------------------------------------------------------------------
detach_opt -> '$empty'                                                                          : [].
detach_opt -> DETACH                                                                            : "detach".

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

remove_item -> variable node_labels                                                             : {removeItem, '$1', {nodeLabels, '$2'}}.
remove_item -> property_expression                                                              : {removeItem, '$1'}.

with -> WITH distinct_opt return_body where_opt                                                 : {with, '$2', '$3', '$4'}.

%% =====================================================================================================================
%% Helper definitions.
%% ---------------------------------------------------------------------------------------------------------------------
distinct_opt -> '$empty'                                                                        : [].
distinct_opt -> DISTINCT                                                                        : "distinct".
%% =====================================================================================================================

return -> RETURN distinct_opt return_body                                                       : {return, '$2', '$3'}.

return_body -> return_items order_opt skip_opt limit_opt                                        : {returnBody, '$1', '$2', '$3', '$4'}.

%% =====================================================================================================================
%% Helper definitions.
%% ---------------------------------------------------------------------------------------------------------------------
order_opt -> '$empty'                                                                           : {}.
order_opt -> order                                                                              : '$1'.

skip_opt -> '$empty'                                                                            : {}.
skip_opt -> skip                                                                                : '$1'.

limit_opt -> '$empty'                                                                           : {}.
limit_opt -> limit                                                                              : '$1'.
%% =====================================================================================================================

return_items -> '*'                                                                             : {returnItems, "*", []}.
return_items -> '*' ',' return_item_commalist                                                   : {returnItems, "*", '$3'}.
return_items -> return_item_commalist                                                           : {returnItems, [], '$1'}.

%% =====================================================================================================================
%% Helper definitions.
%% ---------------------------------------------------------------------------------------------------------------------
return_item_commalist -> return_item                                                            : ['$1'].
return_item_commalist -> return_item ',' return_item_commalist                                  : ['$1' | '$3'].
%% ---------------------------------------------------------------------------------------------------------------------

return_item -> expression AS variable                                                           : {returnItem, '$1', '$3'}.
return_item -> expression                                                                       : {returnItem, '$1'}.

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
sort_item -> expression                                                                         : {sortItem, '$1'}.

where -> WHERE expression                                                                       : {where, '$2'}.

pattern -> pattern_part_commalist                                                               : {pattern, '$1'}.

%% =====================================================================================================================
%% Helper definitions.
%% ---------------------------------------------------------------------------------------------------------------------
pattern_part_commalist -> pattern_part                                                          : ['$1'].
pattern_part_commalist -> pattern_part ',' pattern_part_commalist                               : ['$1' | '$3'].
%% =====================================================================================================================

pattern_part -> variable '=' anonymous_pattern_part                                             : {patternPart, '$1', '$3'}.
pattern_part -> anonymous_pattern_part                                                          : {patternPart, '$1'}.

anonymous_pattern_part -> pattern_element                                                       : {anonymousPatternPart, '$1'}.

pattern_element -> node_pattern pattern_element_chain_list_opt                                  : {patternElement, '$1', '$2'}.
pattern_element -> '(' pattern_element ')'                                                      : {patternElement, '$2'}.

%% =====================================================================================================================
%% Helper definitions.
%% ---------------------------------------------------------------------------------------------------------------------
pattern_element_chain_list_opt -> '$empty'                                                      : [].
pattern_element_chain_list_opt -> pattern_element_chain_list                                    : '$1'.

pattern_element_chain_list -> pattern_element_chain_list pattern_element_chain                  : '$1' ++ ['$2'].
pattern_element_chain_list -> pattern_element_chain                                             : ['$1'].
%% =====================================================================================================================

node_pattern -> '(' variable node_labels properties ')'                                         : {nodePattern, '$2', {nodeLabels, '$3'}, '$4'}.
node_pattern -> '(' variable node_labels ')'                                                    : {nodePattern, '$2', {nodeLabels, '$3'}, {}}.
node_pattern -> '(' variable properties ')'                                                     : {nodePattern, '$2', {},                 '$3'}.
node_pattern -> '(' variable ')'                                                                : {nodePattern, '$2', {},                 {}}.
node_pattern -> '(' node_labels properties ')'                                                  : {nodePattern, {},   {nodeLabels, '$2'}, '$3'}.
node_pattern -> '(' node_labels ')'                                                             : {nodePattern, {},   {nodeLabels, '$2'}, {}}.
node_pattern -> '(' properties ')'                                                              : {nodePattern, {},   {},                 '$2'}.
node_pattern -> '('  ')'                                                                        : {nodePattern, {},   {},                 {}}.

pattern_element_chain -> relationship_pattern node_pattern                                      : {patternElementChain, '$1', '$2'}.

relationship_pattern -> '<-->'                                                                  : {relationshipPattern, "<-->"}.
relationship_pattern -> '<' '-' relationship_detail_opt '-' '>'                                 : {relationshipPattern, "<", "-", '$3', "-", ">"}.
relationship_pattern -> '<--'                                                                   : {relationshipPattern, "<--"}.
relationship_pattern -> '<' '-' relationship_detail_opt '-'                                     : {relationshipPattern, "<", "-", '$3', "-", []}.
relationship_pattern ->  '-->'                                                                  : {relationshipPattern, "-->"}.
relationship_pattern ->     '-' relationship_detail_opt '-' '>'                                 : {relationshipPattern, [],  "-", '$2', "-", ">"}.
relationship_pattern ->  '--'                                                                   : {relationshipPattern, "--"}.
relationship_pattern ->     '-' relationship_detail_opt '-'                                     : {relationshipPattern, [],  "-", '$2', "-", []}.

%% =====================================================================================================================
%% Helper definitions.
%% ---------------------------------------------------------------------------------------------------------------------
relationship_detail_opt -> '$empty'                                                             : {}.
relationship_detail_opt -> relationship_detail                                                  : '$1'.
%% =====================================================================================================================

relationship_detail -> '[' variable char_question_mark_opt relationship_types_opt range_opt properties ']'     
                                                                                                : {relationshipDetail, '$2', '$3', '$4', '$5', '$6'}.

relationship_detail -> '[' variable char_question_mark_opt relationship_types_opt range_opt ']'     
                                                                                                : {relationshipDetail, '$2', '$3', '$4', '$5', {}}.
relationship_detail -> '[' char_question_mark_opt relationship_types_opt range_opt properties ']'     
                                                                                                : {relationshipDetail, {}, '$2', '$3', '$4', '$5'}.
relationship_detail -> '[' char_question_mark_opt relationship_types_opt range_opt ']'          : {relationshipDetail, {}, '$2', '$3', '$4', {}}.

%% =====================================================================================================================
%% Helper definitions.
%% ---------------------------------------------------------------------------------------------------------------------
char_question_mark_opt -> '$empty'                                                              : [].
char_question_mark_opt -> '?'                                                                   : "?".

relationship_types_opt -> '$empty'                                                              : [].
relationship_types_opt -> relationship_types                                                    : {relationshipTypes, '$1'}.

range_opt -> '$empty'                                                                           : {}.
range_opt -> '*' range_literal                                                                  : '$2'.
%% =====================================================================================================================

properties -> map_literal                                                                       : {properties, '$1'}.
properties -> parameter                                                                         : {properties, '$1'}.

relationship_types -> ':' rel_type_name                                                         : ['$2'].
relationship_types -> ':' rel_type_name '|' relationship_types                                  : ['$2' | '$4'].

node_labels -> node_labels node_label                                                           : '$1' ++ ['$2'].
node_labels -> node_label                                                                       : ['$1'].

node_label -> ':' label_name                                                                    : {nodeLabel, '$2'}.

range_literal -> unsigned_integer_literal_opt '..' unsigned_integer_literal_opt                 : {rangeLiteral, '$1', '$3'}.

%% =====================================================================================================================
%% Helper definitions.
%% ---------------------------------------------------------------------------------------------------------------------
unsigned_integer_literal_opt -> '$empty'                                                        : {}.
unsigned_integer_literal_opt -> unsigned_integer_literal                                        : '$1'.
%% =====================================================================================================================

label_name -> symbolic_name                                                                     : {labelName, '$1'}.

rel_type_name -> symbolic_name                                                                  : {relTypeName, '$1'}.

expression -> expression_12                                                                     : {expression, '$1'}.

expression_12 -> expression_11 expression_12_addon_list_opt                                     : {expression12, '$1', '$2'}.

%% =====================================================================================================================
%% Helper definitions.
%% ---------------------------------------------------------------------------------------------------------------------
expression_12_addon_list_opt -> '$empty'                                                        : [].
expression_12_addon_list_opt -> expression_12_addon_list                                        : '$1'.

expression_12_addon_list -> expression_12_addon_list expression_12_addon                        : '$1' ++ ['$2'].
expression_12_addon_list -> expression_12_addon                                                 : ['$1'].

expression_12_addon -> OR expression_11                                                         : {"or", '$2'}.
%% =====================================================================================================================

expression_11 -> expression_10 expression_11_addon_list_opt                                     : {expression11, '$1', '$2'}.

%% =====================================================================================================================
%% Helper definitions.
%% ---------------------------------------------------------------------------------------------------------------------
expression_11_addon_list_opt -> '$empty'                                                        : [].
expression_11_addon_list_opt -> expression_11_addon_list                                        : '$1'.

expression_11_addon_list -> expression_11_addon_list expression_11_addon                        : '$1' ++ ['$2'].
expression_11_addon_list -> expression_11_addon                                                 : ['$1'].

expression_11_addon -> XOR expression_10                                                         : {"xor", '$2'}.
%% =====================================================================================================================

expression_10 -> expression_9 expression_10_addon_list_opt                                      : {expression10, '$1', '$2'}.

%% =====================================================================================================================
%% Helper definitions.
%% ---------------------------------------------------------------------------------------------------------------------
expression_10_addon_list_opt -> '$empty'                                                        : [].
expression_10_addon_list_opt -> expression_10_addon_list                                        : '$1'.

expression_10_addon_list -> expression_10_addon_list expression_10_addon                        : '$1' ++ ['$2'].
expression_10_addon_list -> expression_10_addon                                                 : ['$1'].

expression_10_addon -> AND expression_9                                                         : {"and", '$2'}.
%% =====================================================================================================================

expression_9 -> expression_9_addon_list_opt expression_8                                        : {expression9, '$2', '$1'}.

%% =====================================================================================================================
%% Helper definitions.
%% ---------------------------------------------------------------------------------------------------------------------
expression_9_addon_list_opt -> '$empty'                                                         : [].
expression_9_addon_list_opt -> expression_9_addon_list                                          : '$1'.

expression_9_addon_list -> expression_9_addon_list expression_9_addon                           : '$1' ++ ['$2'].
expression_9_addon_list -> expression_9_addon                                                   : ['$1'].

expression_9_addon -> NOT                                                                       : {"not"}.
%% =====================================================================================================================

expression_8 -> expression_7 expression_8_addon_list_opt                                        : {expression8, '$1', '$2'}.

%% =====================================================================================================================
%% Helper definitions.
%% ---------------------------------------------------------------------------------------------------------------------
expression_8_addon_list_opt -> '$empty'                                                         : [].
expression_8_addon_list_opt -> expression_8_addon_list                                          : '$1'.

expression_8_addon_list -> expression_8_addon_list expression_8_addon                           : '$1' ++ ['$2'].
expression_8_addon_list -> expression_8_addon                                                   : ['$1'].

expression_8_addon -> partial_comparison_expression                                             : '$1'.
%% =====================================================================================================================

expression_7 -> expression_6 expression_7_addon_list_opt                                        : {expression7, '$1', '$2'}.

%% =====================================================================================================================
%% Helper definitions.
%% ---------------------------------------------------------------------------------------------------------------------
expression_7_addon_list_opt -> '$empty'                                                         : [].
expression_7_addon_list_opt -> expression_7_addon_list                                          : '$1'.

expression_7_addon_list -> expression_7_addon_list expression_7_addon                           : '$1' ++ ['$2'].
expression_7_addon_list -> expression_7_addon                                                   : ['$1'].

expression_7_addon -> '+' expression_6                                                          : {"+", '$2'}.
expression_7_addon -> '-' expression_6                                                          : {"-", '$2'}.
%% =====================================================================================================================

expression_6 -> expression_5 expression_6_addon_list_opt                                        : {expression6, '$1', '$2'}.

%% =====================================================================================================================
%% Helper definitions.
%% ---------------------------------------------------------------------------------------------------------------------
expression_6_addon_list_opt -> '$empty'                                                         : [].
expression_6_addon_list_opt -> expression_6_addon_list                                          : '$1'.

expression_6_addon_list -> expression_6_addon_list expression_6_addon                           : '$1' ++ ['$2'].
expression_6_addon_list -> expression_6_addon                                                   : ['$1'].

expression_6_addon -> '*' expression_5                                                          : {"*", '$2'}.
expression_6_addon -> '/' expression_5                                                          : {"/", '$2'}.
expression_6_addon -> '%' expression_5                                                          : {"%", '$2'}.
%% =====================================================================================================================

expression_5 -> expression_4 expression_5_addon_list_opt                                        : {expression5, '$1', '$2'}.

%% =====================================================================================================================
%% Helper definitions.
%% ---------------------------------------------------------------------------------------------------------------------
expression_5_addon_list_opt -> '$empty'                                                         : [].
expression_5_addon_list_opt -> expression_5_addon_list                                          : '$1'.

expression_5_addon_list -> expression_5_addon_list expression_5_addon                           : '$1' ++ ['$2'].
expression_5_addon_list -> expression_5_addon                                                   : ['$1'].

expression_5_addon -> '^' expression_4                                                          : {"^", '$2'}.
%% =====================================================================================================================

expression_4 -> expression_4_addon_list_opt expression_3                                        : {expression4, '$2', '$1'}.

%% =====================================================================================================================
%% Helper definitions.
%% ---------------------------------------------------------------------------------------------------------------------
expression_4_addon_list_opt -> '$empty'                                                         : [].
expression_4_addon_list_opt -> expression_4_addon_list                                          : '$1'.

expression_4_addon_list -> expression_4_addon_list expression_4_addon                           : '$1' ++ ['$2'].
expression_4_addon_list -> expression_4_addon                                                   : ['$1'].

expression_4_addon -> '+'                                                                       : {"+"}.
expression_4_addon -> '-'                                                                       : {"-"}.
%% =====================================================================================================================

expression_3 -> expression_2 expression_3_addon_list_opt                                        : {expression3, '$1', '$2'}.

%% =====================================================================================================================
%% Helper definitions.
%% ---------------------------------------------------------------------------------------------------------------------
expression_3_addon_list_opt -> '$empty'                                                         : [].
expression_3_addon_list_opt -> expression_3_addon_list                                          : '$1'.

expression_3_addon_list -> expression_3_addon_list expression_3_addon                           : '$1' ++ ['$2'].
expression_3_addon_list -> expression_3_addon                                                   : ['$1'].

expression_3_addon -> '[' expression_opt '..' expression_opt ']'                                : {"[", '$2', '$4'}.
expression_3_addon -> '[' expression ']'                                                        : {"[", '$2'}.
expression_3_addon -> '=~' expression_2                                                         : {"=~", '$2'}.
expression_3_addon -> IN expression_2                                                           : {"in", '$2'}.
expression_3_addon -> STARTS WITH expression_2                                                  : {"starts with", '$3'}.
expression_3_addon -> ENDS WITH expression_2                                                    : {"ends with", '$3'}.
expression_3_addon -> CONTAINS expression_2                                                     : {"contains", '$2'}.
expression_3_addon -> IS NOT NULL                                                               : {"is not null"}.
expression_3_addon -> IS NULL                                                                   : {"is null"}.

expression_opt -> '$empty'                                                                      : {}.
expression_opt -> expression                                                                    : '$1'.
%% =====================================================================================================================

expression_2 -> atom expression_2_addon_list_opt                                                : {expression2, '$1', '$2'}.

%% =====================================================================================================================
%% Helper definitions.
%% ---------------------------------------------------------------------------------------------------------------------
expression_2_addon_list_opt -> '$empty'                                                         : [].
expression_2_addon_list_opt -> expression_2_addon_list                                          : '$1'.

expression_2_addon_list -> expression_2_addon_list expression_2_addon                           : '$1' ++ ['$2'].
expression_2_addon_list -> expression_2_addon                                                   : ['$1'].

expression_2_addon -> node_labels                                                               : {nodeLabels, '$1'}.
expression_2_addon -> property_lookup                                                           : '$1'.
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
atom -> '[' expression_commalist ']'                                                            : {atom, '$2', "]"}.
atom -> FILTER '(' filter_expression ')'                                                        : {atom, {'filter', '$3'}}.
atom -> EXTRACT '(' filter_expression '|' expression ')'                                        : {atom, {'extract', '$3', '$5'}}.
atom -> EXTRACT '(' filter_expression ')'                                                       : {atom, {'extract', '$3'}}.
atom -> ALL '(' filter_expression ')'                                                           : {atom, {'all', '$3'}}.
atom -> ANY '(' filter_expression ')'                                                           : {atom, {'any', '$3'}}.
atom -> NONE '(' filter_expression ')'                                                          : {atom, {'none', '$3'}}.
atom -> SINGLE '(' filter_expression ')'                                                        : {atom, {'single', '$3'}}.
atom -> relationships_pattern                                                                   : {atom, '$1'}.
atom -> parenthesized_expression                                                                : {atom, '$1'}.
atom -> function_invocation                                                                     : {atom, '$1'}.
atom -> variable                                                                                : {atom, '$1'}.

partial_comparison_expression -> comparison expression_7                                        : {partialComparisonExpression, '$2', '$1'}.

%% =====================================================================================================================
%% Helper definitions.
%% ---------------------------------------------------------------------------------------------------------------------
comparison -> '='                                                                               : "=".
comparison -> '<>'                                                                              : "<>".
comparison -> '!='                                                                              : "!=".
comparison -> '<'                                                                               : "<".
comparison -> '>'                                                                               : ">".
comparison -> '<='                                                                              : "<=".
comparison -> '>='                                                                              : ">=".
%% =====================================================================================================================

parenthesized_expression -> '(' expression ')'                                                  : {parenthesizedExpression, '$2'}.

relationships_pattern -> node_pattern pattern_element_chain_list                                : {relationshipsPattern, '$1', '$2'}.

filter_expression -> id_in_coll where_opt                                                       : {filterExpression, '$1', '$2'}.

id_in_coll -> variable IN expression                                                            : {idInColl, '$1', '$3'}.

function_invocation -> function_name '(' distinct_opt expression_commalist_opt ')'              : {functionInvocation, '$1', '$4', '$3'}.

%% =====================================================================================================================
%% Helper definitions.
%% ---------------------------------------------------------------------------------------------------------------------
function_invocation -> COUNT  '(' distinct_opt expression_commalist_opt ')'                     : {functionInvocation, {functionName, {symbolicName, "count"}}, '$4', '$3'}.
function_invocation -> EXISTS '(' distinct_opt expression_commalist_opt ')'                     : {functionInvocation, {functionName, {symbolicName, "exists"}}, '$4', '$3'}.

expression_commalist_opt -> '$empty'                                                            : [].
expression_commalist_opt -> expression_commalist                                                : '$1'.
%% =====================================================================================================================

function_name -> symbolic_name                                                                  : {functionName, '$1'}.

list_comprehension -> '[' filter_expression char_vertical_bar_expression_opt ']'                : {listComprehension, '$2', '$3'}.

%% =====================================================================================================================
%% Helper definitions.
%% ---------------------------------------------------------------------------------------------------------------------
char_vertical_bar_expression_opt -> '$empty'                                                    : {}.
char_vertical_bar_expression_opt -> char_vertical_bar_expression                                : '$1'.

char_vertical_bar_expression -> '|' expression                                                  : '$2'.
%% =====================================================================================================================

property_lookup -> '.' property_key_name char_opt                                               : {propertyLookup, '$2', '$3'}.

%% =====================================================================================================================
%% Helper definitions.
%% ---------------------------------------------------------------------------------------------------------------------
char_opt -> '$empty'                                                                            : [].
char_opt -> '?'                                                                                 : "?".
char_opt -> '!'                                                                                 : "!".
%% =====================================================================================================================

variable -> symbolic_name                                                                       : {variable, '$1'}.

number_literal -> double_literal                                                                : {numberLiteral, '$1'}.
number_literal -> signed_integer_literal                                                        : {numberLiteral, '$1'}.

map_literal -> '{' property_key_name_expression_commalist_opt '}'                               : {mapLiteral, '$2'}.

%% =====================================================================================================================
%% Helper definitions.
%% ---------------------------------------------------------------------------------------------------------------------
property_key_name_expression_commalist_opt -> '$empty'                                          : "{}".
property_key_name_expression_commalist_opt -> property_key_name_expression_commalist            : '$1'.

property_key_name_expression_commalist -> property_key_name_expression                          : ['$1'].
property_key_name_expression_commalist -> property_key_name_expression ',' property_key_name_expression_commalist
                                                                                                : ['$1' | '$3'].

property_key_name_expression -> property_key_name ':' expression                                : {'$1', '$3'}.
%% =====================================================================================================================

parameter -> '{' symbolic_name '}'                                                              : {parameter, '$2'}.
parameter -> '{' UNSIGNED_DECIMAL_INTEGER '}'                                                   : {parameter, unwrap('$2')}.

property_expression -> atom property_lookup_list                                                : {propertyExpression, '$1', '$2'}.

%% =====================================================================================================================
%% Helper definitions.
%% ---------------------------------------------------------------------------------------------------------------------
property_lookup_list -> property_lookup_list property_lookup                                    : '$1' ++ ['$2'].
property_lookup_list -> property_lookup                                                         : ['$1'].
%% =====================================================================================================================

property_key_name -> symbolic_name                                                              : {propertyKeyName, '$1'}.

signed_integer_literal -> hex_integer                                                           : {signedIntegerLiteral, '$1'}.
signed_integer_literal -> octal_integer                                                         : {signedIntegerLiteral, '$1'}.
signed_integer_literal -> decimal_integer                                                       : {signedIntegerLiteral, '$1'}.

unsigned_integer_literal -> UNSIGNED_DECIMAL_INTEGER                                            : {unsignedIntegerLiteral, unwrap('$1')}.

hex_integer -> HEX_INTEGER                                                                      : {hexInteger, unwrap('$1')}.

decimal_integer -> SIGNED_DECIMAL_INTEGER                                                       : {decimalInteger, unwrap('$1')}.
decimal_integer -> UNSIGNED_DECIMAL_INTEGER                                                     : {decimalInteger, unwrap('$1')}.

octal_integer -> SIGNED_OCTAL_INTEGER                                                           : {octalInteger, unwrap('$1')}.
octal_integer -> UNSIGNED_OCTAL_INTEGER                                                         : {octalInteger, unwrap('$1')}.

%% =====================================================================================================================
%% Helper definitions.
%% ---------------------------------------------------------------------------------------------------------------------
decimal_integer -> DIGIT_STRING                                                                 : unwrap('$1').
%% =====================================================================================================================

double_literal -> EXPONENT_DECIMAL_REAL                                                         : {doubleLiteral, unwrap('$1')}.
double_literal -> SIGNED_REGULAR_DECIMAL_REAL                                                   : {doubleLiteral, unwrap('$1')}.
double_literal -> UNSIGNED_REGULAR_DECIMAL_REAL                                                 : {doubleLiteral, unwrap('$1')}.

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
