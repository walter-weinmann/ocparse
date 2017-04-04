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
 add_or_subtract_expression_addon_list
 and_expression
 and_expression_addon_list
 anonymous_pattern_part
 atom
 boolean_literal
 clause
 clause_list
 comparison_expression
 comparison_expression_addon_list
 create
 cypher
 delete
 double_literal
 expression
 expression_commalist
 filter_expression
 function_invocation
 function_name
 integer_literal
 id_in_coll
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
 node_label
 node_label_list
 node_labels
 node_pattern
 not_expression
 not_expression_addon_list
 number_literal
 or_expression
 or_expression_addon_list
 order
 parameter
 parenthesized_expression
 partial_comparison_expression
 pattern
 pattern_comprehension
 pattern_element
 pattern_element_chain
 pattern_element_chain_list
 pattern_part
 pattern_part_commalist
 power_of_expression
 power_of_expression_addon_list
 properties
 property_expression
 property_key_name
 property_key_name_expression
 property_key_name_expression_commalist
 property_lookup
 property_lookup_list
 property_or_labels_expression
 property_or_labels_expression_addon_list
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
 string_list_null_operator_expression
 string_list_null_operator_expression_addon
 string_list_null_operator_expression_addon_list
 symbolic_name
 unary_add_or_subtract_expression
 unary_add_or_subtract_expression_addon
 unary_add_or_subtract_expression_addon_list
 union
 union_list
 unwind
 variable
 where
 with
 xor_expression
 xor_expression_addon_list
 .

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Terminals
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
 '$'
 '<-->'
 '<--'
 '-->'
 '--'
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
Left        410 unary_add_or_subtract_expression_addon.                                         % Unary operator

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Grammar rules.
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
cypher -> statement                                                                             : {cypher, '$1', []}.
cypher -> statement ';'                                                                         : {cypher, '$1', ";"}.

%% =============================================================================
%% Helper definitions - test purposes.
%% -----------------------------------------------------------------------------
% cypher -> pattern_element_chain                                                                 : '$1'.
%% =============================================================================

statement -> query                                                                              : {statement, '$1'}.

query -> regular_query                                                                          : {query, '$1'}.

regular_query -> single_query                                                                   : {regularQuery, '$1', []}.
regular_query -> single_query union_list                                                        : {regularQuery, '$1', '$2'}.

%% =============================================================================
%% Helper definitions.
%% -----------------------------------------------------------------------------
union_list ->            union                                                                  :         ['$1'].
union_list -> union_list union                                                                  : '$1' ++ ['$2'].
%% =============================================================================

single_query -> clause_list                                                                     : {singleQuery, '$1'}.

%% =============================================================================
%% Helper definitions.
%% -----------------------------------------------------------------------------
clause_list ->             clause                                                               :         ['$1'].
clause_list -> clause_list clause                                                               : '$1' ++ ['$2'].
%% =============================================================================

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

%% =============================================================================
%% Helper definitions.
%% -----------------------------------------------------------------------------
merge_action_list ->                   merge_action                                             :         ['$1'].
merge_action_list -> merge_action_list merge_action                                             : '$1' ++ ['$2'].
%% -----------------------------------------------------------------------------

merge_action -> ON MATCH  set                                                                   : {mergeAction, "match",  '$3'}.
merge_action -> ON CREATE set                                                                   : {mergeAction, "create", '$3'}.

create -> CREATE pattern                                                                        : {create, '$2'}.

set -> SET set_item_commalist                                                                   : {set, '$2'}.

%% =============================================================================
%% Helper definitions.
%% -----------------------------------------------------------------------------
set_item_commalist -> set_item                                                                  : ['$1'].
set_item_commalist -> set_item ',' set_item_commalist                                           : ['$1' | '$3'].
%% -----------------------------------------------------------------------------

set_item -> property_expression '='  expression                                                 : {setItem, '$1', "=",  '$3'}.
set_item -> variable            '='  expression                                                 : {setItem, '$1', "=",  '$3'}.
set_item -> variable            '+=' expression                                                 : {setItem, '$1', "+=", '$3'}.
set_item -> variable                 node_labels                                                : {setItem, '$1', [],   '$2'}.

delete ->        DELETE expression_commalist                                                    : {delete, [],       '$2'}.
delete -> DETACH DELETE expression_commalist                                                    : {delete, "detach", '$3'}.

%% =============================================================================
%% Helper definitions.
%% -----------------------------------------------------------------------------
expression_commalist -> expression                                                              : ['$1'].
expression_commalist -> expression ',' expression_commalist                                     : ['$1' | '$3'].
%% -----------------------------------------------------------------------------

remove -> REMOVE remove_item_commalist                                                          : {remove, '$2'}.

%% =============================================================================
%% Helper definitions.
%% -----------------------------------------------------------------------------
remove_item_commalist -> remove_item                                                            : ['$1'].
remove_item_commalist -> remove_item ',' remove_item_commalist                                  : ['$1' | '$3'].
%% -----------------------------------------------------------------------------

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

%% =============================================================================
%% Helper definitions.
%% -----------------------------------------------------------------------------
return_item_commalist -> return_item                                                            : ['$1'].
return_item_commalist -> return_item ',' return_item_commalist                                  : ['$1' | '$3'].
%% -----------------------------------------------------------------------------

return_item -> expression                                                                       : {returnItem, '$1', []}.
return_item -> expression AS variable                                                           : {returnItem, '$1', '$3'}.

order -> ORDER BY sort_item_commalist                                                           : {order, '$3'}.

%% =============================================================================
%% Helper definitions.
%% -----------------------------------------------------------------------------
sort_item_commalist -> sort_item                                                                : ['$1'].
sort_item_commalist -> sort_item ',' sort_item_commalist                                        : ['$1' | '$3'].
%% -----------------------------------------------------------------------------

skip -> SKIP expression                                                                         : {skip, '$2'}.

limit -> LIMIT expression                                                                       : {limit, '$2'}.

sort_item -> expression DESCENDING                                                              : {sortItem, '$1', "descending"}.
sort_item -> expression DESC                                                                    : {sortItem, '$1', "desc"}.
sort_item -> expression ASCENDING                                                               : {sortItem, '$1', "ascending"}.
sort_item -> expression ASC                                                                     : {sortItem, '$1', "asc"}.
sort_item -> expression                                                                         : {sortItem, '$1', []}.

where -> WHERE expression                                                                       : {where, '$2'}.

pattern -> pattern_part_commalist                                                               : {pattern, '$1'}.

%% =============================================================================
%% Helper definitions.
%% -----------------------------------------------------------------------------
pattern_part_commalist -> pattern_part                                                          : ['$1'].
pattern_part_commalist -> pattern_part ',' pattern_part_commalist                               : ['$1' | '$3'].
%% =============================================================================

pattern_part -> variable '=' anonymous_pattern_part                                             : {patternPart, '$1', '$3'}.
pattern_part ->              anonymous_pattern_part                                             : {patternPart, [],   '$1'}.

anonymous_pattern_part -> pattern_element                                                       : {anonymousPatternPart, '$1'}.

pattern_element -> node_pattern                                                                 : {patternElement, '$1', []}.
pattern_element -> node_pattern pattern_element_chain_list                                      : {patternElement, '$1', '$2'}.
pattern_element -> '(' pattern_element ')'                                                      : {patternElement, '$2', "("}.

%% =============================================================================
%% Helper definitions.
%% -----------------------------------------------------------------------------
pattern_element_chain_list ->                            pattern_element_chain                  :         ['$1'].
pattern_element_chain_list -> pattern_element_chain_list pattern_element_chain                  : '$1' ++ ['$2'].
%% =============================================================================

node_pattern -> '('                                 ')'                                         : {nodePattern, [],   [],   []}.
node_pattern -> '('          node_labels            ')'                                         : {nodePattern, [],   '$2', []}.
node_pattern -> '(' variable                        ')'                                         : {nodePattern, '$2', [],   []}.
node_pattern -> '(' variable node_labels            ')'                                         : {nodePattern, '$2', '$3', []}.
node_pattern -> '('                      properties ')'                                         : {nodePattern, [],   [],   '$2'}.
node_pattern -> '('          node_labels properties ')'                                         : {nodePattern, [],   '$2', '$3'}.
node_pattern -> '(' variable             properties ')'                                         : {nodePattern, '$2', [],   '$3'}.
node_pattern -> '(' variable node_labels properties ')'                                         : {nodePattern, '$2', '$3', '$4'}.

pattern_element_chain -> relationship_pattern node_pattern                                      : {patternElementChain, '$1', '$2'}.

relationship_pattern -> '<-->'                                                                  : {relationshipPattern, "<-->",[],   []}.
relationship_pattern -> '<--'                                                                   : {relationshipPattern, "<--", [],   []}.
relationship_pattern ->  '-->'                                                                  : {relationshipPattern, "-->", [],   []}.
relationship_pattern ->  '--'                                                                   : {relationshipPattern, "--",  [],   []}.
relationship_pattern -> '<' '-' relationship_detail '-' '>'                                     : {relationshipPattern, "<-",  '$3', "->"}.
relationship_pattern -> '<' '-' relationship_detail '-'                                         : {relationshipPattern, "<-",  '$3', "-"}.
relationship_pattern ->     '-' relationship_detail '-' '>'                                     : {relationshipPattern, "-",   '$2', "->"}.
relationship_pattern ->     '-' relationship_detail '-'                                         : {relationshipPattern, "-",   '$2', "-"}.

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

properties -> map_literal                                                                       : {properties, '$1'}.
properties -> parameter                                                                         : {properties, '$1'}.

relationship_types -> ':' rel_type_name                                                         : {relationshipTypes, '$2', []}.
relationship_types -> ':' rel_type_name rel_type_verticalbarlist                                : {relationshipTypes, '$2', '$3'}.

%% =============================================================================
%% Helper definitions.
%% -----------------------------------------------------------------------------
rel_type_verticalbarlist ->                          '|' ':' rel_type_name                      : [{'$3', ":"}].
rel_type_verticalbarlist ->                          '|'     rel_type_name                      : [{'$2', []}].
rel_type_verticalbarlist -> rel_type_verticalbarlist '|' ':' rel_type_name                      : '$1' ++ [{'$4', ":"}].
rel_type_verticalbarlist -> rel_type_verticalbarlist '|'     rel_type_name                      : '$1' ++ [{'$3', []}].
%% =============================================================================

node_labels -> node_label_list                                                                  : {nodeLabels, '$1'}.

%% =============================================================================
%% Helper definitions.
%% -----------------------------------------------------------------------------
node_label_list ->                 node_label                                                   :         ['$1'].
node_label_list -> node_label_list node_label                                                   : '$1' ++ ['$2'].
%% =============================================================================

node_label -> ':' label_name                                                                    : {nodeLabel, '$2'}.

range_literal -> '*'                                                                            : {rangeLiteral, [],   [],   []}.
range_literal -> '*'                 '..'                                                       : {rangeLiteral, [],   "..", []}.
range_literal -> '*'                 '..' integer_literal                                       : {rangeLiteral, [],   "..", '$3'}.
range_literal -> '*' integer_literal                                                            : {rangeLiteral, '$2', [],   []}.
range_literal -> '*' integer_literal '..'                                                       : {rangeLiteral, '$2', "..", []}.
range_literal -> '*' integer_literal '..' integer_literal                                       : {rangeLiteral, '$2', "..", '$4'}.

label_name -> symbolic_name                                                                     : {labelName, '$1'}.

rel_type_name -> symbolic_name                                                                  : {relTypeName, '$1'}.

expression -> or_expression                                                                     : {expression, '$1'}.

or_expression -> xor_expression                                                                 : {orExpression, '$1', []}.
or_expression -> xor_expression or_expression_addon_list                                        : {orExpression, '$1', '$2'}.

%% =============================================================================
%% Helper definitions.
%% -----------------------------------------------------------------------------
or_expression_addon_list ->                          OR xor_expression                          :         [{"or", '$2'}].
or_expression_addon_list -> or_expression_addon_list OR xor_expression                          : '$1' ++ [{"or", '$3'}].
%% =============================================================================

xor_expression -> and_expression                                                                : {xorExpression, '$1', []}.
xor_expression -> and_expression xor_expression_addon_list                                      : {xorExpression, '$1', '$2'}.

%% =============================================================================
%% Helper definitions.
%% -----------------------------------------------------------------------------
xor_expression_addon_list ->                           XOR and_expression                       :         [{"xor", '$2'}].
xor_expression_addon_list -> xor_expression_addon_list XOR and_expression                       : '$1' ++ [{"xor", '$3'}].
%% =============================================================================

and_expression -> not_expression                                                                : {andExpression, '$1', []}.
and_expression -> not_expression and_expression_addon_list                                      : {andExpression, '$1', '$2'}.

%% =============================================================================
%% Helper definitions.
%% -----------------------------------------------------------------------------
and_expression_addon_list ->                           AND not_expression                       :         [{"and", '$2'}].
and_expression_addon_list -> and_expression_addon_list AND not_expression                       : '$1' ++ [{"and", '$3'}].
%% =============================================================================

not_expression ->                           comparison_expression                               : {notExpression, '$1', []}.
not_expression -> not_expression_addon_list comparison_expression                               : {notExpression, '$2', '$1'}.

%% =============================================================================
%% Helper definitions.
%% -----------------------------------------------------------------------------
not_expression_addon_list ->                           NOT                                      :         [{"not"}].
not_expression_addon_list -> not_expression_addon_list NOT                                      : '$1' ++ [{"not"}].
%% =============================================================================

comparison_expression -> add_or_subtract_expression                                             : {comparisonExpression, '$1', []}.
comparison_expression -> add_or_subtract_expression comparison_expression_addon_list            : {comparisonExpression, '$1', '$2'}.

%% =============================================================================
%% Helper definitions.
%% -----------------------------------------------------------------------------
comparison_expression_addon_list ->                                  partial_comparison_expression       
                                                                                                :         ['$1'].
comparison_expression_addon_list -> comparison_expression_addon_list partial_comparison_expression
                                                                                                : '$1' ++ ['$2'].
%% =============================================================================

add_or_subtract_expression -> multiply_divide_modulo_expression                                 : {addOrSubtractExpression, '$1', []}.
add_or_subtract_expression -> multiply_divide_modulo_expression add_or_subtract_expression_addon_list
                                                                                                : {addOrSubtractExpression, '$1', '$2'}.

%% =============================================================================
%% Helper definitions.
%% -----------------------------------------------------------------------------
add_or_subtract_expression_addon_list ->                                       '+' multiply_divide_modulo_expression                             
                                                                                                :         [{"+", '$2'}].
add_or_subtract_expression_addon_list ->                                       '-' multiply_divide_modulo_expression                             
                                                                                                :         [{"-", '$2'}].
add_or_subtract_expression_addon_list -> add_or_subtract_expression_addon_list '+' multiply_divide_modulo_expression                             
                                                                                                : '$1' ++ [{"+", '$3'}].
add_or_subtract_expression_addon_list -> add_or_subtract_expression_addon_list '-' multiply_divide_modulo_expression                             
                                                                                                : '$1' ++ [{"-", '$3'}].
%% =============================================================================

multiply_divide_modulo_expression -> power_of_expression                                        : {multiplyDivideModuloExpression, '$1', []}.
multiply_divide_modulo_expression -> power_of_expression multiply_divide_modulo_expression_addon_list                                            
                                                                                                : {multiplyDivideModuloExpression, '$1', '$2'}.

%% =============================================================================
%% Helper definitions.
%% -----------------------------------------------------------------------------
multiply_divide_modulo_expression_addon_list ->                                              '*' power_of_expression
                                                                                                :         [{"*", '$2'}].
multiply_divide_modulo_expression_addon_list ->                                              '/' power_of_expression                             
                                                                                                :         [{"/", '$2'}].
multiply_divide_modulo_expression_addon_list ->                                              '%' power_of_expression
                                                                                                :         [{"%", '$2'}].
multiply_divide_modulo_expression_addon_list -> multiply_divide_modulo_expression_addon_list '*' power_of_expression
                                                                                                : '$1' ++ [{"*", '$3'}].
multiply_divide_modulo_expression_addon_list -> multiply_divide_modulo_expression_addon_list '/' power_of_expression                             
                                                                                                : '$1' ++ [{"/", '$3'}].
multiply_divide_modulo_expression_addon_list -> multiply_divide_modulo_expression_addon_list '%' power_of_expression                             
                                                                                                : '$1' ++ [{"%", '$3'}].
%% =============================================================================

power_of_expression -> unary_add_or_subtract_expression                                         : {powerOfExpression, '$1', []}.
power_of_expression -> unary_add_or_subtract_expression power_of_expression_addon_list          : {powerOfExpression, '$1', '$2'}.

%% =============================================================================
%% Helper definitions.
%% -----------------------------------------------------------------------------
power_of_expression_addon_list ->                                '^' unary_add_or_subtract_expression                      
                                                                                                :         [{"^", '$2'}].
power_of_expression_addon_list -> power_of_expression_addon_list '^' unary_add_or_subtract_expression               
                                                                                                : '$1' ++ [{"^", '$3'}].
%% =============================================================================

unary_add_or_subtract_expression ->                         string_list_null_operator_expression                                            
                                                                                                : {unaryAddOrSubtractExpression, '$1', []}.
unary_add_or_subtract_expression -> unary_add_or_subtract_expression_addon_list string_list_null_operator_expression                                            
                                                                                                : {unaryAddOrSubtractExpression, '$2', '$1'}.

%% =============================================================================
%% Helper definitions.
%% -----------------------------------------------------------------------------
unary_add_or_subtract_expression_addon_list ->                                             unary_add_or_subtract_expression_addon                           
                                                                                                :         ['$1'].
unary_add_or_subtract_expression_addon_list -> unary_add_or_subtract_expression_addon_list unary_add_or_subtract_expression_addon                           
                                                                                                : '$1' ++ ['$2'].

unary_add_or_subtract_expression_addon -> '+'                                                   : {"+"}.
unary_add_or_subtract_expression_addon -> '-'                                                   : {"-"}.
%% =============================================================================

string_list_null_operator_expression -> property_or_labels_expression                           : {stringListNullOperatorExpression, '$1', []}.
string_list_null_operator_expression -> property_or_labels_expression string_list_null_operator_expression_addon_list
                                                                                                : {stringListNullOperatorExpression, '$1', '$2'}.

%% =============================================================================
%% Helper definitions.
%% -----------------------------------------------------------------------------
string_list_null_operator_expression_addon_list ->                                                 string_list_null_operator_expression_addon                           
                                                                                                :         ['$1'].
string_list_null_operator_expression_addon_list -> string_list_null_operator_expression_addon_list string_list_null_operator_expression_addon                           
                                                                                                : '$1' ++ ['$2'].

string_list_null_operator_expression_addon -> '[' expression                 ']'                : {"[",           '$2'}.
string_list_null_operator_expression_addon -> '['            '..'            ']'                : {"[",           [],   []}.
string_list_null_operator_expression_addon -> '['            '..' expression ']'                : {"[",           [],   '$3'}.
string_list_null_operator_expression_addon -> '[' expression '..'            ']'                : {"[",           '$2', []}.
string_list_null_operator_expression_addon -> '[' expression '..' expression ']'                : {"[",           '$2', '$4'}.
string_list_null_operator_expression_addon -> '=~'        property_or_labels_expression         : {"=~",          '$2'}.
string_list_null_operator_expression_addon -> IN          property_or_labels_expression         : {"in",          '$2'}.
string_list_null_operator_expression_addon -> STARTS WITH property_or_labels_expression         : {"starts with", '$3'}.
string_list_null_operator_expression_addon -> ENDS WITH   property_or_labels_expression         : {"ends with",   '$3'}.
string_list_null_operator_expression_addon -> CONTAINS    property_or_labels_expression         : {"contains",    '$2'}.
string_list_null_operator_expression_addon -> IS NULL                                           : {"is null"}.
string_list_null_operator_expression_addon -> IS NOT NULL                                       : {"is not null"}.
%% =============================================================================

property_or_labels_expression -> atom                                                           : {propertyOrLabelsExpression, '$1', []}.
property_or_labels_expression -> atom property_or_labels_expression_addon_list                  : {propertyOrLabelsExpression, '$1', '$2'}.

%% =============================================================================
%% Helper definitions.
%% -----------------------------------------------------------------------------
property_or_labels_expression_addon_list ->                                          node_labels                                  
                                                                                                :         ['$1'].
property_or_labels_expression_addon_list ->                                          property_lookup                              
                                                                                                :         ['$1'].
property_or_labels_expression_addon_list -> property_or_labels_expression_addon_list node_labels                                  
                                                                                                : '$1' ++ ['$2'].
property_or_labels_expression_addon_list -> property_or_labels_expression_addon_list property_lookup
                                                                                                : '$1' ++ ['$2'].
%% =============================================================================

atom -> literal                                                                                 : {atom, '$1'}.
atom -> parameter                                                                               : {atom, '$1'}.
atom -> COUNT '(' '*' ')'                                                                       : {atom, {terminal, "count(*)"}}.
atom -> list_comprehension                                                                      : {atom, '$1'}.
atom -> pattern_comprehension                                                                   : {atom, '$1'}.
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

literal -> number_literal                                                                       : {literal, '$1'}.
literal -> STRING_LITERAL                                                                       : {literal, {stringLiteral, unwrap('$1')}}.
literal -> boolean_literal                                                                      : {literal, '$1'}.
literal -> NULL                                                                                 : {literal, {terminal, "null"}}.
literal -> map_literal                                                                          : {literal, '$1'}.
literal -> list_literal                                                                         : {literal, '$1'}.

boolean_literal -> TRUE                                                                         : {booleanLiteral, {terminal, "true"}}.
boolean_literal -> FALSE                                                                        : {booleanLiteral, {terminal, "false"}}.

list_literal -> '[' expression_commalist ']'                                                    : {listLiteral, '$2'}.

partial_comparison_expression -> '='  add_or_subtract_expression                                : {partialComparisonExpression, '$2', "="}.
partial_comparison_expression -> '<>' add_or_subtract_expression                                : {partialComparisonExpression, '$2', "<>"}.
partial_comparison_expression -> '!=' add_or_subtract_expression                                : {partialComparisonExpression, '$2', "!="}.
partial_comparison_expression -> '<'  add_or_subtract_expression                                : {partialComparisonExpression, '$2', "<"}.
partial_comparison_expression -> '>'  add_or_subtract_expression                                : {partialComparisonExpression, '$2', ">"}.
partial_comparison_expression -> '<=' add_or_subtract_expression                                : {partialComparisonExpression, '$2', "<="}.
partial_comparison_expression -> '>=' add_or_subtract_expression                                : {partialComparisonExpression, '$2', ">="}.

parenthesized_expression -> '(' expression ')'                                                  : {parenthesizedExpression, '$2'}.

relationships_pattern -> node_pattern pattern_element_chain_list                                : {relationshipsPattern, '$1', '$2'}.

filter_expression -> id_in_coll                                                                 : {filterExpression, '$1', []}.
filter_expression -> id_in_coll where                                                           : {filterExpression, '$1', '$2'}.

id_in_coll -> variable IN expression                                                            : {idInColl, '$1', '$3'}.

function_invocation -> function_name '('                               ')'                      : {functionInvocation, '$1', [],         []}.
function_invocation -> function_name '('          expression_commalist ')'                      : {functionInvocation, '$1', [],         '$3'}.
function_invocation -> function_name '(' DISTINCT                      ')'                      : {functionInvocation, '$1', "distinct", []}.
function_invocation -> function_name '(' DISTINCT expression_commalist ')'                      : {functionInvocation, '$1', "distinct", '$4'}.

%% =============================================================================
%% Helper definitions.
%% -----------------------------------------------------------------------------
function_invocation -> COUNT         '('                               ')'                     : {functionInvocation, {functionName, {symbolicName, "count"}}, [],         []}.
function_invocation -> COUNT         '('          expression_commalist ')'                     : {functionInvocation, {functionName, {symbolicName, "count"}}, [],         '$3'}.
function_invocation -> COUNT         '(' DISTINCT                      ')'                     : {functionInvocation, {functionName, {symbolicName, "count"}}, "distinct", []}.
function_invocation -> COUNT         '(' DISTINCT expression_commalist ')'                     : {functionInvocation, {functionName, {symbolicName, "count"}}, "distinct", '$4'}.
%% =============================================================================

function_name -> symbolic_name                                                                  : {functionName, '$1'}.

list_comprehension -> '[' filter_expression                ']'                                  : {listComprehension, '$2', []}.
list_comprehension -> '[' filter_expression '|' expression ']'                                  : {listComprehension, '$2', '$4'}.

pattern_comprehension -> '['              relationships_pattern                  '|' expression ']'
                                                                                                : {patternComprehension, [],   '$2', [],   '$4'}.
pattern_comprehension -> '['              relationships_pattern WHERE expression '|' expression ']'
                                                                                                : {patternComprehension, [],   '$2', '$4', '$6'}.
pattern_comprehension -> '[' variable '=' relationships_pattern                  '|' expression ']'
                                                                                                : {patternComprehension, '$2', '$4', [],   '$6'}.
pattern_comprehension -> '[' variable '=' relationships_pattern WHERE expression '|' expression ']'
                                                                                                : {patternComprehension, '$2', '$4', '$6', '$8'}.

property_lookup -> '.' property_key_name                                                        : {propertyLookup, '$2'}.

variable -> symbolic_name                                                                       : {variable, '$1'}.

number_literal -> double_literal                                                                : {numberLiteral, '$1'}.
number_literal -> integer_literal                                                               : {numberLiteral, '$1'}.

map_literal -> '{'                                        '}'                                   : {mapLiteral, []}.
map_literal -> '{' property_key_name_expression_commalist '}'                                   : {mapLiteral, '$2'}.

%% =============================================================================
%% Helper definitions.
%% -----------------------------------------------------------------------------
property_key_name_expression_commalist -> property_key_name_expression                          : ['$1'].
property_key_name_expression_commalist -> property_key_name_expression ',' property_key_name_expression_commalist
                                                                                                : ['$1' | '$3'].

property_key_name_expression -> property_key_name ':' expression                                : {'$1', '$3'}.
%% =============================================================================

parameter -> '$' symbolic_name                                                                  : {parameter, '$2'}.
parameter -> '$' DECIMAL_INTEGER                                                                : {parameter, unwrap('$2')}.

property_expression -> atom property_lookup_list                                                : {propertyExpression, '$1', '$2'}.

%% =============================================================================
%% Helper definitions.
%% -----------------------------------------------------------------------------
property_lookup_list ->                      property_lookup                                    :         ['$1'].
property_lookup_list -> property_lookup_list property_lookup                                    : '$1' ++ ['$2'].
%% =============================================================================

property_key_name -> symbolic_name                                                              : {propertyKeyName, '$1'}.

integer_literal -> HEX_INTEGER                                                                  : {integerLiteral, unwrap('$1')}.
integer_literal -> OCTAL_INTEGER                                                                : {integerLiteral, unwrap('$1')}.
integer_literal -> DECIMAL_INTEGER                                                              : {integerLiteral, unwrap('$1')}.

double_literal -> EXPONENT_DECIMAL_REAL                                                         : {doubleLiteral, unwrap('$1')}.
double_literal -> REGULAR_DECIMAL_REAL                                                          : {doubleLiteral, unwrap('$1')}.

symbolic_name -> ESCAPED_SYMBOLIC_NAME                                                          : {symbolicName, unwrap('$1')}.
symbolic_name -> UNESCAPED_SYMBOLIC_NAME                                                        : {symbolicName, unwrap('$1')}.

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
        {Cypher, null_fun = Ctx} ->
            list_to_binary(string:strip(Cypher));
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
        {Cypher, null_fun = Ctx} ->
            list_to_binary(string:strip(Cypher));
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
source_to_pt(Cypher0) ->
    Cypher = re:replace(Cypher0, "(^[ \r\n]+)|([ \r\n]+$)", "",
        [global, {return, list}]),
    [C | _] = lists:reverse(Cypher),
    NCypher = if C =:= $; ->
        Cypher;
                  true ->
                      string:strip(Cypher)
              end,
    case oclexer:string(NCypher) of
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
