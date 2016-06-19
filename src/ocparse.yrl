%% -*- erlang -*-
Header "%% Copyright (C) K2 Informatics GmbH"
"%% @private"
"%% @Author Walter Weinmann"
"%% @Email office@k2informatics.ch".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Nonterminals
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 any_cypher_option
 atom
 % bulk_import_query
 case_alternatives
 case_alternatives_list
 case_expression
 char_opt
 char_question_mark_opt
 char_semicolon_opt
 char_vertical_bar_expression
 char_vertical_bar_expression_opt
 clause
 command
 comparison
 configuration_option
 configuration_option_list
 configuration_option_list_opt
 create_index
 create_unique_constraint
 cypher
 cypher_option
 dash
 distinct_opt
 double_literal
 drop_index
 drop_unique_constraint
 else_expression
 else_expression_opt
 expression
 expression_commalist
 expression_commalist_opt
 expression_opt
 expression_10
 expression_11
 expression_12
 expression_2
 expression_2_addon
 expression_2_addon_list
 expression_2_addon_list_opt
 expression_3
 expression_4
 expression_5
 expression_6
 expression_7
 expression_8
 expression_9
 filter_expression
 function_invocation
 function_name
 id_in_coll
 index
 label_name
 left_arrow_head_opt
 list_comprehension
 map_literal
 node_label
 node_labels
 node_labels_opt
 node_pattern
 number_literal
 parameter
 parenthesized_expression
 partial_comparison_expression
 pattern_element
 pattern_element_chain
 pattern_element_chain_list
 pattern_element_chain_list_opt
 pattern_element_chain_opt
 properties
 properties_opt
 property_expression
 property_key_name
 property_key_name_expression
 property_key_name_expression_commalist
 property_key_name_expression_commalist_opt
 property_lookup
 property_lookup_list
 property_lookup_list_opt
 query
 query_options
 query_options_opt
 range_literal
 range_literal_opt
 range_opt
 reduce
 regular_decimal_real
 regular_query
 rel_type_name
 relationship_detail
 relationship_detail_opt
 relationship_pattern
 relationship_types
 relationship_types_opt
 relationships_pattern
 right_arrow_head_opt
 shortest_path_pattern
 signed_integer_literal
 single_query
 statement
 symbolic_name
 union
 union_opt
 unique_constraint
 unsigned_decimal_integer
 unsigned_integer_literal
 unsigned_integer_literal_opt
 variable
 variable_opt
 version_number
 version_number_opt
 where
 where_opt
 .

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Terminals
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 ALL
 ALLSHORTESTPATHS
 AND
 ANY
% AS
% ASC
% ASCENDING
 ASSERT
% BY
 CASE
% COMMIT
 CONSTRAINT
 CONTAINS
 COUNT
 CREATE
% CSV
 CYPHER
% DELETE
% DESC
% DESCENDING
% DETACH
 DISTINCT
 DROP
 ELSE
 END
 ENDS
% EXISTS
 EXPLAIN
 EXPONENT_DECIMAL_REAL
 EXTRACT
 FALSE
% FIELDTERMINATOR
 FILTER
% FOREACH
% FROM
% HEADERS
 HEX_INTEGER
 IN
 INDEX
 IS
% JOIN
% L_0X
% L_SKIP
% LIMIT
% LOAD
 MATCH
% MERGE
 NAME
% NODE
 NONE
 NOT
 NULL
 OCTAL_INTEGER
 ON
% OPTIONAL
 OR
% ORDER
% PERIODIC
 PROFILE
 REDUCE
% REL
% RELATIONSHIP
% REMOVE
% RETURN
% SCAN
% SET
 SHORTESTPATH
 SIGNED_DECIMAL_INTEGER
 SIGNED_FLOAT
 SINGLE
% START
 STARTS
 STRING_LITERAL
 THEN
 TRUE
 UNION
 UNIQUE
 UNSIGNED_DECIMAL_INTEGER
 UNSIGNED_FLOAT
% UNWIND
% USING
 WHEN
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
 '0'
 %% wwe '‑'                             %% dash
 %% wwe '‒'                             %% dash
 %% wwe '–'                             %% dash
 %% wwe '—'                            %% dash
 %% wwe '―'                            %% dash
 %% wwe '−'                            %% dash
 %% wwe '﹘'                            %% dash
 %% wwe '﹣'                            %% dash
 %% wwe '－'                            %% dash
 '--'                                   %% dash
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
%Left        600 UMINUS.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Grammar rules.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cypher -> query_options_opt statement char_semicolon_opt                                        : {cypher, '$1', {statement, '$2'}, '$3'}.

cypher -> expression ';'                                                                        : '$1'.

query_options_opt -> '$empty'                                                                   : {}.
query_options_opt -> query_options                                                              : {queryOptions, '$1'}.

char_semicolon_opt -> '$empty'                                                                  : {}.
char_semicolon_opt -> ';'                                                                       : ";".

query_options -> query_options any_cypher_option                                                : '$1' ++ ['$2'].
query_options -> any_cypher_option                                                              : ['$1'].

any_cypher_option -> EXPLAIN                                                                    : {anyCypherOption, explain, []}.
any_cypher_option -> PROFILE                                                                    : {anyCypherOption, profile, []}.
any_cypher_option -> cypher_option                                                              : {anyCypherOption, '$1'}.

cypher_option -> CYPHER version_number_opt configuration_option_list_opt                        : {cypherOption, '$2', '$3'}.

version_number_opt -> '$empty'                                                                  : {}.
version_number_opt -> version_number                                                            : '$1'.

configuration_option_list_opt -> '$empty'                                                       : [].
configuration_option_list_opt -> configuration_option_list                                      : '$1'.

version_number -> UNSIGNED_FLOAT                                                                : {versionNumber, unwrap('$1')}.

configuration_option_list -> configuration_option_list configuration_option                     : '$1' ++ ['$2'].
configuration_option_list -> configuration_option                                               : ['$1'].

configuration_option -> symbolic_name '=' symbolic_name                                         : {configurationOption, '$1', '$3'}.

statement -> command                                                                            : '$1'.
statement -> query                                                                              : '$1'.

query -> regular_query                                                                          : {query, '$1'}.
% wwe query -> bulk_import_query                                                                      : {query, '$1'}.

regular_query -> single_query union_opt                                                         : {regularQuery, '$1', '$2'}.

union_opt -> '$empty'                                                                           : {}.
union_opt -> union                                                                              : '$1'.

single_query -> clause                                                                          : {singleQuery, '$1'}.

union -> UNION ALL clause                                                                       : {union, '$3', '$2'}.
union -> UNION clause                                                                           : {union, '$2'}.

clause -> MATCH                                                                                 : {clause, match}.

command -> create_index                                                                         : {command, '$1'}.
command -> drop_index                                                                           : {command, '$1'}.
command -> create_unique_constraint                                                             : {command, '$1'}.
command -> drop_unique_constraint                                                               : {command, '$1'}.

create_unique_constraint -> CREATE unique_constraint                                            : {createUniqueConstraint, '$2'}.

create_index -> CREATE index                                                                    : {createIndex, '$2'}.

drop_unique_constraint -> DROP unique_constraint                                                : {dropUniqueConstraint, '$2'}.

drop_index -> DROP index                                                                        : {dropIndex, '$2'}.

index -> INDEX ON node_label '(' property_key_name ')'                                          : {index ,{'$3', '$5'}}.

unique_constraint -> CONSTRAINT ON '(' variable node_label ')' ASSERT property_expression IS UNIQUE
                                                                                                : {uniqueConstraint, '$4', '$5', '$8'}.

where -> WHERE expression                                                                       : {where, '$2'}.

shortest_path_pattern -> SHORTESTPATH '(' pattern_element ')'                                   : {shortestPathPattern, '$1', '$3'}.
shortest_path_pattern -> ALLSHORTESTPATHS '(' pattern_element ')'                               : {shortestPathPattern, '$1', '$3'}.

pattern_element -> node_pattern pattern_element_chain_opt                                       : {patternElement, '$1', '$2'}.
pattern_element -> '(' pattern_element ')'                                                      : {patternElement, '$2'}.

pattern_element_chain_opt -> '$empty'                                                           : {}.
pattern_element_chain_opt -> pattern_element_chain                                              : '$1'.

node_pattern -> '(' variable_opt node_labels_opt properties_opt ')'                             : {nodePattern, '$2', '$3', '$4'}.

variable_opt -> '$empty'                                                                        : {}.
variable_opt -> variable                                                                        : '$1'.

node_labels_opt -> '$empty'                                                                     : [].
node_labels_opt -> node_labels                                                                  : {nodeLabels, '$1'}.

properties_opt -> '$empty'                                                                      : {}.
properties_opt -> properties                                                                    : '$1'.

pattern_element_chain -> relationship_pattern node_pattern                                      : {patternElementChain, '$1', '$2'}.

relationship_pattern -> left_arrow_head_opt dash relationship_detail_opt dash right_arrow_head_opt
                                                                                                : {relationshipPattern, '$1', '$2', '$3', '$4', '$5'}.

left_arrow_head_opt -> '$empty'                                                                 : {}.
left_arrow_head_opt -> '<'                                                                      : {leftArrowHead, '$1'}.

dash -> '--'                                                                                    : {dash, '$1'}.
%% wwe dash -> '‐'                                                                                     : {dash, '$1'}.
%% wwe dash -> '‑'                                                                                     : {dash, '$1'}.
%% wwe dash -> '‒'                                                                                     : {dash, '$1'}.
%% wwe dash -> '–'                                                                                     : {dash, '$1'}.
%% wwe dash -> '—'                                                                                     : {dash, '$1'}.
%% wwe dash -> '―'                                                                                     : {dash, '$1'}.
%% wwe dash -> '−'                                                                                     : {dash, '$1'}.
%% wwe dash -> '﹘'                                                                                     : {dash, '$1'}.
%% wwe dash -> '﹣'                                                                                     : {dash, '$1'}.
%% wwe dash -> '－'                                                                                     : {dash, '$1'}.

relationship_detail_opt -> '$empty'                                                             : {}.
relationship_detail_opt -> relationship_detail                                                  : '$1'.

right_arrow_head_opt -> '$empty'                                                                : {}.
right_arrow_head_opt -> '>'                                                                     : {rightArrowHead, '$1'}.

relationship_detail -> '[' variable_opt char_question_mark_opt relationship_types_opt range_opt properties_opt ']'     
                                                                                                : {relationshipDetail, '$2', '$3', '$4', '$5', '$6'}.

char_question_mark_opt -> '$empty'                                                              : [].
char_question_mark_opt -> '?'                                                                   : "?".

relationship_types_opt -> '$empty'                                                              : [].
relationship_types_opt -> relationship_types                                                    : {relationshipTypes, '$1'}.

range_opt -> '$empty'                                                                           : {}.
range_opt -> '*' range_literal_opt                                                              : {"*", '$2'}.

range_literal_opt -> '$empty'                                                                   : {}.
range_literal_opt -> range_literal                                                              : '$1'.

properties -> map_literal                                                                       : {properties, '$1'}.
properties -> parameter                                                                         : {properties, '$1'}.

relationship_types -> ':' rel_type_name                                                         : ['$2'].
relationship_types -> ':' rel_type_name '|' relationship_types                                  : ['$2' | '$4'].

node_labels -> node_labels node_label                                                           : '$1' ++ ['$2'].
node_labels -> node_label                                                                       : ['$1'].

node_label -> ':' label_name                                                                    : {nodeLabel, '$2'}.

range_literal -> unsigned_integer_literal_opt '..' unsigned_integer_literal_opt                 : {rangeLiteral, '$1', '$3'}.

unsigned_integer_literal_opt -> '$empty'                                                        : {}.
unsigned_integer_literal_opt -> unsigned_integer_literal                                        : '$1'.

label_name -> symbolic_name                                                                     : {labelName, '$1'}.

rel_type_name -> symbolic_name                                                                  : {relTypeName, '$1'}.

expression -> expression_12                                                                     : {expression, '$1'}.

expression_12 -> expression_11 'OR' expression_11                                               : {expression12, '$1', '$2', '$3'}.
expression_12 -> expression_11                                                                  : {expression12, '$1'}.

expression_11 -> expression_10 'XOR' expression_10                                              : {expression11, '$1', '$2', '$3'}.
expression_11 -> expression_10                                                                  : {expression11, '$1'}.

expression_10 -> expression_9 'AND' expression_9                                                : {expression10, '$1', '$2', '$3'}.
expression_10 -> expression_9                                                                   : {expression10, '$1'}.

expression_9 -> NOT expression_8                                                                : {expression9, '$2', '$1'}.
expression_9 -> expression_8                                                                    : {expression9, '$1'}.

expression_8 -> expression_7 partial_comparison_expression                                      : {expression8, '$1', '$2'}.
expression_8 -> expression_7                                                                    : {expression8, '$1'}.

expression_7 -> expression_6 '+' expression_6                                                   : {expression7, '$1', "+", '$3'}.
expression_7 -> expression_6 '-' expression_6                                                   : {expression7, '$1', "-", '$3'}.
expression_7 -> expression_6                                                                    : {expression7, '$1'}.

expression_6 -> expression_5 '*' expression_5                                                   : {expression6, '$1', "*", '$3'}.
expression_6 -> expression_5 '/' expression_5                                                   : {expression6, '$1', "/", '$3'}.
expression_6 -> expression_5 '%' expression_5                                                   : {expression6, '$1', "%", '$3'}.
expression_6 -> expression_5                                                                    : {expression6, '$1'}.

expression_5 -> expression_4 '^' expression_4                                                   : {expression5, '$1', "^", '$3'}.
expression_5 -> expression_4                                                                    : {expression5, '$1'}.

expression_4 -> '+' expression_3                                                                : {expression4, '$2', "+"}.
expression_4 -> '-' expression_3                                                                : {expression4, '$2', "-"}.
expression_4 -> expression_3                                                                    : {expression4, '$1'}.

expression_3 -> expression_2 '[' expression '..' expression ']'                                 : {expression3, '$1', "[", '$3', '$5'}.
expression_3 -> expression_2 '[' expression ']'                                                 : {expression3, '$1', "[", '$3'}.
expression_3 -> expression_2 '=~' expression_2                                                  : {expression3, '$1', "=~", '$3'}.
expression_3 -> expression_2 IN expression_2                                                    : {expression3, '$1', in, '$3'}.
expression_3 -> expression_2 STARTS WITH expression_2                                           : {expression3, '$1', 'starts with', '$4'}.
expression_3 -> expression_2 ENDS WITH expression_2                                             : {expression3, '$1', 'ends with', '$4'}.
expression_3 -> expression_2 CONTAINS expression_2                                              : {expression3, '$1', contains, '$3'}.
expression_3 -> expression_2 IS NOT NULL                                                        : {expression3, '$1', 'is not null'}.
expression_3 -> expression_2 IS NULL                                                            : {expression3, '$1', 'is null'}.
expression_3 -> expression_2                                                                    : {expression3, '$1'}.

expression_2 -> atom expression_2_addon_list_opt                                                : {expression2, '$1', '$2'}.

expression_2_addon_list_opt -> '$empty'                                                         : [].
expression_2_addon_list_opt -> expression_2_addon_list                                          : '$1'.

expression_2_addon_list -> expression_2_addon_list expression_2_addon                           : '$1' ++ ['$2'].
expression_2_addon_list -> expression_2_addon                                                   : ['$1'].

expression_2_addon -> node_labels                                                               : {nodeLabels, '$1'}.
expression_2_addon -> property_lookup                                                           : '$1'.

expression_commalist -> expression                                                              : ['$1'].
expression_commalist -> expression ',' expression_commalist                                     : ['$1' | '$3'].

atom -> number_literal                                                                          : {atom, '$1'}.
atom -> STRING_LITERAL                                                                          : {atom, {stringLiteral, unwrap('$1')}}.
atom -> parameter                                                                               : {atom, '$1'}.
atom -> TRUE                                                                                    : {atom, {terminal, 'true'}}.
atom -> FALSE                                                                                   : {atom, {terminal, 'false'}}.
atom -> NULL                                                                                    : {atom, {terminal, 'null'}}.
atom -> case_expression                                                                         : {atom, '$1'}.
atom -> COUNT '(' '*' ')'                                                                       : {atom, {terminal, 'count'}}.
atom -> map_literal                                                                             : {atom, '$1'}.
atom -> list_comprehension                                                                      : {atom, '$1'}.
atom -> '[' expression_commalist ']'                                                            : {atom, '$2', "]"}.
atom -> FILTER '(' filter_expression ')'                                                        : {atom, {'filter', '$3'}}.
atom -> EXTRACT '(' filter_expression '|' expression ')'                                        : {atom, {'extract', '$3', '$5'}}.
atom -> EXTRACT '(' filter_expression ')'                                                       : {atom, {'extract', '$3'}}.
atom -> reduce                                                                                  : {atom, '$1'}.
atom -> ALL '(' filter_expression ')'                                                           : {atom, {'all', '$3'}}.
atom -> ANY '(' filter_expression ')'                                                           : {atom, {'any', '$3'}}.
atom -> NONE '(' filter_expression ')'                                                          : {atom, {'none', '$3'}}.
atom -> SINGLE '(' filter_expression ')'                                                        : {atom, {'single', '$3'}}.
atom -> relationships_pattern                                                                   : {atom, '$1'}.
atom -> shortest_path_pattern                                                                   : {atom, '$1'}.
atom -> parenthesized_expression                                                                : {atom, '$1'}.
atom -> function_invocation                                                                     : {atom, '$1'}.
%% wwe atom -> variable                                                                                : {atom, '$1'}.

reduce -> REDUCE '(' variable '=' expression ',' id_in_coll '|' expression ')'                  : {reduce, '$3', '$5', '$7', '$9'}.

partial_comparison_expression -> comparison expression_7                                        : {partialComparisonExpression, '$2', '$1'}.

comparison -> '='                                                                               : "=".
comparison -> '<>'                                                                              : "<>".
comparison -> '!='                                                                              : "!=".
comparison -> '<'                                                                               : "<".
comparison -> '>'                                                                               : ">".
comparison -> '<='                                                                              : "<=".
comparison -> '>='                                                                              : ">=".

parenthesized_expression -> '(' expression ')'                                                  : {parenthesizedExpression, '$2'}.

relationships_pattern -> node_pattern pattern_element_chain_list_opt                            : {relationshipsPattern, '$1', '$2'}.

pattern_element_chain_list_opt -> '$empty'                                                      : [].
pattern_element_chain_list_opt -> pattern_element_chain_list                                    : '$1'.

pattern_element_chain_list -> pattern_element_chain_list pattern_element_chain                  : '$1' ++ ['$2'].
pattern_element_chain_list -> pattern_element_chain                                             : ['$1'].

filter_expression -> id_in_coll where_opt                                                       : {filterExpression, '$1', '$2'}.

where_opt -> '$empty'                                                                           : {}.
where_opt -> where                                                                              : '$1'.

id_in_coll -> variable IN expression                                                            : {idInColl, '$1', '$3'}.

function_invocation -> function_name '(' distinct_opt expression_commalist_opt ')'              : {functionInvocation, '$1', '$4', '$3'}.

distinct_opt -> '$empty'                                                                        : [].
distinct_opt -> DISTINCT                                                                        : "distinct".

expression_commalist_opt -> '$empty'                                                            : [].
expression_commalist_opt -> expression_commalist                                                : '$1'.

function_name -> symbolic_name                                                                  : {functionName, '$1'}.

list_comprehension -> '[' filter_expression char_vertical_bar_expression_opt ']'                : {listComprehension, '$2', '$3'}.

char_vertical_bar_expression_opt -> '$empty'                                                    : {}.
char_vertical_bar_expression_opt -> char_vertical_bar_expression                                : '$1'.

char_vertical_bar_expression -> '|' expression                                                  : '$2'.

property_lookup -> '.' property_key_name char_opt                                               : {propertyLookup, '$2', '$3'}.

char_opt -> '$empty'                                                                            : [].
char_opt -> '?'                                                                                 : "?".
char_opt -> '!'                                                                                 : "!".

case_expression -> CASE expression_opt case_alternatives_list else_expression_opt END           : {caseExpression, '$2', '$3', '$4'}.

expression_opt -> '$empty'                                                                      : {}.
expression_opt -> expression                                                                    : '$1'.

else_expression_opt -> '$empty'                                                                 : {}.
else_expression_opt -> else_expression                                                          : '$1'.

else_expression -> ELSE expression                                                              : '$2'.

case_alternatives_list -> case_alternatives_list case_alternatives                              : '$1' ++ ['$2'].
case_alternatives_list -> case_alternatives                                                     : ['$1'].

case_alternatives -> WHEN expression THEN expression                                            : {caseAlternatives, '$2', '$4'}.

number_literal -> double_literal                                                                : {numberLiteral, '$1'}.
number_literal -> signed_integer_literal                                                        : {numberLiteral, '$1'}.

map_literal -> '{' property_key_name_expression_commalist_opt '}'                               : {mapLiteral, '$2'}.

property_key_name_expression_commalist_opt -> '$empty'                                          : [].
property_key_name_expression_commalist_opt -> property_key_name_expression_commalist            : '$1'.

property_key_name_expression_commalist -> property_key_name_expression                          : ['$1'].
property_key_name_expression_commalist -> property_key_name_expression ',' property_key_name_expression_commalist
                                                                                                : ['$1' | '$3'].

property_key_name_expression -> property_key_name ':' expression                                : {propertyKeyNameExpression, '$1', '$3'}.

parameter -> '{' symbolic_name '}'                                                              : {parameter, '$2'}.
parameter -> '{' unsigned_decimal_integer '}'                                                   : {parameter, '$2'}.

property_expression -> atom property_lookup_list_opt                                            : {propertyExpression, '$1', '$2'}.

property_lookup_list_opt -> '$empty'                                                            : [].
property_lookup_list_opt -> property_lookup_list                                                : '$1'.

property_lookup_list -> property_lookup_list property_lookup                                    : '$1' ++ ['$2'].
property_lookup_list -> property_lookup                                                         : ['$1'].

property_key_name -> symbolic_name                                                              : {propertyKeyName, '$1'}.

signed_integer_literal -> HEX_INTEGER                                                           : {signedIntegerLiteral, {hexInteger, unwrap('$1')}}.
signed_integer_literal -> OCTAL_INTEGER                                                         : {signedIntegerLiteral, {octalInteger, unwrap('$1')}}.
signed_integer_literal -> SIGNED_DECIMAL_INTEGER                                                : {signedIntegerLiteral, {decimalInteger, unwrap('$1')}}.
signed_integer_literal -> unsigned_decimal_integer                                              : '$1'.

unsigned_integer_literal -> unsigned_decimal_integer                                            : {unsignedIntegerLiteral, '$1'}.

unsigned_decimal_integer -> UNSIGNED_DECIMAL_INTEGER                                            : {unsignedDecimalInteger, unwrap('$1')}.
unsigned_decimal_integer -> '0'                                                                 : {unsignedDecimalInteger, "0"}.

double_literal -> EXPONENT_DECIMAL_REAL                                                         : {doubleLiteral, {exponentDecimalReal, unwrap('$1')}}.
double_literal -> regular_decimal_real                                                          : '$1'.

regular_decimal_real -> SIGNED_FLOAT                                                            : {doubleLiteral, {regularDecimalReal, unwrap('$1')}}.
regular_decimal_real -> UNSIGNED_FLOAT                                                          : {doubleLiteral, {regularDecimalReal, unwrap('$1')}}.

symbolic_name -> NAME                                                                           : {symbolicName, unwrap('$1')}.

variable -> symbolic_name                                                                       : {variable, '$1'}.

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
-export([pt_to_string/1, foldtd/3, foldbu/3
         , parsetree/1
         , parsetree_with_tokens/1
         , is_reserved/1]).

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

start(_Type, _Args) -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).
stop(_State)        -> ok.

init([])            -> {ok, { {one_for_one, 5, 10}, []} }.

%%-----------------------------------------------------------------------------
%%                          parser helper functions
%%-----------------------------------------------------------------------------

unwrap({_,_,X}) -> X.

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
    NCypher = if C =:= $; -> Cypher; true -> string:strip(Cypher) ++ ";" end,
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

-spec is_reserved(binary() | atom() | list()) -> true | false.
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

-spec pt_to_string(tuple()| list()) -> {error, term()} | binary().
pt_to_string(PTree) -> foldtd(fun(_,_) -> null_fun end, null_fun, PTree).

-spec foldtd(fun(), term(), tuple() | list()) -> {error, term()} | binary().
foldtd(Fun, Ctx, PTree) when is_function(Fun, 2) ->
    try ocparse_fold:fold(top_down, Fun, Ctx, 0, PTree) of
        {error,_} = Error -> Error;
        {Cypher, null_fun = Ctx} -> 
            list_to_binary(string:strip(Cypher));
        {_Output, NewCtx} -> 
            NewCtx
    catch
        _:Error -> {error, Error}
    end.

-spec foldbu(fun(), term(), tuple()) -> {error, term()} | binary().
foldbu(Fun, Ctx, PTree) when is_function(Fun, 2) ->
    try ocparse_fold:fold(bottom_up, Fun, Ctx, 0, PTree) of
        {error,_} = Error -> Error;
        {Cypher, null_fun = Ctx} -> list_to_binary(string:strip(Cypher));
        {_Output, NewCtx} -> NewCtx
    catch
        _:Error -> {error, Error}
    end.
