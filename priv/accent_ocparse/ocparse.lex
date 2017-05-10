%{
#include "yygrammar.h"
%}
%%
([//][.]*[\n\r]?)                       { /* COMMENT */ }
([#][.]*[\n\r]?)                        { /* COMMENT */ }
(0|([1-9][0-9]*))                       { return DECIMAL_INTEGER; } 
(`([^`]*)*`)                            { return ESCAPED_SYMBOLIC_NAME; } 
((\.[0-9]+)(e|E)[-]?[0-9]+)             { return EXPONENT_DECIMAL_REAL; }
(([0-9]+)(e|E)[-]?[0-9]+)               { return EXPONENT_DECIMAL_REAL; } 
(([0-9]+\.[0-9]+)(e|E)[-]?[0-9]+)       { return EXPONENT_DECIMAL_REAL; } 
(0x([0-9]|[A-Fa-f])+)                   { return HEX_INTEGER; } 
([A-F])                                 { return HEX_LETTER; } 
(0[0-7]+)                               { return OCTAL_INTEGER; } 
([0-9]*\.[0-9]+)                        { return REGULAR_DECIMAL_REAL; } 
([A-Za-z_@#][A-Za-z0-9_@#]*)            { return UNESCAPED_SYMBOLIC_NAME; } 
(\'([^\\\']*)*\')                       { return STRING_LITERAL; } 
([/][\*][.|\n|\r]*[\*][/])              { return STRING_LITERAL; } 
"~"                                     { return '~'; }
"^"                                     { return '^'; }
"."                                     { return '.'; }
"|"                                     { return '|'; }
"*"                                     { return '*'; }
"+"                                     { return '+'; }
"("                                     { return ')'; }
")"                                     { return ')'; }
"["                                     { return '['; }
"]"                                     { return ']'; }
"{"                                     { return '{'; }
"}"                                     { return '}'; }
"-"                                     { return '-'; }
"="                                     { return '='; }
"<"                                     { return '<'; }
">"                                     { return '>'; }
"//"                                    { return '/'; }
"%"                                     { return '%'; }
":"                                     { return ':'; }
","                                     { return ','; }
";"                                     { return ';'; }
"$"                                     { return '$'; }
"[\r\s\t]*"                             { /* skip blank */ }
"\n"                                    { yypos++; /* adjust linenumber and skip newline */ }
.                                       { yyerror("illegal token"); }

