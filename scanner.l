%option noyywrap

%{
#include <iostream>
#include "ast.hpp"
#include "parser.hpp"

#define YY_DECL yy::parser::symbol_type yylex()

%}

%%

[ \n]+ {}
\/\/[^\n]* { return yy::parser::make_SINGLE_COMMENT(); }
/\*[^*]*\*+(?:[^/*][^*]*\*+)*/ { return yy::parser::make_MULTILINE_COMMENT(); }
\+ { return yy::parser::make_PLUS(); }
\* { return yy::parser::make_TIMES(); }
- { return yy::parser::make_MINUS(); }
\/ { return yy::parser::make_DIVIDE(); }
@ { return yy::parser::make_AT(); }
\. { return yy::parser::make_COMP(); }
[0-9]+ { return yy::parser::make_INT(); }
def { return yy::parser::make_DEF(); }
var { return yy::parser::make_VAR(); }
data { return yy::parser::make_DATA(); }
map { return yy::parser::make_MAP(); }
to { return yy::parser::make_TO(); }
\{ { return yy::parser::make_O_CURLY(); }
\} { return yy::parser::make_C_CURLY(); }
\( { return yy::parser::make_O_PAREN(); }
\) { return yy::parser::make_C_PAREN(); }
\[ { return yy::parser::make_O_SQUARE(); }
\] { return yy::parser::make_C_SQUARE(); }
\"[^\"]*\" { return yy::parser::make_STR(); }
, { return yy::parser::make_COMMA(); }
-> { return yy::parser::make_ARROW(); }
= { return yy::parser::make_EQUAL(); }
[a-z][a-zA-Z_]* { return yy::parser::make_LID(); }
[A-Z][a-zA-Z_]* { return yy::parser::make_UID(); }

%%