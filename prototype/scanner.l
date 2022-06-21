%option noyywrap

%{
#include <iostream>
#include "ast.hpp"
#include "parser.hpp"

#define YY_DECL yy::parser::symbol_type yylex()

%}

%%

[ \n]+ {}
\/\/[^\n]* { return yy::parser::make_SINGLE_COMMENT(std::string(yytext)); }
\/\*[^*]*\\*+(?:[^\/*][^*]*\\*+)\*\/ { return yy::parser::make_MULTILINE_COMMENT(std::string(yytext)); }
\+ { return yy::parser::make_PLUS(); }
\* { return yy::parser::make_TIMES(); }
- { return yy::parser::make_MINUS(); }
\/ { return yy::parser::make_DIVIDE(); }
[0-9]+ { return yy::parser::make_INT(atoi(yytext)); }
def { return yy::parser::make_DEF(); }
data { return yy::parser::make_DATA(); }
map { return yy::parser::make_MAP(); }
to { return yy::parser::make_TO(); }
\{ { return yy::parser::make_OCURLY(); }
\} { return yy::parser::make_CCURLY(); }
\( { return yy::parser::make_OPAREN(); }
\) { return yy::parser::make_CPAREN(); }
,  { return yy::parser::make_COMMA(); }
-> { return yy::parser::make_ARROW(); }
= { return yy::parser::make_EQUAL(); }
[a-z][a-zA-Z]* { return yy::parser::make_LID(std::string(yytext)); }
[A-Z][a-zA-Z]* { return yy::parser::make_UID(std::string(yytext)); }

%%