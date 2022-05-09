%option noyywrap

%{
#include <iostream>
%}

%%

[ \n]+ {}
<single_line_comment>
<multiline_comment>
\+ { std::cout << "PLUS" << std::endl; }
\* { std::cout << "TIMES" << std::endl; }
- { std::cout << "MINUS" << std::endl; }
\/ { std::cout << "DIVIDE" << std::endl; }
@
\.
[0-9]+ { std::cout << "NUMBER: " << yytext << std::endl; }
def { std::cout << "KEYWORD: def" << std::endl; }
var 
data { std::cout << "KEYWORD: data" << std::endl; }
map { std::cout << "KEYWORD: map" << std::endl; }
to { std::cout << "KEYWORD: to" << std::endl; }
\{ { std::cout << "OPEN CURLY" << std::endl; }
\} { std::cout << "CLOSED CURLY" << std::endl; }
\( { std::cout << "OPEN PARENTH" << std::endl; }
\) { std::cout << "CLOSE PARENTH" << std::endl; }
\[
\]
<string>
,  { std::cout << "COMMA" << std::endl; }
-> { std::cout << "PATTERN ARROW" << std::endl; }
= { std::cout << "EQUAL" << std::endl; }
[a-z][a-zA-Z_]* { std::cout << "LOWERCASE IDENTIFIER: " << yytext << std::endl; }
[A-Z][a-zA-Z_]* { std::cout << "UPPERCASE IDENTIFIER: " << yytext << std::endl; }
%%

int main() {
    yylex();
}