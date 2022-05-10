%option noyywrap

%{
#include <iostream>
%}

%%

[ \n]+ {}
\/\/[^\n]* { std::cout << "SINGLE LINE COMMENT" << std::endl; }
/\*[^*]*\*+(?:[^/*][^*]*\*+)*/ { std::cout << "MULTI-LINE COMMENT" << std::endl; }
\+ { std::cout << "PLUS" << std::endl; }
\* { std::cout << "TIMES" << std::endl; }
- { std::cout << "MINUS" << std::endl; }
\/ { std::cout << "DIVIDE" << std::endl; }
@ { std::cout << "AT" << std::endl; }
\. { std::cout << "COMPOSITION" << std::endl; }
[0-9]+ { std::cout << "NUMBER: " << yytext << std::endl; }
def { std::cout << "KEYWORD: def" << std::endl; }
var { std::cout << "KEYWORD: var" << std::endl; }
data { std::cout << "KEYWORD: data" << std::endl; }
map { std::cout << "KEYWORD: map" << std::endl; }
to { std::cout << "KEYWORD: to" << std::endl; }
\{ { std::cout << "OPEN CURLY" << std::endl; }
\} { std::cout << "CLOSED CURLY" << std::endl; }
\( { std::cout << "OPEN PARENTH" << std::endl; }
\) { std::cout << "CLOSE PARENTH" << std::endl; }
\[ { std::cout << "CLOSE SQUARE" << std::endl; }
\] { std::cout << "CLOSE SQUARE" << std::endl; }
\"[^\"]*\" { std::cout << "STRING" << std::endl; }
, { std::cout << "COMMA" << std::endl; }
-> { std::cout << "PATTERN ARROW" << std::endl; }
= { std::cout << "EQUAL" << std::endl; }
[a-z][a-zA-Z_]* { std::cout << "LOWERCASE IDENTIFIER: " << yytext << std::endl; }
[A-Z][a-zA-Z_]* { std::cout << "UPPERCASE IDENTIFIER: " << yytext << std::endl; }
%%

int main() {
    yylex();
}