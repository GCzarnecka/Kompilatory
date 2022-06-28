#include "ast.hpp"
#include "parser.hpp"

void yy::parser::error(const std::string &msg)
{
    std::cout << "An error occured: " << msg << std::endl;
}

extern std::vector<definition_ptr> program;

int main()
{
    yy::parser parser;
    parser.parse();
    std::cout << "Program size: " << program.size() << std::endl;
}