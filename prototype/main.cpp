#include "ast.hpp"
#include "parser.hpp"
#include "type.hpp"

void yy::parser::error(const std::string &msg)
{
    std::cout << "An error occured: " << msg << std::endl;
}

extern std::vector<definition_ptr> program;

void typecheck_program(const std::vector<definition_ptr> &prog)
{
    type_mgr mgr;
    type_env env;

    type_ptr int_type = type_ptr(new type_base("Int"));
    type_ptr binop_type = type_ptr(new type_arr(
        int_type,
        type_ptr(new type_arr(int_type, int_type))));

    env.bind("+", binop_type);
    env.bind("-", binop_type);
    env.bind("*", binop_type);
    env.bind("/", binop_type);

    for (auto &def : prog)
    {
        def->typecheck_first(mgr, env);
    }

    for (auto &def : prog)
    {
        def->typecheck_second(mgr, env);
    }
}

int main()
{
    yy::parser parser;
    parser.parse();
    typecheck_program(program);
    std::cout << "Number of correct declarations: " << program.size() << std::endl;
}