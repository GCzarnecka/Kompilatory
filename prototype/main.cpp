#include "ast.hpp"
#include <iostream>
#include "parser.hpp"
#include "error.hpp"
#include "type.hpp"
void yy::parser::error(const std::string& msg) {
    std::cout << "An error occured: " << msg << std::endl;
}

extern std::vector<definition_ptr> program;

void typecheck_program(
        const std::vector<definition_ptr>& prog,
        type_mgr& mgr, type_env& env) {
    type_ptr int_type = type_ptr(new type_base("Int"));
    type_ptr binop_type = type_ptr(new type_arr(
            int_type,
            type_ptr(new type_arr(int_type, int_type))));

    env.bind("+", binop_type);
    env.bind("-", binop_type);
    env.bind("*", binop_type);
    env.bind("/", binop_type);

    for(auto& def : prog) {
        def->typecheck_first(mgr, env);
    }

    for(auto& def : prog) {
        def->typecheck_second(mgr, env);
    }


    for(auto& pair : env.names) {
        std::cout << pair.first << ": ";
        pair.second->print(mgr, std::cout);
        std::cout << std::endl;
    }
}

int main() {
    yy::parser parser;
    type_mgr mgr;
    type_env env;

    parser.parse();
    for(auto& definition : program) {
        definition_defn* def = dynamic_cast<definition_defn*>(definition.get());
        if(!def) continue;

        std::cout << def->name;
        for(auto& param : def->params) std::cout << " " << param;
        std::cout << ":" << std::endl;

        def->body->print(1, std::cout);
    }
    try {
        typecheck_program(program, mgr, env);
    } catch(unification_error& err) {
        std::cout << "failed to unify types: " << std::endl;
        std::cout << "  (1) \033[34m";
        err.left->print(mgr, std::cout);
        std::cout << "\033[0m" << std::endl;
        std::cout << "  (2) \033[32m";
        err.right->print(mgr, std::cout);
        std::cout << "\033[0m" << std::endl;
    } catch(type_error& err) {
        std::cout << "failed to type check program: " << err.description << std::endl;
    }
}