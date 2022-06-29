#pragma once
#include "ast.hpp"
#include <ostream>
#include <sstream>
#include <string>

// std::string op_name(binop op)
// {
//     switch (op)
//     {
//     case PLUS:
//         return "+";
//     case MINUS:
//         return "-";
//     case TIMES:
//         return "*";
//     case DIVIDE:
//         return "/";
//     }
//     return "??";
// }

// void print_indent(int n, std::ostream &to)
// {
//     while (n--)
//         to << "  ";
// }

// void ast_int::print(int indent, std::ostream &to) const
// {
//     print_indent(indent, to);
//     to << "INT: " << value << std::endl;
// }

// void ast_lid::print(int indent, std::ostream &to) const
// {
//     print_indent(indent, to);
//     to << "LID: " << id << std::endl;
// }

// void ast_uid::print(int indent, std::ostream &to) const
// {
//     print_indent(indent, to);
//     to << "UID: " << id << std::endl;
// }

// void ast_binop::print(int indent, std::ostream &to) const
// {
//     print_indent(indent, to);
//     to << "BINOP: " << op_name(op) << std::endl;
//     left->print(indent + 1, to);
//     right->print(indent + 1, to);
// }

// void ast_app::print(int indent, std::ostream &to) const
// {
//     print_indent(indent, to);
//     to << "APP:" << std::endl;
//     left->print(indent + 1, to);
//     right->print(indent + 1, to);
// }

// void ast_case::print(int indent, std::ostream &to) const
// {
//     print_indent(indent, to);
//     to << "CASE: " << std::endl;
//     for (auto &branch : branches)
//     {
//         print_indent(indent + 1, to);
//         branch->pat->print(to);
//         to << std::endl;
//         branch->expr->print(indent + 2, to);
//     }
// }

// void pattern_var::print(std::ostream &to) const
// {
//     to << var;
// }

// void pattern_constr::print(std::ostream &to) const
// {
//     to << constr;
//     for (auto &param : params)
//     {
//         to << " " << param;
//     }
// }
