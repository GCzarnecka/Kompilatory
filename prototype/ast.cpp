#include "ast.hpp"

std::string op_name(binop op)
{
    switch (op)
    {
    case PLUS:
        return "+";
    case MINUS:
        return "-";
    case TIMES:
        return "*";
    case DIVIDE:
        return "/";
    }
    throw 0;
}

void ast_int::print(int indent, std::ostream& to) const {
    print_indent(indent, to);
    to << "INT: " << value << std::endl;
}
type_ptr ast_int::typecheck(type_mgr &mgr, const type_env &env) const
{
    return type_ptr(new type_base("Int"));
}

void ast_lid::print(int indent, std::ostream& to) const {
    print_indent(indent, to);
    to << "LID: " << id << std::endl;
}
type_ptr ast_lid::typecheck(type_mgr &mgr, const type_env &env) const
{
    return env.lookup(id);
}

void ast_uid::print(int indent, std::ostream& to) const {
    print_indent(indent, to);
    to << "UID: " << id << std::endl;
}
type_ptr ast_uid::typecheck(type_mgr &mgr, const type_env &env) const
{
    return env.lookup(id);
}

void ast_binop::print(int indent, std::ostream& to) const {
    print_indent(indent, to);
    to << "BINOP: " << op_name(op) << std::endl;
    left->print(indent + 1, to);
    right->print(indent + 1, to);
}
type_ptr ast_binop::typecheck(type_mgr &mgr, const type_env &env) const
{
    type_ptr ltype = left->typecheck(mgr, env);
    type_ptr rtype = right->typecheck(mgr, env);
    type_ptr ftype = env.lookup(op_name(op));
    if(!ftype)
        throw type_error(std::string("unknown binary operator ") + op_name(op));

    type_ptr return_type = mgr.new_type();
    type_ptr arrow_one = type_ptr(new type_arr(rtype, return_type));
    type_ptr arrow_two = type_ptr(new type_arr(ltype, arrow_one));

    mgr.unify(arrow_two, ftype);
    return return_type;
}
void ast_app::print(int indent, std::ostream& to) const {
    print_indent(indent, to);
    to << "APP:" << std::endl;
    left->print(indent + 1, to);
    right->print(indent + 1, to);
}
type_ptr ast_app::typecheck(type_mgr &mgr, const type_env &env) const
{
    type_ptr ltype = left->typecheck(mgr, env);
    type_ptr rtype = right->typecheck(mgr, env);

    type_ptr return_type = mgr.new_type();
    type_ptr arrow = type_ptr(new type_arr(rtype, return_type));
    mgr.unify(arrow, ltype);
    return return_type;
}

void ast_case::print(int indent, std::ostream& to) const {
    print_indent(indent, to);
    to << "CASE: " << std::endl;
    for(auto& branch : branches) {
        print_indent(indent + 1, to);
        branch->pat->print(to);
        to << std::endl;
        branch->expr->print(indent + 2, to);
    }
}
type_ptr ast_map::typecheck(type_mgr &mgr, const type_env &env) const
{
    type_ptr map_type = to->typecheck(mgr, env);
    type_ptr branch_type = mgr.new_type();

    for (auto &branch : branches)
    {
        type_env new_env = env.scope();
        branch->pat->match(map_type, mgr, new_env);
        type_ptr curr_branch_type = branch->expr->typecheck(mgr, new_env);
        mgr.unify(branch_type, curr_branch_type);
    }

    case_type = mgr.resolve(case_type, var);
    if(!dynamic_cast<type_base*>(case_type.get())) {
        throw type_error("attempting case analysis of non-data type");
    }

    return branch_type;
}

void pattern_var::match(type_ptr t, type_mgr &mgr, type_env &env) const
{
    env.bind(var, t);
}

void pattern_var::print(std::ostream& to) const {
    to << var;
}
void pattern_constr::print(std::ostream& to) const {
    to << constr;
    for(auto& param : params) {
        to << " " << param;
    }
}

void pattern_constr::match(type_ptr t, type_mgr &mgr, type_env &env) const
{
    type_ptr constructor_type = env.lookup(constr);
    if(!constructor_type) {
        throw type_error(std::string("pattern using unknown constructor ") + constr);
    }

    for (int i = 0; i < params.size(); i++)
    {
        type_arr *arr = dynamic_cast<type_arr *>(constructor_type.get());
        if(!arr)
            throw type_error("too many parameters in constructor pattern");

        env.bind(params[i], arr->left);
        constructor_type = arr->right;
    }

    mgr.unify(t, constructor_type);
    //type_base *result_type = dynamic_cast<type_base *>(constructor_type.get());

}