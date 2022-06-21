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

type_ptr ast_int::typecheck(type_mgr &mgr, const type_env &env) const
{
    return type_ptr(new type_base("Int"));
}

type_ptr ast_lid::typecheck(type_mgr &mgr, const type_env &env) const
{
    return env.lookup(id);
}

type_ptr ast_uid::typecheck(type_mgr &mgr, const type_env &env) const
{
    return env.lookup(id);
}

type_ptr ast_binop::typecheck(type_mgr &mgr, const type_env &env) const
{
    type_ptr ltype = left->typecheck(mgr, env);
    type_ptr rtype = right->typecheck(mgr, env);
    type_ptr ftype = env.lookup(op_name(op));
    if (!ftype)
        throw 0;

    type_ptr return_type = mgr.new_type();
    type_ptr arrow_one = type_ptr(new type_arr(rtype, return_type));
    type_ptr arrow_two = type_ptr(new type_arr(ltype, arrow_one));

    mgr.unify(arrow_two, ftype);
    return return_type;
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

    return branch_type;
}

void pattern_var::match(type_ptr t, type_mgr &mgr, type_env &env) const
{
    env.bind(var, t);
}

void pattern_constr::match(type_ptr t, type_mgr &mgr, type_env &env) const
{
    type_ptr constructor_type = env.lookup(constr);
    if (!constructor_type)
        throw 0;

    for (int i = 0; i < params.size(); i++)
    {
        type_arr *arr = dynamic_cast<type_arr *>(constructor_type.get());
        if (!arr)
            throw 0;

        env.bind(params[i], arr->left);
        constructor_type = arr->right;
    }

    mgr.unify(t, constructor_type);
    type_base *result_type = dynamic_cast<type_base *>(constructor_type.get());
    if (!result_type)
        throw 0;
}