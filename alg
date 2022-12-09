#!/usr/bin/env python3

"""Simply-typed lambda calculus."""
# Base code taken from PP3 solution

import pdict

def check_types(term, context=None): 
    if context is None: context = pdict.PersistentDictionary()

    if isinstance(term, list):
        if term[0] == "typedlambda":
            [_, var, var_type, body] = term
            body_type = check_types(body, context.set(var, var_type))
            return ["arrow", var_type, body_type]
        
        elif term[0] == "let":
            [_, var, val, body] = term
            val_type = check_types(val, context)
            return check_types(body, context.set(var, val_type))

        elif term[0] == "var":
            if term[1] in context:
                return context[term[1]]
            else:
                raise ValueError("unbound variable {}".format(term[1]))

        elif term[0] == "app":
            [_, fun, arg] = term
            fun_type = check_types(fun, context)
            arg_type = check_types(arg, context)
            if fun_type[0] != 'arrow':
                raise TypeError("{} is not a function".format(syntax.format_term(fun)))
            [_, var_type, res_type] = fun_type

            if check_equal(arg_type, var_type, context):
                return res_type

            if var_type != arg_type:
                raise TypeError("types do not match")

            

            return res_type
        
        elif term[0] == "case": # T-Case

            t0 = term[1]
            t0_type = check_types(t0, context)

            if not isinstance(t0_type, list) or t0_type[0] != 'sum':
                raise TypeError("case statement takes first argument of sum type {}".format(syntax.format_term(term)))
            
            left_output = term[3]
            left_var = term[2]
            left_var_type = t0_type[1]

            right_output = term[5]
            right_var = term[4]
            right_var_type = t0_type[2]

            
            left_type = check_types(left_output, context.set(left_var,left_var_type)) 
            right_type = check_types(right_output,context.set(right_var,right_var_type))

            if left_type == right_type: # check that inr and inl output are the same
                return left_type
            else:
                if check_equal(left_type, right_type, context):
                    if isinstance(left_type, list) and left_type[0] == "mu":
                        return left_type
                    else:
                        return right_type
                
                raise ValueError("inl and inr do not match in {}".format(syntax.format_term(term)))

        elif term[0] == "typedinject": #T-INL T-INR
            case_var = term[1]
            index_type = term[3][term[2]]
            if check_types(case_var,context) == index_type: # does injected var match index type of sum
                return term[3] # return sum type if works
            else:
                raise TypeError("Invalid type for {}".format(syntax.format_term(term)))

        elif term[0] == "inject":
            raise TypeError("Please type inject statement {}".format(syntax.format_term(term)))

        elif term[0] == "fix":
            [_, arg] = term
            arg_type = check_types(arg, context)
            if arg_type[0] != "arrow":
                raise TypeError("argument of fix must be a function (not {})".format(arg_type))
            [_, arg_type, res_type] = arg_type
            if arg_type != res_type:
                raise TypeError("argument of fix must have same argument and result type")
            return res_type

        elif term[0] in ["succ", "pred"]:
            [op, arg] = term
            arg_type = check_types(arg, context)
            if arg_type != "Nat":
                raise TypeError("argument of {} must be of type Nat".format(op))
            return "Nat"

        elif term[0] == "iszero":
            [op, arg] = term
            arg_type = check_types(arg, context)
            if arg_type != "Nat":
                raise TypeError("argument of {} must be of type Nat".format(op))
            return "Bool"

        elif term[0] == "if":
            [_, cond, true, false] = term
            cond_type = check_types(cond, context)
            true_type = check_types(true, context)
            false_type = check_types(false, context)
            if cond_type != "Bool":
                raise TypeError("condition must be of type Bool")
            if true_type != false_type:
                raise TypeError("branches of conditional must have same type")
            return true_type
        
        elif term[0] == "pair":  # T-Tuple
            return ["product", check_types(term[1],context),check_types(term[2],context)]
        
        elif term[0] == "project": # T-Proj
            return check_types(term[1],context)[int(term[2])]

        elif term[0] == "fold": # requires only one recursive type since no way to save types, T-FOLD
            flag = False
            for i in context.items():
                if i[1][0] == "mu":
                    var_type = i[1]
                    flag = True
                    break
            
            if not flag:
                raise TypeError ("Need recursive type for fold")
            
            [_, find, replacer] = var_type 
            t1_type = sub_var(find, var_type, replacer)

            if check_equal(check_types(term[1],context),t1_type, context): # check if t1 fits recursive type
                return var_type
            else:
                raise TypeError("{} is not of specified recursive type".format(syntax.format_term(term[1])))
        
        elif term[0] == "unfold":
            for i in context.items():
                if i[1][0] == "mu":
                    var_type = i[1]
                    break
            
            if not var_type:
                raise("Need recursive type for fold")
            
            if not check_equal(check_types(term[1],context), var_type, context):
                raise TypeError("{} needs to be of type {}".format(syntax.format_term(term[1]), syntax.format_type([var_type])))

            [_, find, term] = var_type 

            unfold_type = sub_var(find, var_type, term)

            return sub_var(find, var_type, term)
        
    elif term == "zero":
        return "Nat"

    elif term in ["true", "false"]:
        return "Bool"

    else:
        raise ValueError("invalid term")

def sub_var(var, new, term):
    if isinstance(term, list):
        if term[0] == "typevar":
            if term[1] == var:
                return new
            else:
                return term
        else:
            [op, *args] = term
            return [op] + [sub_var(var,new,t) for t in args]
    else:
        return term

def substitute(var, new, term):
    """Substitute subterm for var in term."""
    if isinstance(term, list):
        if term[0] == "var":
            if term[1] == var:
                return new
            else:
                return term
        elif term[0] == "lambda":
            [_, evar, ebody] = term
            if evar == var:
                return term
            else:
                return ["lambda", evar, substitute(var, new, ebody)]
        elif term[0] == "let":
            raise NotImplementedError()
        else:
            [op, *args] = term
            return [op] + [substitute(var, new, t) for t in args]

    else:
        return term    

def check_equal(term_type, var_type, context): # check if a type is equal to a recursive type
    if len(var_type) == 1 and isinstance(var_type, list):
         var_type = var_type[0]
    if len(term_type) == 1 and isinstance(term_type, list):
        term_type = term_type[0]

    if term_type == var_type:
        return True 
    if isinstance(var_type, list):
        if var_type[0] == "mu":
            [_, find, term] = var_type 

            var_type = sub_var(find, var_type, term)

        if var_type[0] == "sum":
            if isinstance(term_type, list):
                term_pass = term_type.copy()
            else:
                term_pass = term_type
            
            if isinstance(var_type, list):
                var_pass = var_type.copy()
            else:
                var_pass = var_type

            if check_equal(term_pass, var_pass[1], context):
                return True
            elif check_equal(term_pass, var_pass[2], context):
                return True

        if isinstance(term_type, list):
            if var_type[0] == term_type[0]:
                if var_type[0] == 'product' and (var_type[1] == term_type[1]):
                    var_type.pop(0)
                    var_type.pop(0)
                    term_type.pop(0)
                    term_type.pop(0)

                    if isinstance(term_type, list):
                        term_pass = term_type.copy()
                    else:
                        term_pass = term_type
            
                    if isinstance(var_type, list):
                        var_pass = var_type.copy()
                    else:
                        var_pass = var_type
                    
                    return check_equal(term_pass, var_pass, context)

                    
        else:
            return False

    return False

def erase_types(t): # remove types to perform lambda calculus
    if isinstance(t, list):
        if t[0] == "typedlambda":
            [_, var, _, body] = t
            return ["lambda", var, erase_types(body)]
        if t[0] == "typedinject":
            [_, term, index, _] = t
            return ["inject", term, index]
        else:
            return [erase_types(child) for child in t]
    else:
        return t


if __name__ == "__main__":
    import sys
    import syntax
    import lam

    for line in syntax.read_lines("alg>"):
        try:
            term = syntax.parse_term(line)
            term_type = check_types(term)
            term = lam.desugar_term(erase_types(term))
            result = lam.eval_term(term)
            print("{} : {}".format(syntax.format_term(result), 
                                   syntax.format_type(term_type)))
        except (syntax.ParseError, TypeError, ValueError) as e:
            print("error: {}".format(e))
