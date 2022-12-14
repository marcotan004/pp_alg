#!/usr/bin/env python3

"""Pure untyped lambda calculus."""
''' Base code taken from solution of PP2 '''

import syntax

def clean_term(term): # convert from a typed lambda expression to untyped
    if isinstance(term,list):
        if term[0] == "typedlambda":
            [lam, var, typ, *arg] = term
            lam = "lambda"

            return [lam,var] + [clean_term(t) for t in arg]
        if term[0] == "var":
            return term
        else:
            [op, *args] = term
            return [op] + [clean_term(t) for t in args]  
    else:
        return term
          
def free_vars(term):
    if isinstance(term, str):
        if term in ["zero", "true", "false", "succ", "pred", "iszero", "fix"]:
            return set()

    elif isinstance(term, list):
        if term[0] == "var":
            return {term[1]}
        elif term[0] == "lambda":
            [_, var, body] = term
            return free_vars(body) - set([var])
        elif term[0] == "let":
            raise NotImplementedError()
        else:
            [op, *args] = term
            return set.union(set(), *map(free_vars, args))

    raise ValueError("invalid term: {}".format(term))
'''def sub_case(var, new, term):
    print("Var {}".format(var))
    print("New {}".format(new))
    print("Term {}".format(term))

    if isinstance(term, list):
        pass
    else:
        print(term)
        return term'''
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

# call-by-value fixpoint combinator
Z = syntax.parse_term("λf. (λx. f (λv. x x v)) (λx. f (λv. x x v))")

def desugar_term(term):
    if isinstance(term, list):
        if term[0] == "let":
            [_, var, val, body] = term
            return ["app", ["lambda", var, desugar_term(body)], desugar_term(val)]

        elif term[0] == "fix":
            [_, f] = term
            return ["app", Z, desugar_term(f)]

        else:
            return list(map(desugar_term, term))

    else:
        return term

def eval_term(term):
    if isinstance(term, list):
        if term[0] == "lambda":
            return term

        elif term[0] == "app":
            [_, fun, arg] = term
            fun = eval_term(fun)
            arg = eval_term(arg)
            if fun[0] != "lambda":
                raise TypeError("not a function")
            [_, var, body] = fun
            return eval_term(substitute(var, arg, body))
        
        elif term[0] == "case":
            # check for inject
            if not isinstance(term[1],list):
                pass
            else:
                if term[1][0] != "inject":
                    term = [term[0]] + [eval_term(term[1])] + term[2:] # (E-Case)
            
            if term[1][0] != "inject":
                return term
            else:
                [_, arg, index] = term[1]
                index = (2*index)
                [var, body] = term[index:index+2]
                arg = eval_term(arg) # E-INL, E-INR

                return eval_term(substitute(var, arg, body)) # (E-CASEINL), (E-CASEINR)
                

        elif term[0] == "succ":
            [_, arg] = term
            arg = eval_term(arg)
            if arg == "zero" or arg[0] == "succ":
                return ["succ", arg]
            else:
                raise TypeError("can't take succ of non-number {}".format(arg))

        elif term[0] == "pred":
            [_, arg] = term
            arg = eval_term(arg)
            if arg == "zero":
                return "zero"
            elif arg[0] == "succ":
                return arg[1]
            else:
                raise TypeError("can't take pred of non-number {}".format(arg))

        elif term[0] == "iszero":
            [_, arg] = term
            arg = eval_term(arg)
            if arg == "zero":
                return "true"
            elif arg[0] == "succ":
                return "false"
            else:
                raise TypeError("can't call iszero on non-number")

        elif term[0] == "if":
            [_, cond, true, false] = term
            cond = eval_term(cond)
            if cond == "true":
                return eval_term(true)
            elif cond == "false":
                return eval_term(false)
            else:
                raise TypeError("condition must be boolean")
        
        elif term[0] == "pair": # evaluate terms left to right
            for i in range(len(term)):
                if i == 0:
                    continue
                
                term[i] = eval_term(term[i])
            
            return term
        
        elif term[0] == "project": # return element based on index
            return eval_term(term[1])[int(term[2])] # E-ProjTuple
        
        elif term[0] == "inject":
            return term # T-INL T-INR
        
        elif term[0] == "fold": # E - FOLD
            return ["fold", eval_term(term[1])]
        
        elif term[0] == "unfold": # E-UNFLD
            if eval_term(term[1])[0] == "fold":
                return eval_term(term[1])[1]
            else:
                return ["unfold", eval_term(term[1])]

        else:
            raise ValueError("invalid term: {}".format(term))

    elif term in ["zero", "true", "false"]:
        return term

    else:
        raise ValueError("invalid term: {}".format(term))
