import sys
import os
import collections
try:
    import readline
except ImportError:
    pass

class ParseError(Exception):
    pass

### Constants

special_toks = ["=>", # must come before "="
                "(", ")", "λ", "\\", ".", "=", ":", "->",
                "*", "{", ",", "}",
                "+", "|",
                "μ", "mu",
                ]

reserved_words = ["0", "succ", "pred", "iszero",
                  "true", "false", "if", "then", "else",
                  "let", "in",
                  "fix",
                  "inl", "inr", "as", "case", "of",
                  "fold", "unfold",
                  ]

# tokens that end a chain of applications
terminators = [")", "then", "else", "in", "as", ",", "}", "of", "=>", "|"]

### Lexer

def lexer(s):
    i = j = 0
    tokens = []
    def flush():
        nonlocal i
        if i < j:
            tokens.append("".join(s[i:j]))
            i = j
    while j < len(s):
        if s[j].isspace():
            flush()
            i = j = j+1
        else:
            for tok in special_toks:
                if s[j:j+len(tok)] == tok:
                    flush()
                    tokens.append(tok)
                    i = j = j+len(tok)
                    break
            else:
                j += 1
    flush()
    return tokens

### Parser

def expect(what, w):
    if len(w) == 0:
        raise ParseError("expected '{}' (not end of string)".format(what))
    elif w[0] != what:
        raise ParseError("expected '{}' (not '{}')".format(what, w[0]))
    w.popleft()
    
def parse_term(s):
    w = collections.deque(lexer(s))
    t = parse_abs(w)
    if len(w) != 0:
        raise ParseError("unexpected '{}' after term".format(w[0]))
    return t
    
def parse_abs(w):
    if len(w) == 0:
        raise ParseError("unexpected end of string")
    elif w[0] in ["\\", "λ"]:
        w.popleft()
        x = parse_var(w)
        if len(w) == 0:
            raise ParseError("unexpected end of string")
        elif w[0] == ":":
            expect(":", w)
            tau = parse_type(w)
            expect(".", w)
            t = parse_abs(w)
            return ["typedlambda", x, tau, t]
        elif w[0] == ".":
            expect(".", w)
            t = parse_abs(w)
            return ["lambda", x, t]
        else:
            raise ParseError("expected . or :")
    elif w[0] == "let":
        expect("let", w)
        x = parse_var(w)
        expect("=", w)
        t1 = parse_abs(w)
        expect("in", w)
        t2 = parse_abs(w)
        return ["let", x, t1, t2]
    elif w[0] == "if":
        expect("if", w)
        t1 = parse_abs(w)
        expect("then", w)
        t2 = parse_abs(w)
        expect("else", w)
        t3 = parse_abs(w)
        return ["if", t1, t2, t3]
    elif w[0] == 'case':
        expect('case', w)
        t0 = parse_abs(w)
        expect('of', w)
        expect('inl', w)
        xl = parse_var(w)
        expect('=>', w)
        tl = parse_abs(w)
        expect('|', w)
        expect('inr', w)
        xr = parse_var(w)
        expect('=>', w)
        tr = parse_abs(w)
        return ['case', t0, xl, tl, xr, tr]
    else:
        return parse_app(w)

def parse_app(w):
    if w[0] in ["succ", "pred", "iszero", "fix", "fold", "unfold"]:
        op = w.popleft()
        t = [op, parse_atom(w)]
    elif w[0] in ['inl', 'inr']:
        op = w.popleft()
        if op == 'inl':
            field = 1
        elif op == 'inr':
            field = 2
        arg = parse_atom(w)
        if len(w) > 0 and w[0] == 'as':
            expect('as', w)
            typ = parse_type(w)
            t = ['typedinject', arg, field, typ]
        else:
            t = ['inject', arg, field]
    else:
        t = parse_atom(w)
    while len(w) > 0 and w[0] not in terminators:
        t = ["app", t, parse_atom(w)]

    return t

def parse_atom(w):
    if len(w) == 0:
        raise ParseError("unexpected end of string")
    elif w[0] == "(":
        expect("(", w)
        t = parse_abs(w)
        expect(")", w)
    elif w[0] == "0":
        expect("0", w)
        t = "zero"
    elif w[0] in ["true", "false"]:
        return w.popleft()
    elif w[0] == '{':
        expect('{', w)
        t1 = parse_abs(w)
        expect(',', w)
        t2 = parse_abs(w)
        expect('}', w)
        t = ['pair', t1, t2]
    else:
        t = ["var", parse_var(w)]
        
    if len(w) > 0 and w[0] == '.':
        expect('.', w)
        field = parse_var(w)
        if field in ['1', '2']:
            t = [f'project', t, field]
        else:
            raise ParseError("projection (.) must be followed by 1 or 2")
    return t

def parse_var(w):
    if len(w) == 0:
        raise ParseError("unexpected end of string")
    elif w[0] in special_toks or w[0] in reserved_words:
        raise ParseError("unexpected '{}'".format(w[0]))
    else:
        return w.popleft()

def parse_mu(w):
    if len(w) > 0 and w[0] in ['mu', 'μ']:
        w.popleft()
        tv = parse_var(w)
        expect(".", w)
        typ = parse_type(w)
        return ['mu', tv, typ]
    else:
        return parse_arrow(w)
    
def parse_arrow(w):
    tau = parse_sum(w)
    if len(w) > 0 and w[0] == "->":
        expect("->", w)
        tau2 = parse_arrow(w)
        return ["arrow", tau, tau2]
    else:
        return tau

def parse_sum(w):
    tau = ["sum", parse_product(w)]
    while len(w) > 0 and w[0] == '+':
        expect("+", w)
        tau.append(parse_product(w))
    if len(tau) == 2:
        tau = tau[1]
    elif len(tau) > 3:
        raise ParseError('Sums of three or more types not supported; use parentheses')
    return tau

def parse_product(w):
    tau = ["product", parse_base(w)]
    while len(w) > 0 and w[0] == '*':
        expect("*", w)
        tau.append(parse_base(w))
    if len(tau) == 2:
        tau = tau[1]
    elif len(tau) > 3:
        raise ParseError('Products of three or more types not supported; use parentheses')
    return tau

def parse_base(w):
    if len(w) == 0:
        raise ParseError("unexpected end of string")
    elif w[0] in ["Nat", "Bool"]:
        tau = w[0]
        w.popleft()
        return tau
    elif w[0] == "(":
        expect("(", w)
        tau = parse_type(w)
        expect(")", w)
        return tau
    else:
        return ['typevar', parse_var(w)]

parse_type = parse_mu

def format_abs(t):
    if isinstance(t, list):
        if t[0] == "if":
            return "if {} then {} else {}".format(format_abs(t[1]), format_abs(t[2]), format_abs(t[3]))
        elif t[0] == "let":
            return "let {} = {} in {}".format(t[1], format_abs(t[2]), format_abs(t[3]))
        elif t[0] == "lambda":
            return "λ{}. {}".format(t[1], format_abs(t[2]))
        elif t[0] == "typedlambda":
            return "λ{}:{}. {}".format(t[1], format_type(t[2]), format_abs(t[3]))
        elif t[0] == "closure":
            return "<closure>"
        elif t[0] == 'case':
            return f'case {format_abs(t[1])} of inl {t[2]} => {format_abs(t[3])} | inr {t[4]} => {format_abs(t[5])}'
    return format_app(t)

def format_app(t):
    if isinstance(t, list):
        if t[0] in ["succ", "pred", "iszero", "fix", "fold", "unfold"]:
            return "{} {}".format(t[0], format_atom(t[1]))
        elif t[0] == 'inject':
            if t[2] == 1:
                return f'inl {format_atom(t[1])}'
            elif t[2] == 2:
                return f'inr {format_atom(t[1])}'
        elif t[0] == 'typedinject':
            if t[2] == 1:
                return f'inl {format_atom(t[1])} as {format_arrow(t[3])}'
            elif t[2] == 2:
                return f'inr {format_atom(t[1])} as {format_arrow(t[3])}'
        elif t[0] == "app":
            return "{} {}".format(format_app(t[1]), format_atom(t[2]))
    return format_atom(t)

def format_atom(t):
    if isinstance(t, list):
        if t[0] == "var":
            return t[1]
        elif t[0] in ["if", "let", "lambda", "typedlambda",
                      "succ", "pred", "iszero", "fix", "app", "fold", "unfold"]:
            return "({})".format(format_abs(t))
        elif t[0] == 'pair':
            return f"{{{format_abs(t[1])}, {format_abs(t[2])}}}"
        elif t[0] == "project":
            return f"{format_abs(t[1])}.{t[2]}"
    elif t == "zero":
        return "0"
    elif t in ["true", "false"]:
        return t
    raise ValueError("don't know how to format {}".format(t))

format_term = format_abs

def format_mu(tau):
    if isinstance(tau, list) and tau[0] == "mu":
        return f"μ{tau[1]}.{format_mu(tau[2])}"
    else:
        return format_arrow(tau)

def format_arrow(tau):
    if isinstance(tau, list) and tau[0] == "arrow":
        return "{}->{}".format(format_sum(tau[1]), format_arrow(tau[2]))
    else:
        return format_sum(tau)

def format_sum(tau):
    if isinstance(tau, list) and tau[0] == "sum":
        return "{}+{}".format(format_product(tau[1]), format_product(tau[2]))
    else:
        return format_product(tau)

def format_product(tau):
    if isinstance(tau, list) and tau[0] == "product":
        return "{}*{}".format(format_base(tau[1]), format_base(tau[2]))
    else:
        return format_base(tau)

def format_base(tau):
    if isinstance(tau, list):
        if tau[0] == "typevar":
            return tau[1]
        elif tau[0] in ['mu', 'arrow', 'sum', 'product']:
            return "({})".format(format_type(tau))
    elif tau in ["Nat", "Bool"]:
        return tau
    raise ValueError("don't know how to format {}".format(tau))

format_type = format_mu

def read_lines(prompt=""):
    """Read lines from stdin. If the file is a tty, that is, keyboard input
    from the user, then display a prompt and allow editing and history."""
    if os.isatty(sys.stdin.fileno()):
        while True:
            try:
                line = input(prompt)
            except EOFError:
                print()
                break
            yield line
    else:
        for line in sys.stdin:
            yield line

if __name__ == "__main__":
    for line in read_lines("> "):
        t = parse_term(line)
        print(t)
        print(format_term(t))
        
