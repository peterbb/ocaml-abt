%token <Loc.t * string> SYM OP
%token EOF
%token LPAR RPAR LBRACK RBRACK
%token SEMICOLON DOT

%{ open Surface_syntax %}

%start <Surface_syntax.toplevel> toplevel
%%

toplevel:
    | EOF
        { Eof }
    | e = expr; SEMICOLON
        { Expr e }

expr:
    | e0 = expr0; es = list(pair(OP, expr0))
        { I (e0, es) }

expr0:
    | head = SYM; args = args2
        { E { head; indexes = fst args;  args = snd args} }
    | LPAR; e = expr; RPAR
        { e }


args2:
    | args = args1
        { ([], args) }
    | LBRACK; xs = list(SYM); RBRACK; args = args1
        { (xs, args) }

args1:
    | 
        { [] }
    | LPAR; xs = separated_list(SEMICOLON, arg); RPAR
        { xs }

arg:
    | e = expr
        { ([], e) }
    | xs = nonempty_list(SYM); DOT; e = expr
        { (xs, e) }

