%token <Loc.t * string> SYM
%token EOF
%token LPAR RPAR LBRACE RBRACE LBRACK RBRACK
%token SEMICOLON DOT

%{ open Syntax %}

%start <Syntax.t list> slurp
%%

slurp:
    | es = list(expression); EOF
        { es }

expression:
    | s = SYM; args = args2
        { Expr (s, fst args, snd args) }
    | LBRACE; x = infix; RBRACE
        { let (op, args) = x in
          Expr (op, [], args) }

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
    | e = expression
        { ([], e) }
    | xs = nonempty_list(SYM); DOT; e = expression
        { (xs, e) }

infix:
    | e0 = expression; o = SYM; e1 = arg
        { (o, [ [], e0; e1]) }
