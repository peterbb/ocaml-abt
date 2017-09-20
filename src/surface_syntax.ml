
type 'a loc = Loc.t * 'a

type name = string loc

type expr = 
    | I of expr * (name * expr) list
    | E of {
        head : name;
        indexes : name list;
        args : (name list * expr) list;
    }

type toplevel =
    | Eof
    | Expr of expr
    
