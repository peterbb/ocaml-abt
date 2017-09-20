
module type COMMON = sig
    type t
    val eq : t -> t -> bool
    val compare : t -> t -> int
    val to_string : t -> string
end

module type SORT = sig
    include COMMON
end

module type OPERATOR = sig
    include COMMON
    module S : SORT

    val of_string : string -> S.t -> t option
    val arity : t -> S.t list * (S.t list * S.t list * S.t) list * S.t
end

module type VAR = sig
    include COMMON
    type sort

    val fresh : string -> sort -> t
    val global : string -> sort -> t
    val sort : t -> sort
end

module type NAME = sig
    include COMMON
    type sort

    val fresh : string -> sort -> t
    val global : string -> sort -> t
    val sort : t -> sort
end

module type LANGUAGE = sig
    module S : SORT
    module O : OPERATOR with module S = S
end

module Symbols (S : SORT) = struct
    type sort = S.t

    type t =
        | Gensym of int * string * S.t
        | Named  of string * S.t

    let eq x y = match x, y with
        | Gensym (i, _, _), Gensym (j, _, _) -> i = j
        | Named (x, s), Named (y, t) -> x = y && S.eq s t
        | Gensym _, Named _ -> false
        | Named _, Gensym _ -> false

    let lex x y = if x = 0 then y else x

    let compare x y = match x, y with
        | Gensym (i, _, _), Gensym (j, _, _) -> compare i j
        | Named (x, s), Named (y, t) -> lex (compare x y) (S.compare s t)
        | Named _, Gensym _ -> -1
        | Gensym _, Named _ -> +1


    let sort = function
        | Gensym (_, _, s) | Named (_, s) -> s

    let global n s = Named (n, s)

    let fresh =
        let k = ref 0 in
        fun name sort ->
            let i = !k in
            k := i + 1;
            Gensym (i, name, sort)

    let refresh = function 
        | Gensym (_, name, sort) | Named (name, sort) -> fresh name sort

    let to_string = function
        | Gensym (i, n, _) -> Printf.sprintf "%s%d" n i
        | Named (n, _) -> n
end

module type S = sig
    include LANGUAGE

    module V : VAR with type sort = S.t
    module N : NAME with type sort = S.t

    type 'a loc = Loc.t * 'a

    type names = N.t loc list
    type vars = V.t loc list

    type t
    type 'a abs
    type 'a view =
        | Var of V.t loc
        | App  of O.t loc * names * 'a abs list

    val unfold : t -> t view
    val open_abs : t abs -> names * vars * t
    val subst_abs : t list -> t abs -> names * t
    val rename_abs : names -> t abs -> vars * t
    val subst_rename_abs : names -> t list -> t abs -> t
    val fold : t view -> t
    val abs : names -> vars -> t -> t abs
    val subst : t -> V.t -> t -> t
    val sort : t -> S.t
end

module Make (L : LANGUAGE) = struct
    open Error

    include L

    module V = Symbols(S)
    module N = V

    type 'a loc = Loc.t * 'a

    type names = N.t loc list
    type vars = V.t loc list

    type var =
        | Free  of V.t
        | Bound of int * S.t

    type 'a abs = names * vars * 'a

    module I = struct
        type t =
            | Var   of var loc
            | App   of O.t loc * var loc list * t abs list
    end

    module E = struct
        type 'a view =
            | Var of V.t loc
            | App of O.t loc * names * 'a abs list
    end 

    include I
    include E

    let sort = function
        | I.Var (_, Bound (_, s)) -> s
        | I.Var (_, Free x) -> V.sort x
        | App ((_, o), _, _) ->
            match O.arity o with (_, _, s) -> s

    let check_index (loc, name) (expected : S.t) =
        let given = N.sort name in
        if not (S.eq given expected) then
            error ~loc "wrong sort"

    let check_indexes indexes sorts =
        let rec loop = function
            | [], [] -> ()
            | (name :: indexes), (sort :: sorts) ->
                check_index name sort;
                loop (indexes, sorts)
            | [], _ :: _ ->
                error "not enough indexes"
            | _ :: _, [] ->
                error "too many indexes"
        in loop (indexes, sorts)

    let loc = function
        | I.Var (l, _) | I.App ((l, _), _, _) -> l

    let arg_loc = function
        | [], ((l, _) :: _), body ->
            Loc.join l (loc body)
        | ((l, _) :: _), _, body ->
            Loc.join l (loc body)
        | [], [], body ->
            loc body 

    let check_arg (ns, xs, body) (ns_sorts, xs_sorts, body_sort) =
        let loc = loc body in

        let check_binder ~sort ~err bs bs_sorts =
            let rec loop bs bs_sorts =
                match bs, bs_sorts with
                | [], [] -> ()
                | ((loc, b) :: bs), (expected :: bs_sorts) ->
                    loop bs bs_sorts;
                    let given = sort b in
                    if not (S.eq given expected) then
                        (err loc b given expected)
                | [], (_ :: _) ->
                    error ~loc "argument should have more binders"
                | (_ :: _), [] ->
                    error ~loc "argument has too many binders"
            in loop bs bs_sorts
        in
        let name_binder_sort_error loc name given expected =
            error ~loc ""
        in

        let var_binder_sort_error loc var given expected =
            error ~loc ""
        in
        check_binder ~sort:N.sort ~err:name_binder_sort_error  ns ns_sorts;
        check_binder ~sort:V.sort ~err:var_binder_sort_error xs xs_sorts;
        if not (S.eq (sort body) body_sort) then
            error ~loc "terms is of sort %s, but should be of sort %s."
                (S.to_string (sort body)) (S.to_string body_sort)

    let valence_to_string (ss0, ss1, s) =
        let list _start _end = function
            | [] -> ""
            | xs ->
                let xs = List.map S.to_string xs in
                let xs = String.concat " " xs in
                _start ^ xs ^ _end
        in
        Printf.sprintf "%s%s%s"
            (list "{" "}" ss0)
            (list "(" ")" ss1)
            (S.to_string s)

    let check_args ~loc ~o args valences =
        let rec loop = function
            | [], [] -> ()
            | arg::args, valence::valences ->
                check_arg arg valence;
                loop (args, valences)
            | [], (_ :: _ as valences) ->
                let plural = match valences with [_] -> "" | _ -> "s" in
                let valences = List.map valence_to_string valences in
                error ~loc
                    "operator '%s' is missing argument%s of valence%s %s."
                    (O.to_string o) plural plural
                    (String.concat " " valences)
            | arg :: _, [] ->
                let loc = arg_loc arg in
                error ~loc "operator '%s' is applied to %d arguments"
                    (O.to_string o)
                    (List.length args)
        in loop (args, valences)

    let unfold = function
        | I.App (o, ns, args) ->
            let f = function
                | (l, Free x) -> (l, x)
                | (_, Bound _) -> assert false
            in
            E.App (o, List.map f ns, args)
        | I.Var (l, Free x) ->
            E.Var (l, x)
        | I.Var (_, Bound _) -> assert false

    let fold = function
        | E.Var (l, x) ->
            I.Var (l, Free x)
        | E.App ((loc, o), ns, args) ->
            let (index_sorts, valences, _) = O.arity o in
            check_indexes ns index_sorts;
            check_args ~loc ~o args valences;
            I.App ((loc, o), List.map (fun (l, n) -> l, Free n) ns, args)

    (* [apply subst term], where [subst] is a list of n closed
     * terms, and [term] is a term with n free de bruijn indexes.  *)
    let apply subst =
        let lookup index x = List.nth subst (x - index) in

        let rec subst_tm index = function
            | I.Var (_, Free _) as x -> x
            | I.Var (_, Bound (i, _)) as x ->
                begin match lookup index i with
                | t -> t
                | exception Invalid_argument _ -> x
                end
            | I.App (o, ns, args) ->
                let ns = List.map (subst_name index) ns in
                let args = List.map (subst_args index) args in
                I.App (o, ns, args)
        and subst_name index = function
            | _, Free _ as n -> n
            | _, Bound (i, _) as n ->
                begin match lookup index i with
                | I.Var (l, x) -> (l, x)
                | exception Invalid_argument _ -> n
                | I.App _ -> assert false
                end
        and subst_args index (names, args, body) = 
            let index = index + List.length names + List.length args in
            (names, args, subst_tm index body)

        in subst_tm 0

    let check_term term expected = 
        let given = sort term in
        if not (S.eq expected given) then
            error ~loc:(loc term)
                "term has wrong sort. expected %s, got %s"
                (S.to_string expected) (S.to_string given)

    let subst_rename_abs names terms (name_binders, term_binders, body) =
        let name_sorts = List.map (fun (_, n) -> N.sort n) name_binders in
        let term_sorts = List.map (fun (_, x) -> V.sort x) term_binders in
        check_indexes names name_sorts;
        List.iter2 check_term terms term_sorts;
        let f (l, x) = I.Var (l, Free x) in
        let sigma = List.rev ((List.map f names) @ terms) in
        apply sigma body

    let refresh_all = List.map (fun (l, x) -> (l, V.refresh x))

    let open_abs ((names, args, _) as body) =
        let names = refresh_all names in
        let args = refresh_all args in
        let vals = List.map (fun (l, x) -> I.Var (l, Free x)) args in
        (names, args, subst_rename_abs names vals body)
        
    let subst_abs terms ((names, _, _) as body) =
        let names = refresh_all names in
        (names, subst_rename_abs names terms body)

    let rename_abs names ((_, args, _) as body)= 
        let args = refresh_all args in
        let vals = List.map (fun (l, x) -> I.Var (l, Free x)) args in
        (args, subst_rename_abs names vals body)

    let abs (names : names) (vars : vars) body =
        let find x =
            let rec loop index = function
            | [] -> raise Not_found
            | (_, y) :: _ when V.eq x y -> index
            | _ :: xs -> loop (index + 1) xs
            in loop 0
        in

        let rec abs_tm index = function
            | I.Var (_, Bound _) as tm -> tm
            | I.Var (l, Free x) as tm ->
                begin match find x vars with
                | i -> I.Var (l, Bound (i + index, V.sort x))
                | exception Not_found -> tm
                end
            | I.App (o, ns, args) ->
                let ns = List.map (abs_ns index) ns in
                let args = List.map (abs_arg index) args in
                I.App (o, ns, args)
        and abs_ns index = function
            | _, Bound _ as x -> x
            | l, Free x as v ->
                begin match find x names with
                | i -> (l, Bound (i + index, V.sort x))
                | exception Not_found -> v
                end
        and abs_arg index = function
            | (names', vars', body) ->
                let index = index + List.length names' + List.length vars' in
                (names', vars', abs_tm index body)
        in (names, vars, abs_tm 0 body)

    let subst term x =
        let rec subst = function
            | I.Var (_, Free y) when V.eq x y ->
                term
            | I.Var _ as y ->
                y
            | I.App (o, ns, args) ->
                I.App (o, ns, List.map subst_arg args)
        and subst_arg (names, args, body) = 
            (names, args, subst body)
        in
        check_term term (V.sort x);
        subst
end

