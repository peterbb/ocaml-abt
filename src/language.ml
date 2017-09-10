
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
    module O : OPERATOR
        with module S = S
end

module Symbols (S : SORT) = struct
    type sort = S.t

    type t =
        | S of int * string * S.t
        | G of string * S.t

    let eq x y = match x, y with
        | (S (i, _, _)), (S (j, _, _)) -> i = j
        | (G (x, s)), (G (y, t)) -> x = y && S.eq s t
        | G _, S _ | S _, G _ -> false

    let lex x y = if x = 0 then y else x

    let compare x y = match x, y with
        | S (i, _, _), S (j, _, _) -> compare i j
        | G (x, s), G (y, t) -> lex (compare x y) (S.compare s t)
        | S _, G _ -> -1
        | G _, S _ -> +1


    let sort = function
        | S (_, _, s) | G (_, s) -> s

    let global n s = G (n, s)

    let fresh =
        let k = ref 0 in
        fun name sort ->
            let i = !k in
            k := i + 1;
            S (i, name, sort)

    let refresh = function 
        | S (_, name, sort) | G (name, sort) -> fresh name sort

    let to_string = function
        | S (i, n, _) -> Printf.sprintf "%s%d" n i
        | G (n, _) -> n
end

module type S = sig
    include LANGUAGE

    module V : VAR with type sort = S.t
    module N : NAME with type sort = S.t

    type names = N.t list
    type vars = V.t list

    type t
    type 'a abs
    type 'a view =
        | Var of V.t
        | App  of O.t * names * 'a abs list

    (* Destruction *)
    val unfold : t -> t view
    val open_abs : t abs -> names * vars * t
    val subst_abs : t list -> t abs -> names * t
    val rename_abs : names -> t abs -> vars * t
    val subst_rename_abs : names -> t list -> t abs -> t

    (* Construction *)
    val fold : t view -> t
    val abs : names -> vars -> t -> t abs

    (* Misc *)
    val subst : t -> V.t -> t -> t
    val sort : t -> S.t
end

module Make (L : LANGUAGE) = struct
    include L

    module V = Symbols(S)
    module N = V

    type names = N.t list
    type vars = V.t list

    type var =
        | Free  of V.t
        | Bound of int * S.t

    type 'a abs = N.t list * V.t list * 'a

    module I = struct
        type t =
            | Var   of var
            | App   of O.t * var list * t abs list
    end

    module E = struct
        type 'a view =
            | Var of V.t
            | App  of O.t * N.t list * 'a abs list
    end 

    include I
    include E

    let sort = function
        | I.Var (Bound (_, s)) -> s
        | I.Var (Free x) -> V.sort x
        | App (o, _, _) ->
            match O.arity o with (_, _, s) -> s

    exception Sort_error_in_var_binder of 
        { var : V.t; expected : S.t; given : S.t }
    exception Sort_error_in_name_binder of
        { var : N.t; expected : S.t; given : S.t }
    exception Sort_error_in_argument of
        { expr : unit; expected : S.t; given : S.t }

    (* This function checks whether indexes are correct,
     * and if the bindings and the body of the arguments
     * have the correct type. The bodies are assumed to be well
     * typed. *)
    let check indexes args (index_sorts, arg_valences, _) =
        (* Here 'XXX' marks where proper handeling and reporting
         * of sort errors are missing. *)
        let same_sort s0 s1 =
            if not (S.eq s0 s1) then
                failwith "not same sort" (* XXX *)
        in
        let check_arg (ns, xs, body) (ns_sorts, xs_sorts, body_sort) =
            let ns = List.map N.sort ns in
            let xs = List.map V.sort xs in
            List.iter2 same_sort ns ns_sorts; (* XXX *)
            List.iter2 same_sort xs xs_sorts; (* XXX *)
            if not (S.eq (sort body) body_sort) then
                raise (Sort_error_in_argument {
                    expr = ();
                    expected = body_sort;
                    given = sort body
                })
        in
        let indexes = List.map N.sort indexes in
        List.iter2 same_sort indexes index_sorts; (* XXX *)
        List.iter2 check_arg args arg_valences (* XXX *)

    let unfold = function
        | I.App (o, ns, args) ->
            let f = function
                | Free x -> x
                | Bound _ -> assert false
            in
            E.App (o, List.map f ns, args)
        | I.Var (Free x) ->
            E.Var x
        | I.Var (Bound _) -> assert false

    let fold = function
        | E.Var x ->
            I.Var (Free x)
        | E.App (o, ns, args) ->
            check ns args (O.arity o);
            I.App (o, List.map (fun n -> Free n) ns, args)

    let apply subst =
        let lookup index x = List.nth subst (x - index) in

        let rec subst_tm index = function
            | I.Var (Free _) as x -> x
            | I.Var (Bound (i, _)) as x ->
                begin match lookup index i with
                | t -> t
                | exception _ -> x
                end
            | I.App (o, ns, args) ->
                let ns = List.map (subst_name index) ns in
                let args = List.map (subst_args index) args in
                I.App (o, ns, args)
        and subst_name index = function
            | Free _ as n -> n
            | Bound (i, _) as n ->
                begin match lookup index i with
                | I.Var x -> x
                | I.App _ -> failwith "internal error: subst_name"
                | exception _ -> n
                end
        and subst_args index (names, args, body) = 
            let index = index + List.length names + List.length args in
            (names, args, subst_tm index body)
        in subst_tm 0

    let subst_rename_abs names values (_, _, body) =
        let f x = I.Var (Free x) in
        let sigma = List.rev ((List.map f names) @ values) in
        (* TODO: type check *)
        apply sigma body

    let refresh_all = List.map V.refresh 

    let open_abs ((names, args, _) as body) =
        let names = refresh_all names in
        let args = refresh_all args in
        let vals = List.map (fun x -> I.Var (Free x)) args in
        (names, args, subst_rename_abs names vals body)
        
    let subst_abs terms ((names, _, _) as body) =
        let names = refresh_all names in
        (names, subst_rename_abs names terms body)

    let rename_abs names ((_, args, _) as body)= 
        let args = refresh_all args in
        let vals = List.map (fun x -> I.Var (Free x)) args in
        (args, subst_rename_abs names vals body)

    let abs names vars body =

        let find x =
            let rec loop index = function
            | [] -> raise Not_found
            | y :: _ when V.eq x y -> index
            | _ :: xs -> loop (index + 1) xs
            in loop 0
        in

        let rec abs_tm index = function
            | I.Var (Bound _) as tm -> tm
            | I.Var (Free x) as tm ->
                begin match find x vars with
                | i -> I.Var (Bound (i + index, V.sort x))
                | exception Not_found -> tm
                end
            | I.App (o, ns, args) ->
                let ns = List.map (abs_ns index) ns in
                let args = List.map (abs_arg index) args in
                I.App (o, ns, args)
        and abs_ns index = function
            | Bound _ as x -> x
            | Free x as v ->
                begin match find x names with
                | i -> Bound (i + index, V.sort x)
                | exception Not_found -> v
                end
        and abs_arg index = function
            | (names', vars', body) ->
                let index = index + List.length names' + List.length vars' in
                (names', vars', abs_tm index body)
        in (names, vars, abs_tm 0 body)

    let subst term x =
        let rec subst = function
            | I.Var (Free y) when V.eq x y ->
                term
            | I.Var _ as y ->
                y
            | I.App (o, ns, args) ->
                I.App (o, ns, List.map subst_arg args)
        and subst_arg (names, args, body) = 
            (names, args, subst body)
        in subst
end

