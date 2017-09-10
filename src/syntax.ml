
type t =
    Expr of string * string list * (string list * t) list


module Inject (L : Language.S) = struct

    type sort = L.S.t
    type 'a ctx = (string * sort * 'a) list
    type nctx = L.N.t ctx
    type vctx = L.V.t ctx


    (* 
        input:
            names : string list
            sorts : sort list
            make  : string -> sort -> 'a
            ctx   : 'a ctx
        output:
            vars  : 'a list
            ctx   : 'a ctx
    *)
    let process_binder names sorts make ctx =
        let rec loop = function
            | [], [] -> ([], ctx)
            | (x :: xs), (s :: ss) ->
                let x' = make x s in
                let (vars, ctx) = loop (xs, ss) in
                (x' :: vars), ((x, s, x') :: ctx)
            | [], _::_ ->
                failwith "too many binders"
            | _::_, [] ->
                failwith "not enough binders"
        in loop (names, sorts)

    let lookup x s (ctx : 'a ctx) =
        let rec loop = function
            | (y, t, z) :: _ when x = y && (L.S.eq s t) ->
                z
            | _ :: rest -> loop rest
            | [] -> raise Not_found
        in loop ctx

    let rec to_abt (nctx : nctx) (vctx : vctx) sort = function
        | Expr (o, [], []) ->
            begin match lookup o sort vctx with
            | v ->
                L.fold (L.Var v)
            | exception Not_found ->
                match L.O.of_string o sort with
                | Some o -> L.fold (L.App (o, [], []))
                | None ->
                    L.fold (L.Var (L.V.global o sort))
            end
        | Expr (o, names, args) ->
            begin match L.O.of_string o sort with
            | None -> failwith "not an operator"
            | Some o ->
                let (_, args_sorts, _) = L.O.arity o in
                let names = List.map (name_to_abt nctx sort) names in
                let args = List.map2 (arg_to_abt nctx vctx) args_sorts args in
                L.fold (L.App (o, names, args))
            end

    and name_to_abt nctx sort x =
        match lookup x sort nctx with
        | v -> v
        | exception _ -> L.N.global x sort

    and arg_to_abt nctx vctx (name_sorts, var_sorts, sort) (vars, body) =
        let (names, vars) = split vars (List.length name_sorts) in
        let namesx, nctx = process_binder names name_sorts L.N.fresh nctx in
        let varsx, vctx = process_binder vars var_sorts L.V.fresh vctx in
        L.abs namesx varsx (to_abt nctx vctx sort body)

    and split xs n = match xs, n with
        | xs, 0 -> [], xs
        | (x :: xs), n when n > 0 ->
            let (xs, ys) = split xs (n - 1) in
            (x :: xs, ys)
        | [], n when n > 0 -> 
            failwith "not enough variables"
        | _ -> assert false
end
