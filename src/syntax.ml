
type t =
    Expr of string * string list * (string list * t) list

module Inject (L : Language.S) = struct

    let rec to_abt nctx vctx = function
        | Expr (o, [], []) ->
            begin match List.assoc o vctx with
            | v ->
                L.fold (L.Var v)
            | exception Not_found ->
                match L.L.O.of_string o with
                | Some o -> L.fold (L.App (o, [], []))
                | None ->
                    (* L.fold (L.Var (L.V.global o)) *) 
                    failwith "to_abt not implemented"
            end
        | Expr (o, names, args) ->
            begin match L.L.O.of_string o with
            | None -> failwith "not an operator"
            | Some o ->
                let (_, args_sorts, _) = L.L.O.arity o in
                let names = List.map (name_to_abt nctx) names in
                let args = List.map2 (arg_to_abt nctx vctx) args_sorts args in
                L.fold (L.App (o, names, args))
            end

    and name_to_abt nctx x =
        match List.assoc x nctx with
        | v -> v
        | exception _ -> failwith "name_to_abt"

    and arg_to_abt nctx vctx (name_sorts, var_sorts, _) (vars, body) =
        let (names, vars) = split vars (List.length name_sorts) in
        let namesx = List.map2 L.N.fresh names name_sorts in
        let varsx = List.map2 L.V.fresh vars var_sorts in
        let nctx = (List.combine names namesx) @ nctx in
        let vctx = (List.combine vars varsx) @ vctx in
        L.abs namesx varsx (to_abt nctx vctx body)

    and split xs n = match xs, n with
        | xs, 0 -> [], xs
        | (x :: xs), n when n > 0 ->
            let (xs, ys) = split xs (n - 1) in
            (x :: xs, ys)
        | [], n when n > 0 -> 
            failwith "not enough variables"
        | _ -> assert false
end
