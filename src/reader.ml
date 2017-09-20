
open Surface_syntax

module I = Parser.MenhirInterpreter

module Make (L : Language.S) = struct
    open Error

    type sort = L.S.t
    type 'a ctx = (string * sort * 'a) list
    type nctx = L.N.t ctx
    type vctx = L.V.t ctx

    let lookup (x : string) (x_sort : sort) (ctx : 'a ctx) : 'a =
        let rec loop = function
            | (y, y_sort, z) :: _ when x = y && (L.S.eq x_sort y_sort) ->
                z
            | _ :: rest -> loop rest
            | [] -> raise Not_found
        in loop ctx

    (* Take a binder list and associated sorts and construct
     * the internalized version of the binder list and 
     * extend the given context. *)
    let process_binder
            (names : name list)
            (sorts : sort list)
            (make  : string -> sort -> 'a)
            (ctx : 'a ctx)
            : 'a loc list * 'a ctx
         =
        let rec loop = function
            | [], [] -> ([], ctx)
            | ((l, x) :: xs), (s :: ss) ->
                let x' = make x s in
                let (vars, ctx) = loop (xs, ss) in
                ((l, x') :: vars), ((x, s, x') :: ctx)
            | [], _::_ ->
                error "not enough binders"
            | (loc, x) :: xs, [] ->
                let xs = List.map snd xs in
                error ~loc
                    "Too many binders.\nPossible solution: remove the variable(s) %s."
                    (String.concat " " (x :: xs))
                    
        in loop (names, sorts)

    (* Split a list in two, such that the first list contains [n] elements. *)
    let rec split (xs : 'a loc list) (n : int) : 'a loc list * 'a loc list =
        match xs, n with
        | xs, 0 -> [], xs
        | (x :: xs), n when n > 0 ->
            let (xs, ys) = split xs (n - 1) in
            (x :: xs, ys)
        | [], n when n > 0 -> 
            [], []
        | _ -> assert false

    let rec loc_of_arg = function
        | ((loc, _) :: _), _ -> loc
        | [], E {head = (loc, _)}  -> loc
        | [], I (e, _) -> loc_of_arg ([], e)

    let rec to_abt nctx vctx sort = function
        | E { head=(l, o); indexes=[]; args=[] } ->
            begin match lookup o sort vctx with
            | v ->
                L.fold (L.Var (l, v))
            | exception Not_found ->
                match L.O.of_string o sort with
                | Some o -> L.fold (L.App ((l, o), [], []))
                | None ->
                    L.fold (L.Var (l, L.V.global o sort))
            end
        | E { head=(loc, o); indexes; args} ->
            begin match L.O.of_string o sort with
            | None -> error ~loc "'%s' is not an operator." o
            | Some o ->
                let (_, valences, _) = L.O.arity o in
                let indexes = List.map (name_to_abt nctx sort) indexes in
                let args = args_to_abt nctx vctx args valences in
                L.fold (L.App ((loc, o), indexes, args))
            end
        | I _ -> assert false

    and name_to_abt nctx sort (l, x) =
        match lookup x sort nctx with
        | v -> (l, v)
        | exception Not_found -> (l, L.N.global x sort)

    and args_to_abt nctx vctx args valences =
        match args, valences with
        | [], _ -> []
        | (arg :: args), (valence :: valences)  ->
            (arg_to_abt nctx vctx valence arg)
            :: args_to_abt nctx vctx args valences
        | (arg :: rest), [] ->
            let loc = loc_of_arg arg in
            error ~loc "Too many arguments."
        
    and arg_to_abt nctx vctx (name_sorts, var_sorts, sort) (binders, body) =
        let (names, vars) = split binders (List.length name_sorts) in
        let names, nctx = process_binder names name_sorts L.N.fresh nctx in
        let vars, vctx = process_binder vars var_sorts L.V.fresh vctx in
        L.abs names vars (to_abt nctx vctx sort body)


    let rec remove_ops abt =
        let remove_ops_arg (xs, x) = (xs, remove_ops x) in
        match abt with
        | E { head; indexes; args } ->
            let args = List.map remove_ops_arg args in
            E { head; indexes; args }
        | I (e, []) ->
            remove_ops e
        | I (e0, [o, e1]) ->
            let e0 = remove_ops e0 in
            let e1 = remove_ops e1 in
            E { head = o; indexes = []; args = [[], e0; [], e1]}
        | I _ -> failwith "remove_ops not implemented"

    let rec parse_loop lexbuf (checkpoint : toplevel I.checkpoint) : expr option =
        let open Lexing in
        match checkpoint with
        | I.Shifting _
        | I.AboutToReduce _ ->
            let checkpoint = I.resume checkpoint in
            parse_loop lexbuf checkpoint
        | I.Accepted (Expr e) -> Some (remove_ops e)
        | I.Accepted Eof -> None
        | I.Rejected -> assert false
        | I.InputNeeded env ->
            let token = Lexer.token lexbuf
            and startp = lexbuf.lex_start_p 
            and endp = lexbuf.lex_curr_p in
            let token = match token with
                | Parser.SYM (l, o) as t->
                    begin match L.F.fixity o with
                    | Infix -> Parser.OP (l, o)
                    | Nofix -> t
                    end
                | t -> t
            in
            let checkpoint = I.offer checkpoint (token, startp, endp) in
            parse_loop lexbuf checkpoint
        | I.HandlingError _env ->
            Error.error "parse error"

    let parse_expr lexbuf =
        let open Lexing in
        let init = Parser.Incremental.toplevel lexbuf.lex_curr_p in
        parse_loop lexbuf init

    let parse ~sort lexbuf =
        match parse_expr lexbuf with
        | Some raw -> Some (to_abt [] [] sort raw)
        | None -> None
end
