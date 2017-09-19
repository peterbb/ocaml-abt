
open Format

module Make (L : Language.S) = struct
    let name = L.N.to_string
    let var = L.V.to_string

    let name' (_, x) = name x
    let var' (_, x) = var x
    
    let rec print ppf tm =
        match L.unfold tm with
        | L.Var (_, x) ->
            fprintf ppf "%s" (var x)
        | L.App ((_, op), ns, args) ->
            fprintf ppf "%s" (L.O.to_string op);
            begin match ns with
                | [] -> ()
                | [_, n] ->
                    fprintf ppf "[%s]" (name n)
                | (_, n)::ns ->
                    fprintf ppf "[%s" (name n);
                    List.iter (fun (_, n) -> fprintf ppf " %s" (name n)) ns;
                    fprintf ppf "]"
            end;
            begin match args with
            | [] -> ()
            | [abs] ->
                fprintf ppf "(";
                print_abs ppf "" abs;
                fprintf ppf ")"
            | (abs :: abss) ->
                fprintf ppf "(";
                print_abs ppf "" abs;
                List.iter (print_abs ppf "; ") abss;
                fprintf ppf ")"
            end

    and print_abs ppf prefix abs = 
        fprintf ppf "%s" prefix;
        let (ns, xs, body) = L.open_abs abs in
        match List.map name' ns @ List.map var' xs with
        | [] ->
            print ppf body
        | [x] ->
            fprintf ppf "%s. " x;
            print ppf body
        | (x :: xs) ->
            fprintf ppf "%s" x;
            List.iter (fun x -> fprintf ppf " %s" x) xs;
            fprintf ppf ". ";
            print ppf body
end
