
open Format

module Make (L : Language.S) = struct
    
    let list ppf ?(nothing_if_nil=false) ?(init="") ?(final="") ?(sep=" ") f xs =
        match xs with
        | [] ->
            if not nothing_if_nil then
                fprintf ppf "%s%s" init final
        | x :: xs ->
            fprintf ppf "%s%a" init f x;
            List.iter (fun x -> fprintf ppf "%s%a" sep f x) xs;
            fprintf ppf "%s" final

    let l ppf s = fprintf ppf "%s" s
    let var ppf (_, x) = l ppf (L.V.to_string x)
    let name ppf (_, n) = l ppf (L.N.to_string n)

    let rec print ppf tm =
        match L.unfold tm with
        | L.Var x ->
            var ppf x
        | L.App ((_, op), ns, args) ->
            l ppf (L.O.to_string op);
            list ppf ~nothing_if_nil:true ~init:"[" ~sep:", " ~final:"]" name ns;
            list ppf ~nothing_if_nil:true ~init:"(" ~sep:"; " ~final:")" print_abs args

    and print_abs ppf abs = 
        let (ns, xs, body) = L.open_abs abs in
        list ppf name ns;
        list ppf var xs;
        if ns <> [] || xs <> [] then
            fprintf ppf ". ";
        print ppf body

end
