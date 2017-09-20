
exception Error of string

let error ?loc fmt =
    let loc = match loc with
                | None -> ""
                | Some l -> Loc.to_string l ^ ":\n"
    in
    let k s = raise (Error (loc ^ s)) in
    Format.ksprintf k fmt

