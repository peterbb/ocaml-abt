
module Language = Language


module Util (L : Language.S) : sig

    val parse_file : sort:L.S.t -> Lexing.lexbuf-> L.t list

    val parse_filename : sort:L.S.t -> string -> L.t list

    val print : L.t -> unit
end = struct
    module Reader  = Reader.Make(L)
    module Printer = Printer.Make(L)

    let check_correct_sort sort abt = 
        if L.S.eq (L.sort abt) sort then
            ()
        else
            failwith "wrong sort"

        
    let parse_file ~sort lexbuf =
        let rec loop acc =
            match Reader.parse ~sort lexbuf with
            | None -> List.rev acc
            | Some e ->
                loop (e :: acc)
        in loop []

    let parse_filename ~sort filename =
        let ch = open_in filename in
        try 
            let lexbuf = Lexing.from_channel ch in
            lexbuf.lex_curr_p <-
                { lexbuf.lex_curr_p with pos_fname = filename };
            let program = parse_file ~sort lexbuf in
            close_in ch;
            program
        with
        | e -> (close_in ch; raise e)

    let print = Printer.print Format.std_formatter
end
