
module Language = Language


module Util (L : Language.S) = struct
    module Reader  = Syntax.Inject(L)
    module Printer = Printer.Make(L)

    let check_correct_sort sort abt = 
        if L.S.eq (L.sort abt) sort then
            ()
        else
            failwith "wrong sort"

    let parse_expression lexbuf sort =
        let abt = Parser.expression Lexer.token lexbuf
                  |> Reader.to_abt [] [] sort in
        check_correct_sort sort abt;
        abt

    let parse_file lexbuf sort =
        let ps = Parser.slurp Lexer.token lexbuf
                 |> List.map (Reader.to_abt [] [] sort)
        in 
        List.iter (check_correct_sort sort) ps;
        ps

    let parse_filename filename sort =
        let ch = open_in filename in
        try 
            let lexbuf = Lexing.from_channel ch in
            lexbuf.lex_curr_p <-
                { lexbuf.lex_curr_p with pos_fname = filename };
            let program = parse_file lexbuf sort in
            close_in ch;
            program
        with
        | e -> (close_in ch; raise e)

    let print = Printer.print Format.std_formatter
end
