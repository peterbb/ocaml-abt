
module Language = Language


module Util (L : Language.S) = struct
    module Reader  = Syntax.Inject(L)
    module Printer = Printer.Make(L)

    let parse_expression lexbuf =
        Parser.expression Lexer.token lexbuf
        |> Reader.to_abt [] []

    let parse_file lexbuf =
        Parser.slurp Lexer.token lexbuf
        |> List.map (Reader.to_abt [] [])

    let parse_filename filename =
        let ch = open_in filename in
        try 
            let lexbuf = Lexing.from_channel ch in
            lexbuf.lex_curr_p <-
                { lexbuf.lex_curr_p with pos_fname = filename };
            let program = parse_file lexbuf in
            close_in ch;
            program
        with
        | e -> (close_in ch; raise e)

    let print = Printer.print Format.std_formatter
end
