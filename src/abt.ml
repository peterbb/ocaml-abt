
module Language = Language


module Util (L : Language.S) = struct
    module Reader  = Syntax.Inject(L)
    module Printer = Printer.Make(L)

    let check_correct_sort sort abt = 
        if L.S.eq (L.sort abt) sort then
            ()
        else
            failwith "wrong sort"


    module I = Parser.MenhirInterpreter

    let succeed xs = xs

    let fail lexbuf = function
        | I.HandlingError env ->
            begin match I.top env with
            | Some (I.Element (state, _, _start, _end))  ->
                let state = I.number state in
                let loc = Loc.{ _start; _end } in
                Error.error ~loc "%s" (Parser_error.message state)
            | None ->
                let _start = lexbuf.Lexing.lex_start_p
                and _end = lexbuf.Lexing.lex_curr_p in
                let loc = Loc.{ _start ; _end } in
                Error.error ~loc
                    "parse error: expected an expression, but found '%s'."
                    (Lexing.lexeme lexbuf)
            end
        | _ -> assert false

    let parse_loop lexbuf result =
        let supplier = I.lexer_lexbuf_to_supplier Lexer.token lexbuf in
        I.loop_handle succeed (fail lexbuf) supplier result

    let slurp lexbuf =
        parse_loop lexbuf (Parser.Incremental.slurp lexbuf.lex_curr_p)
        
    let parse_file lexbuf sort =
        let ps = slurp lexbuf |> List.map (Reader.to_abt [] [] sort)
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
