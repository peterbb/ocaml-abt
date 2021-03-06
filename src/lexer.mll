
{
    open Lexing 
    open Parser

    let loc lexbuf =
        Loc.{
            _start = lexeme_start_p lexbuf;
            _end = lexeme_end_p lexbuf;
        }
}


let white = " " | "\t"
let newline = "\r" | "\n" | "\r\n"

let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']
let other0 = '_' | '-' | '@' | '+' | '#' | '!' | '%' | '&' | '^' | '='
let other1 = '?' | '<' | '>' | ":" | "\\" | '/'
let sym = (digit | letter | other0 | other1)+


rule token = parse
    | eof           { EOF }
    | white+        { token lexbuf }
    | newline       { new_line lexbuf; token lexbuf }
    | "(*"          { comment lexbuf; token lexbuf }

    | "."           { DOT }
    | ";"           { SEMICOLON }
    | "("           { LPAR }
    | ")"           { RPAR }
    | "["           { LBRACK }
    | "]"           { RBRACK }

    | sym as s      { SYM (loc lexbuf, s) }

and comment = parse
    | "*)"          { () }
    | "(*"          { comment lexbuf; comment lexbuf }
    | eof           { failwith "eof in comment" }
    | newline       { new_line lexbuf; comment lexbuf }
    | _             { comment lexbuf }

