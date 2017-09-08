open Printf

module L = struct
    module S = struct
        type t = Expr

        let eq Expr Expr = true
        let compare Expr Expr = 0
    end

    module O = struct
        module S = S
        type t =
            | Num of int
            | Add

        let of_string = function
            | "+" -> Some Add
            | x -> Some (Num (int_of_string x))

        let to_string = function
            | Num n -> Printf.sprintf "%d" n
            | Add -> "+"

        let compare x y = match x, y with
            | Add, Add -> 0
            | Num n, Num m -> compare n m
            | Add, Num _ -> -1
            | Num _, Add -> 1

        let eq x y = compare x y = 0

        open S
        let arity = function
            | Num _ -> [], [], Expr
            | Add -> [], [[],[], Expr ; [], [], Expr ], S.Expr
    end
end

module A = Abt.Language.Make(L)
module U = Abt.Util(A)

let parse filename =
    let ich = open_in filename in
    let lexbuf = Lexing.from_channel ich in
    let program = U.parse_file lexbuf in
    close_in ich;
    program


let () = printf "parsing %s.\n" (Sys.argv.(1))
let p = parse (Sys.argv.(1))
let () = List.iter (fun e -> U.print e; Format.printf "\n") p

