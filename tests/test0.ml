open Printf

module L = struct
    module S = struct
        type t = Expr

        let eq Expr Expr = true
        let compare Expr Expr = 0
        let to_string Expr = "expr"
    end

    module O = struct
        module S = S
        type t =
            | Num of int
            | Add

        let of_string str _sort = match str with
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

    module F = struct
        open Abt.Language
        let fixity = function
            | "+" -> Infix
            | _ -> Nofix
    end
end

module A = Abt.Language.Make(L)
module U = Abt.Util(A)


let () = printf "parsing %s.\n" (Sys.argv.(1))
let p = U.parse_filename ~sort:L.S.Expr (Sys.argv.(1))
let () = List.iter (fun e -> U.print e; Format.printf "\n") p


