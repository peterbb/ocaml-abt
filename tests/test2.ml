
(* Untyped lambda calculus. *)
module L = struct
    module S = struct
        type t = E | T

        let eq = (=)

        let compare x y = match x, y with
            | E, E | T, T -> 0
            | E, T -> -1
            | T, E -> +1
    
        let to_string = function E -> "E" | T -> "T"
    end

    module O = struct
        type t = Lam | Ap | Int | Arr

        let eq = (=)

        let compare x y =
            let f = function
                | Lam -> 0
                | Ap -> 1
                | Int -> 2
                | Arr -> 3
            in compare (f x) (f y)

        module S = S
        open S

        let arity = function
            | Lam -> [], [[], [], T; [], [E], E], E
            | Ap -> [], [[], [], E; [], [], E], E
            | Int -> [], [], T
            | Arr -> [], [[], [], T; [], [], T], T


        let of_string str sort = match str, sort with
            | "ap", E -> Some Ap
            | "lam", E -> Some Lam
            | "->", T -> Some Arr
            | "int", T -> Some Int
            | _ -> None

        let to_string = function
            | Ap -> "ap"
            | Lam -> "lam"
            | Arr -> "->"
            | Int -> "int"
    end

    module F = struct
        open Abt.Language
        let fixity = function
            | "->" -> Infix
            | _ -> Nofix
    end
end

module A = Abt.Language.Make(L)
module U = Abt.Util(A)

let program = U.parse_filename ~sort:L.S.E Sys.argv.(1)
let () = List.iter (fun p -> U.print p; Format.printf "\n")  program

