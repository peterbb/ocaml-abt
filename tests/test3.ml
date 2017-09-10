
module Spec = struct
    module S = struct
        type t = E | T | J

        let to_string = function
            | E -> "E"
            | T -> "T"
            | J -> "J"

        let compare x y = 
            let inj = function E -> 0 | T -> 1 | J -> 2
            in compare (inj x) (inj y)

        let eq x y = compare x y = 0
    end

    module O = struct
        type t =
            | Fn | Ap | TFn | TAp
            | Ar | All
            | Check

        let compare x y =
            let inj = function
                | Fn -> 0 | Ap -> 1 | TFn -> 2 | TAp -> 3
                | Ar -> 4 | All -> 5 | Check -> 6
            in compare (inj x) (inj y)

        let eq = (=)

        let to_string = function
            | Fn -> "\\"
            | Ap -> "@"
            | TFn -> "\\\\"
            | TAp -> "#"
            | Ar -> "->"
            | All -> "!"
            | Check -> ":"

        module S = S
        open S

        let of_string str sort = match str, sort with
            | "\\", E  -> Some Fn
            | "@", E  -> Some Ap
            | "\\\\", E -> Some TFn
            | "#", E -> Some TAp
            | "->", T -> Some Ar
            | "!", T -> Some All
            | ":", J -> Some Check
            | _ -> None

        let abs x y = [], [x], y
        let only x = [], [], x

        let arity = function
            | Fn -> [], [only T; abs E E], E
            | Ap -> [], [only E; only E], E
            | TFn -> [], [abs T E], E
            | TAp -> [], [only E; only T], E
            | Ar -> [], [only T; only T], T
            | All -> [], [abs T T], T
            | Check -> [], [only E; only T], J
    end
end

module L = Abt.Language.Make(Spec)
module U = Abt.Util(L)

let program =
    U.parse_filename Sys.argv.(1) Spec.S.J
let () = List.iter U.print program



