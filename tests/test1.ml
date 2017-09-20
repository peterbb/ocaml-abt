
(* Untyped lambda calculus. *)
module L = struct
    module S = struct
        type t = E

        let eq E E = true
        let compare E E = 0
    
        let to_string E = "E"
    end

    module O = struct
        type t = Lam | Ap

        let eq = (=)

        module S = S
        open S

        let arity = function
            | Lam -> [], [[], [E], E], E
            | Ap -> [], [[], [], E; [], [], E], E

        let compare x y = match x, y with
            | Lam, Lam | Ap, Ap -> 0
            | Lam, Ap -> +1
            | Ap, Lam -> -1

        let of_string str _sort = match str with
            | "ap" -> Some Ap
            | "lam" -> Some Lam
            | _ -> None

        let to_string = function
            | Ap -> "ap"
            | Lam -> "lam"
    end

    module F = struct
        open Abt.Language
        let fixity = function
            | _ -> Nofix
    end
end

module A = Abt.Language.Make(L)
module U = Abt.Util(A)

let program =
        U.parse_filename ~sort:L.S.E Sys.argv.(1)
let () = List.iter (fun p -> U.print p; Format.printf "\n")  program


