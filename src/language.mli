module type COMMON = sig
    type t
    val eq : t -> t -> bool
    val compare : t -> t -> int
end

module type SORT = sig
    include COMMON
end

module type OPERATOR = sig
    include COMMON
    val of_string : string -> t option
    val to_string : t -> string

    module S : SORT
    val arity : t -> S.t list * (S.t list * S.t list * S.t) list * S.t
end

module type VAR = sig
    include COMMON
    type sort

    (* Note that [fresh x s] is different from [fresh x s]. *)
    val fresh : string -> sort -> t
    val global : string -> sort -> t

    val sort : t -> sort
    val to_string : t -> string
end

module type NAME = sig
    include COMMON
    type sort
    
    val fresh : string -> sort -> t
    val sort : t -> sort
    val to_string : t -> string
end

module type LANGUAGE = sig
    module S : SORT
    module O : OPERATOR
        with module S = S
end

module type S = sig
    module L : LANGUAGE
    open L

    module V : VAR with type sort = S.t
    module N : NAME with type sort = S.t

    type names = N.t list
    type vars = V.t list

    type t
    type 'a abs
    type 'a view =
        | Var of V.t
        | App  of O.t * names * 'a abs list

    (* Destruction *)
    val unfold : t -> t view
    val open_abs : t abs -> names * vars * t
    val subst_abs : t list -> t abs -> names * t
    val rename_abs : names -> t abs -> vars * t
    val subst_rename_abs : names -> t list -> t abs -> t

    (* Construction *)
    val fold : t view -> t
    val abs : names -> vars -> t -> t abs

    (* Misc *)
    val subst : t -> V.t -> t -> t

end

module Make (L : LANGUAGE) : S with module L = L