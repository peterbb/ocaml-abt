module type COMMON = sig
    type t
    val eq : t -> t -> bool
    val compare : t -> t -> int
    val to_string : t -> string
end

module type SORT = sig
    include COMMON
end

module type OPERATOR = sig
    include COMMON
    module S : SORT

    val of_string : string -> S.t -> t option
    val arity : t -> S.t list * (S.t list * S.t list * S.t) list * S.t
end

module type VAR = sig
    include COMMON

    type sort

    (* Note that [fresh x s] is different from [fresh x s]. *)
    val fresh : string -> sort -> t
    val global : string -> sort -> t
    val sort : t -> sort
end

module type NAME = sig
    include COMMON
    type sort
    
    val fresh : string -> sort -> t
    val global : string -> sort -> t
    val sort : t -> sort
end

module type LANGUAGE = sig
    module S : SORT
    module O : OPERATOR
        with module S = S
end

module type S = sig
    include LANGUAGE

    module V : VAR with type sort = S.t
    module N : NAME with type sort = S.t

    type 'a loc = Loc.t * 'a

    type names = N.t loc list
    type vars = V.t loc list

    type t
    type 'a abs
    type 'a view =
        | Var of V.t loc
        | App  of O.t loc * names * 'a abs list

    (* Destruction *)
    val unfold : t -> t view

   (* [open_abs abs] will generate fresh names and
    * variables, and insert them for the bound variables
    * in the term. *)
    val open_abs : t abs -> names * vars * t

    (* [open_abs subst abs] will generate fresh names and variables,
     * and then open the abstraction with the fresh names
     * substituted for the bound names, and [subst] will be
     * substituted for the bound variables.
     *)
    val subst_abs : t list -> t abs -> names * t


    (* [rename_abs names abs] will generate fresh variables,
     * and substitute the fresh variables for the bound variables,
     * and substitute [names] for the bound names. *)
    val rename_abs : names -> t abs -> vars * t


    (* [subst_rename_abs names subst abs] will substitute
     * [subst] for the bound variables and rename the bound
     * names to [names], in [abs].  *)
    val subst_rename_abs : names -> t list -> t abs -> t

    val fold : t view -> t

    (* [abs names vars t] turns the (necessarily free) occurences of 
     * the names and variables into bound variables. *)
    val abs : names -> vars -> t -> t abs

    val subst : t -> V.t -> t -> t
    val sort : t -> S.t
end

module Make (L : LANGUAGE) : S
    with module S = L.S
    with module O = L.O

