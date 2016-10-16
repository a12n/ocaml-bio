open Batteries

module type Elt_sig = sig
  type t
  val n : int
  val of_char : char -> t
  val of_int : int -> t
  val to_char : t -> char
  val to_int : t -> int
end

module Make : functor (Elt : Elt_sig) -> sig
  type t and seq = t

  val of_array : Elt.t array -> t
  val of_enum : Elt.t Enum.t -> t
  val of_list : Elt.t list -> t
  val of_string : string -> t

  val enum : t -> Elt.t Enum.t
  val to_array : t -> Elt.t array
  val to_list : t -> Elt.t list
  val to_string : t -> string

  val fold_left : ('a -> Elt.t -> 'a) -> 'a -> t -> 'a
  val fold_left2 : ('a -> Elt.t -> Elt.t -> 'a) -> 'a -> t -> t -> 'a
  val iter : (Elt.t -> unit) -> t -> unit
  val iteri : (int -> Elt.t -> unit) -> t -> unit
  val map : (Elt.t -> Elt.t) -> t -> t

  val get : t -> int -> Elt.t
  val is_empty : t -> bool
  val left : t -> int -> t
  val length : t -> int
  val rev : t -> t
  val right : t -> int -> t
  val sub : t -> start:int -> len:int -> t

  val count : Elt.t -> t -> int

  val edit_dist : t -> t -> int
  val hamm_dist : t -> t -> int
  val p_dist : t -> t -> float

  val lcs : t -> t -> t
  val scs : t -> t -> t

  (** Position frequency matrix. *)
  module Pfm : sig
    type t = private int array array

    val from_array : seq array -> t
    val from_enum : seq Enum.t -> t
    val from_list : seq list -> t

    val consensus : t -> seq
  end
end
