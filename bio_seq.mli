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

  val compare : t -> t -> int
  val delete : t -> sub:t -> t
  val find_elt : ?first:int -> t -> Elt.t -> int Enum.t
  val find_sub : ?first:int -> t -> sub:t -> int Enum.t
  val get : t -> int -> Elt.t
  val is_empty : t -> bool
  val left : t -> int -> t
  val length : t -> int
  val overlap : ?len:int -> t -> t -> t
  val overlap_length : t -> t -> int
  val rev : t -> t
  val right : t -> int -> t
  val slice : ?first:int -> ?last:int -> t -> t
  val sub : t -> first:int -> len:int -> t

  val int_of_kmer : t -> int
  val kmer_clumps : k:int -> l:int -> t:int -> t -> t Enum.t
  val kmer_compos : k:int -> t -> int array
  val kmer_of_int : k:int -> int -> t
  val kmers : k:int -> t -> t Enum.t
  val num_kmers : k:int -> int

  val count : Elt.t -> t -> int

  val edit_dist : t -> t -> int
  val hamm_dist : t -> t -> int
  val p_dist : t -> t -> float

  val lcs : t -> t -> t
  val scs : t -> t -> t

  val print : (t, 'a) IO.printer
  val print_elt : (Elt.t, 'a) IO.printer
  val print_quoted : (t, 'a) IO.printer

  (** Position frequency matrix. *)
  module Pfm : sig
    type t = private int array array

    val from_array : seq array -> t
    val from_enum : seq Enum.t -> t
    val from_list : seq list -> t

    val consensus : t -> seq
  end

  module Align : sig
    module Scoring : sig
      (** Scoring is [(gap, subst)] pair.
          Function [subst a b] must return score of replacing [a]
          with [b]. *)
      type t = [ `Linear of int ] * (Elt.t -> Elt.t -> int)

      val default : t
    end

    (** Alignment of sequences [s] and [t] is represented as a list of
        edit operations, needed to transform [s] into [t]. *)
    type t = [ `Delete of Elt.t | `Insert of Elt.t |
               `Match of Elt.t | `Subst of Elt.t * Elt.t ] list

    val global : ?scoring:Scoring.t -> seq -> seq -> int * t
    val local : ?scoring:Scoring.t -> seq -> seq -> int * t
    val semi_global : ?scoring:Scoring.t -> seq -> seq -> int * t

    val to_string : t -> string * string
  end
end
