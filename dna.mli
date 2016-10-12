module Nt : sig
  type t = A | C | G | T

  val comp : t -> t
  val n : int
  val of_char : char -> t
  val of_int : int -> t
  val to_char : t -> char
  val to_int : t -> int
end

include module type of Seq.Make (Nt)

val comp : t -> t
val gc_content : t -> float
val rev_comp : t -> t
