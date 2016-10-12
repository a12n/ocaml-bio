open Batteries

module type Elt_sig = sig
  type t
  val n : int
  val of_char : char -> t
  val of_int : int -> t
  val to_char : t -> char
  val to_int : t -> int
end

module Make (Elt : Elt_sig) : sig
  type t

  val of_string : string -> t

  val to_string : t -> string
end = struct
  type t = string


  let of_string = String.map Elt.(to_char % of_char)


  let to_string seq = seq
end
