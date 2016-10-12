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

  val of_list : Elt.t list -> t
  val of_string : string -> t

  val to_list : t -> Elt.t list
  val to_string : t -> string
end = struct
  type t = string


  let of_list list = String.of_list (List.map Elt.to_char list)

  let of_string = String.map Elt.(to_char % of_char)


  let to_list = List.map Elt.of_char % String.to_list

  let to_string seq = seq
end
