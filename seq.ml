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

  val of_array : Elt.t array -> t
  val of_enum : Elt.t Enum.t -> t
  val of_list : Elt.t list -> t
  val of_string : string -> t

  val enum : t -> Elt.t Enum.t
  val to_array : t -> Elt.t array
  val to_list : t -> Elt.t list
  val to_string : t -> string
end = struct
  type t = string


  let of_array arr = String.init (Array.length arr) (Elt.to_char % Array.get arr)

  let of_enum enum = String.of_enum (Enum.map Elt.to_char enum)

  let of_list list = String.of_list (List.map Elt.to_char list)

  let of_string = String.map Elt.(to_char % of_char)


  let enum = Enum.map Elt.of_char % String.enum

  let to_array = Array.of_enum % enum

  let to_list = List.map Elt.of_char % String.to_list

  let to_string seq = seq
end
