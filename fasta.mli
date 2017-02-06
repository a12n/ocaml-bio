open Batteries

module type Seq_sig = sig
  type t
  val is_empty : t -> bool
  val of_string : string -> t
  val sub : t -> start:int -> len:int -> t
  val to_string : t -> string
end

module Make : functor (Seq : Seq_sig) -> sig
  module Entry : sig
    type t = string * string * Seq.t
  end

  val from_input : IO.input -> Entry.t Enum.t
  val to_output : 'a IO.output -> Entry.t Enum.t -> unit
end

module Dna : module type of Make (Dna)
module Prot : module type of Make (Prot)
module Rna : module type of Make (Rna)
