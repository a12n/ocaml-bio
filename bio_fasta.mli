open Batteries

module type Seq_sig = sig
  type t
  val length : t -> int
  val of_string : string -> t
  val sub : t -> first:int -> len:int -> t
  val to_string : t -> string
end

module Make : functor (Seq : Seq_sig) -> sig
  module Entry : sig
    type t = string * string * Seq.t
  end

  val from_file : string -> Entry.t Enum.t
  val from_input : IO.input -> Entry.t Enum.t
  val from_string : string -> Entry.t Enum.t
  val to_file : string -> Entry.t Enum.t -> unit
  val to_output : 'a IO.output -> Entry.t Enum.t -> unit
  val to_string : Entry.t Enum.t -> string
end

module Dna : module type of Make (Bio_dna)
module Prot : module type of Make (Bio_prot)
module Rna : module type of Make (Bio_rna)
