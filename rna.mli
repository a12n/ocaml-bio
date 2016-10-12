module Nt : sig
  type nt = A | C | G | U
  include Seq.Elt_sig with type t = nt
end

include module type of Seq.Make (Nt)
