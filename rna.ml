open BatPervasives

module Nt = struct
  type nt = A | C | G | U and t = nt

  let n = 4

  let of_char = function
    | 'A' | 'a' -> A
    | 'C' | 'c' -> C
    | 'G' | 'g' -> G
    | 'U' | 'u' -> U
    | _ -> invalid_arg "Rna.Nt.of_char"

  let of_int = function
    (* As in 2bit for DNA *)
    | 0b10 -> A
    | 0b01 -> C
    | 0b11 -> G
    | 0b00 -> U
    | _ -> invalid_arg "Rna.Nt.of_int"

  let to_char = function
    | A -> 'A'
    | C -> 'C'
    | G -> 'G'
    | U -> 'U'

  let to_int = function
    (* As in 2bit for DNA *)
    | A -> 0b10
    | C -> 0b01
    | G -> 0b11
    | U -> 0b00
end

include Seq.Make (Nt)

module Codon = struct
  type t = Nt.t * Nt.t * Nt.t
end

module Rf = struct
  type t = Codon.t Batteries.Enum.t
end

module Gen_code = struct
  module type Sig = sig
    val start_codons : Codon.t list
    val stop_codons : Codon.t list
    val translate : Codon.t -> Aa.t option
  end

  open Nt

  let find_stop_codons translate =
    let nts = [A; C; G; U] in
    Batteries.List.(
      n_cartesian_product [nts; nts; nts] |>
      filter_map (
        function [base1; base2; base3] ->
          let codon = base1, base2, base3 in
          (match translate codon with
           | Some _aa -> None
           | None -> Some codon)
               | _other -> None
      ))

  module Std = struct
    let translate = function
      | G,C,U | G,C,C | G,C,A | G,C,G                 -> Some Aa.A
      | C,G,U | C,G,C | C,G,A | C,G,G | A,G,A | A,G,G -> Some Aa.R
      | A,A,U | A,A,C                                 -> Some Aa.N
      | G,A,U | G,A,C                                 -> Some Aa.D
      | U,G,U | U,G,C                                 -> Some Aa.C
      | C,A,A | C,A,G                                 -> Some Aa.Q
      | G,A,A | G,A,G                                 -> Some Aa.E
      | G,G,U | G,G,C | G,G,A | G,G,G                 -> Some Aa.G
      | C,A,U | C,A,C                                 -> Some Aa.H
      | A,U,U | A,U,C | A,U,A                         -> Some Aa.I
      | U,U,A | U,U,G | C,U,U | C,U,C | C,U,A | C,U,G -> Some Aa.L
      | A,A,A | A,A,G                                 -> Some Aa.K
      | A,U,G                                         -> Some Aa.M
      | U,U,U | U,U,C                                 -> Some Aa.F
      | C,C,U | C,C,C | C,C,A | C,C,G                 -> Some Aa.P
      | U,C,U | U,C,C | U,C,A | U,C,G | A,G,U | A,G,C -> Some Aa.S
      | A,C,U | A,C,C | A,C,A | A,C,G                 -> Some Aa.T
      | U,G,G                                         -> Some Aa.W
      | U,A,U | U,A,C                                 -> Some Aa.Y
      | G,U,U | G,U,C | G,U,A | G,U,G                 -> Some Aa.V
      | U,A,A | U,A,G | U,G,A                         -> None

    let start_codons = [U,U,G; C,U,G; A,U,G]

    let stop_codons = find_stop_codons translate

    let rev_translate = function
      | Aa.A -> [G,C,U; G,C,C; G,C,A; G,C,G]
      | Aa.R -> [C,G,U; C,G,C; C,G,A; C,G,G; A,G,A; A,G,G]
      | Aa.N -> [A,A,U; A,A,C]
      | Aa.D -> [G,A,U; G,A,C]
      | Aa.C -> [U,G,U; U,G,C]
      | Aa.Q -> [C,A,A; C,A,G]
      | Aa.E -> [G,A,A; G,A,G]
      | Aa.G -> [G,G,U; G,G,C; G,G,A; G,G,G]
      | Aa.H -> [C,A,U; C,A,C]
      | Aa.I -> [A,U,U; A,U,C; A,U,A]
      | Aa.L -> [U,U,A; U,U,G; C,U,U; C,U,C; C,U,A; C,U,G]
      | Aa.K -> [A,A,A; A,A,G]
      | Aa.M -> [A,U,G]
      | Aa.F -> [U,U,U; U,U,C]
      | Aa.P -> [C,C,U; C,C,C; C,C,A; C,C,G]
      | Aa.S -> [U,C,U; U,C,C; U,C,A; U,C,G; A,G,U; A,G,C]
      | Aa.T -> [A,C,U; A,C,C; A,C,A; A,C,G]
      | Aa.W -> [U,G,G]
      | Aa.Y -> [U,A,U; U,A,C]
      | Aa.V -> [G,U,U; G,U,C; G,U,A; G,U,G]
  end

  module Vert_mt = struct
    let translate = function
      | A,U,A         -> Some Aa.M
      | U,G,A         -> Some Aa.W
      | A,G,A | A,G,G -> None
      | codon         -> Std.translate codon

    let start_codons = [A,U,U; A,U,C; A,U,A; A,U,G; G,U,G]

    let stop_codons = find_stop_codons translate
  end

  module Yeast_mt = struct
    let translate = function
      | A,U,A                         -> Some Aa.M
      | C,U,U | C,U,C | C,U,A | C,U,G -> Some Aa.T
      | U,G,A                         -> Some Aa.W
      | codon                         -> Std.translate codon

    let start_codons = [A,U,A; A,U,G]

    let stop_codons = find_stop_codons translate
  end

  module Mold_mt = struct
    let translate = function
      | U,G,A -> Some Aa.W
      | codon -> Std.translate codon

    let start_codons = [U,U,A; U,U,G; C,U,G; A,U,U; A,U,C; A,U,A; A,U,G; G,U,G]

    let stop_codons = find_stop_codons translate
  end

  (* (\** Invertebrate Mitochondrial. *\) *)
  (* module Invert_mt : Sig *)
  (*  *)
  (* (\** Ciliate, Dasycladacean and Hexamita Nuclear. *\) *)
  (* module Ciliate : Sig *)
  (*  *)
  (* (\** Echinoderm and Flatworm Mitochondrial. *\) *)
  (* module Echinoderm_mt : Sig *)
  (*  *)
  (* (\** Euplotid Nuclear. *\) *)
  (* module Euplotid : Sig *)
  (*  *)
  (* (\** Bacterial, Archaeal and Plant Plastid. *\) *)
  (* module Plastid : Sig *)
  (*  *)
  (* (\** Alternative Yeast Nuclear. *\) *)
  (* module Alt_yeast : Sig *)
  (*  *)
  (* (\** Ascidian Mitochondrial. *\) *)
  (* module Ascidian_mt : Sig *)
  (*  *)
  (* (\** Alternative Flatworm Mitochondrial. *\) *)
  (* module Alt_flatworm_mt : Sig *)
  (*  *)
  (* (\** Chlorophycean Mitochondrial. *\) *)
  (* module Chlorophycean_mt : Sig *)
  (*  *)
  (* (\** Trematode Mitochondrial. *\) *)
  (* module Trematode_mt : Sig *)
  (*  *)
  (* (\** Scenedesmus obliquus Mitochondrial. *\) *)
  (* module Scenedesmus_mt : Sig *)
  (*  *)
  (* (\** Thraustochytrium Mitochondrial. *\) *)
  (* module Thraustochytrium_mt : Sig *)
  (*  *)
  (* (\** Pterobranchia Mitochondrial. *\) *)
  (* module Pterobranchia_mt : Sig *)
  (*  *)
  (* (\** Candidate Division SR1 and Gracilibacteria. *\) *)
  (* module Gracilibacteria : Sig *)
end

let codons_of_enum nts =
  Batteries.Enum.(from (fun () ->
      let base1 = get_exn nts in
      let base2 = get_exn nts in
      let base3 = get_exn nts in
      base1, base2, base3
    ))

let codons = codons_of_enum % enum

(*$T codons
  Batteries.List.of_enum (codons (of_string "")) = []
  Batteries.List.of_enum (codons (of_string "aug")) = Nt.[A,U,G]
  Batteries.List.of_enum (codons (of_string "gauuaca")) = Nt.[G,A,U; U,A,C]
*)

let orf ?(gen_code=(module Gen_code.Std : Gen_code.Sig)) rf =
  let module Gen_code = (val gen_code : Gen_code.Sig) in
  Batteries.Enum.(
    rf |>
    drop_while (fun codon -> not (List.mem codon Gen_code.start_codons)) |>
    take_while (fun codon -> not (List.mem codon Gen_code.stop_codons))
    (* TODO: append stop codon *)
  )

let rfs_of_enum nts =
  Batteries.Enum.(
    nts |> codons_of_enum,
    clone nts |> skip 1 |> codons_of_enum,
    clone nts |> skip 2 |> codons_of_enum
  )

let rfs = rfs_of_enum % enum

(*$T rfs
  (let (a,b,c) = rfs (of_string "") in \
   let (a,b,c) = Batteries.List.(of_enum a, of_enum b, of_enum c) in \
   a = [] && b = [] && c = [])

  (let (a,b,c) = rfs (of_string "aau") in \
   let (a,b,c) = Batteries.List.(of_enum a, of_enum b, of_enum c) in \
   a = Nt.[A,A,U] && b = [] && c = [])

  (let (a,b,c) = rfs (of_string "gauuaca") in \
   let (a,b,c) = Batteries.List.(of_enum a, of_enum b, of_enum c) in \
   a = Nt.[G,A,U; U,A,C] && b = Nt.[A,U,U; A,C,A] && c = Nt.[U,U,A])
*)

let translate ?(gen_code=(module Gen_code.Std : Gen_code.Sig)) rf =
  let module Gen_code = (val gen_code : Gen_code.Sig) in
  let open Batteries in
  Enum.from_while (fun () -> Option.bind (Enum.get rf) Gen_code.translate) |>
  Prot.of_enum

(*$= translate
  (translate (codons (of_string "AUGAAAAAUAAGUUUAAAACCCAGGAAGAGUGA"))) (Prot.of_string "MKNKFKTQEE")
  (translate (codons (of_string "UAG"))) (Prot.of_string "")
  (translate (Batteries.List.enum Nt.[A,U,G])) (Prot.of_string "M")
  (translate (Batteries.Enum.empty ())) (Prot.of_string "")
*)
