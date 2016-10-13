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

  val fold_left : ('a -> Elt.t -> 'a) -> 'a -> t -> 'a
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


  let fold_left f = String.fold_left (fun ans c -> f ans (Elt.of_char c))

  let map f = String.map Elt.(to_char % f % of_char)


  let get seq i = Elt.of_char (String.get seq i)

  let is_empty = String.is_empty

  let left = String.left

  let length = String.length

  let rev = String.rev

  let right = String.right

  let sub seq ~start ~len = String.sub seq start len


  let count elt seq =
    let c = Elt.to_char elt in
    let ans = ref 0 in
    for i = 0 to length seq - 1 do
      if seq.[i] = c then incr ans
    done;
    !ans


  let edit_dist s t =
    let n = length s in
    let m = length t in
    let d = Array.make_matrix 2 (m + 1) 0 in
    for j = 1 to m do
      d.(0).(j) <- j
    done;
    for i = 1 to n do
      d.(i mod 2).(0) <- i;
      for j = 1 to m do
        let cost = if s.[i - 1] = t.[j - 1] then 0 else 1 in
        let del = d.((i - 1) mod 2).(j) + 1 in
        let ins = d.(i mod 2).(j - 1) + 1 in
        let subst = d.((i - 1) mod 2).(j - 1) + cost in
        d.(i mod 2).(j) <- Int.(min (min del ins) subst)
      done
    done;
    d.(n mod 2).(m)

  let hamm_dist_aux s t n =
    let ans = ref 0 in
    for i = 0 to n - 1 do
      if s.[i] <> t.[i] then incr ans
    done;
    !ans

  let hamm_dist s t =
    let n = length s in
    if length t = n then
      hamm_dist_aux s t n
    else invalid_arg "Seq.hamm_dist"

  let p_dist s t =
    let n = length s in
    if length t = n then
      if n > 0 then
        float_of_int (hamm_dist_aux s t n) /.
        float_of_int n
      else 0.0
    else invalid_arg "Seq.p_dist"
end
