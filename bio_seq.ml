open Batteries

module type Elt_sig = sig
  type t
  val n : int
  val of_char : char -> t
  val of_int : int -> t
  val to_char : t -> char
  val to_int : t -> int
end

module Make (Elt : Elt_sig) = struct
  type t = string and seq = t


  let of_array arr = String.init (Array.length arr) (Elt.to_char % Array.get arr)

  let of_enum = String.of_enum % (Enum.map Elt.to_char)

  let of_list = String.of_list % (List.map Elt.to_char)

  let of_string = String.map Elt.(to_char % of_char)


  let enum = Enum.map Elt.of_char % String.enum

  let to_array = Array.of_enum % enum

  let to_list = List.map Elt.of_char % String.to_list

  let to_string seq = seq


  let compare = String.compare

  let delete str ~sub =
    if not (String.is_empty sub) then
      String.nreplace ~str ~sub ~by:""
    else str

  let find_elt ?(first=0) seq elt =
    let c = Elt.to_char elt in
    Enum.from_loop first (fun first ->
        match String.index_from seq first c with
        | i -> i, i + 1
        | exception (Invalid_argument _) -> raise Enum.No_more_elements
        | exception Not_found -> raise Enum.No_more_elements
      )


  let get seq i = Elt.of_char (String.get seq i)

  let is_empty = String.is_empty

  let left = String.left

  let length = String.length

  let rev = String.rev

  let right = String.right

  let slice = String.slice

  let sub seq ~first ~len = String.sub seq first len

  let overlap_length s t =
    (* TODO *)
    let n = length s in
    let m = length t in
    let l = Int.min n m in
    let rec loop ans len =
      if len <= l then
        if right s len = left t len then
          loop len (len + 1)
        else loop ans (len + 1)
      else ans in
    loop 0 1

  let overlap ?len s t =
    let n = length s in
    let len = Option.default_delayed
        (fun () -> overlap_length s t) len in
    if len < 0 || len > n then
      invalid_arg "invalid overlap length";
    left s (n - len) ^ t


  let int_of_kmer s =
    let k = length s in
    let rec loop ans m i =
      if i >= 0 then
        let elt = Elt.(to_int (of_char s.[i])) in
        loop (ans + m * elt) (m * Elt.n) (i - 1)
      else ans in
    loop 0 1 (k - 1)

  let kmer_of_int ~k m =
    let rec loop buf m i =
      if i >= 0 then
        let c = Elt.(to_char (of_int (m mod n))) in
        Bytes.set buf i c;
        loop buf (m / Elt.n) (i - 1)
      else Bytes.unsafe_to_string buf in
    loop (Bytes.create k) m (k - 1)

  let kmers ~k s = Enum.init (length s - k + 1) (fun i -> sub ~first:i ~len:k s)

  let num_kmers ~k = Int.pow Elt.n k

  let all_kmers ~k = Enum.init (num_kmers ~k) (kmer_of_int ~k)

  let kmer_compos ~k s =
    let ans = Array.make (num_kmers ~k) 0 in
    Enum.iter (fun t ->
        let i = int_of_kmer t in
        ans.(i) <- ans.(i) + 1
      ) (kmers ~k s);
    ans

  (** All distinct k-mers forming (l, t)-clumps in the sequence. *)
  let kmer_clumps ~k ~l ~t s =
    let ans = Hashtbl.create k in
    let count = Hashtbl.create l in
    let incr_count kmer n =
      if n + 1 >= t then
        Hashtbl.replace ans kmer ();
      n + 1 in
    Enum.iter (fun i ->
        let kmer = sub s ~first:i ~len:k in
        Hashtbl.modify_def 0 kmer (incr_count kmer) count
      ) Enum.(0 -- (l - k));
    Enum.iter (fun i ->
        let kmer = sub s ~first:(i - 1) ~len:k in
        Hashtbl.modify kmer Int.pred count;
        let kmer = sub s ~first:(i + l - k) ~len:k in
        Hashtbl.modify_def 0 kmer (incr_count kmer) count;
      ) Enum.(1 -- (length s - l));
    Hashtbl.keys ans


  let fold_left f = String.fold_left (fun ans c -> f ans (Elt.of_char c))

  let fold_left2 f init s t =
    if length s <> length t then
      invalid_arg "length mismatch";
    String.fold_lefti (fun ans i si ->
        f ans (Elt.of_char si) (Elt.of_char t.[i])
      ) init s

  let iter f = String.iter (f % Elt.of_char)

  let iteri f = String.iteri (fun i c -> f i (Elt.of_char c))

  let map f = String.map Elt.(to_char % f % of_char)


  let count_elt elt seq =
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
    else invalid_arg "length mismatch"

  let p_dist s t =
    let n = length s in
    if length t = n then
      if n > 0 then
        float_of_int (hamm_dist_aux s t n) /.
        float_of_int n
      else 0.0
    else invalid_arg "length mismatch"


  let find_sub ?(first=0) ?(dist=0) s ~sub =
    if dist = 0 then
      (* TODO: really handle first *)
      String.find_all s sub |> Enum.filter ((<=) first)
    else
      let m = length sub in
      Enum.filter (fun i ->
          hamm_dist (String.sub s i m) sub <= dist
        ) Enum.(first -- (length s - m - first))

  let count_sub ?dist ~sub s = Enum.count (find_sub ?dist s ~sub)

  let exists_sub ?first ?dist s ~sub =
    not (Enum.is_empty (find_sub ?first ?dist s ~sub))


  (** All neighbours of sequence [s] within Hamming distance of [d]
      from [s]. *)
  let neighbors ~dist s =
    let basic () =
      Set.of_array (Array.init Elt.n String.(of_char % Elt.(to_char % of_int))) in
    let rec aux s =
      let n = length s in
      if n > 1 then
        let suffix = right s (n - 1) in
        Set.fold (fun t ans ->
            if hamm_dist suffix t < dist then
              Set.fold (fun prefix ans ->
                  Set.add (prefix ^ t) ans
                ) (basic ()) ans
            else Set.add (left s 1 ^ t) ans
          ) (aux suffix) Set.empty
      else basic () in
    if dist > 0 then
      aux s
    else Set.singleton s


  let lcs s t =
    let n = length s in
    let m = length t in
    let c = Array.make_matrix (n + 1) (m + 1) 0 in
    for i = 1 to n do
      for j = 1 to m do
        c.(i).(j) <-
          if s.[i - 1] = t.[j - 1] then
            c.(i - 1).(j - 1) + 1
          else
            max c.(i).(j - 1) c.(i - 1).(j)
      done
    done;
    let buf = Buffer.create (max n m) in
    let rec backtrack = function
      | (_, 0) | (0, _) -> ()
      | (i, j) when s.[i - 1] = t.[j - 1] ->
        backtrack (i - 1, j - 1);
        Buffer.add_char buf s.[i - 1]
      | (i, j) ->
        if c.(i).(j - 1) > c.(i - 1).(j) then
          backtrack (i, j - 1)
        else
          backtrack (i - 1, j)
    in backtrack (n, m);
    Buffer.contents buf

  let scs s t =
    let lcs = lcs s t in
    let sn = length s in
    let tn = length t in
    let n = length lcs in
    let buf = Buffer.create (sn + tn) in
    let rec loop i si ti =
      if i < n then
        if s.[si] = lcs.[i] then
          if t.[ti] = lcs.[i] then
            (Buffer.add_char buf lcs.[i]; loop (i + 1) (si + 1) (ti + 1))
          else
            (Buffer.add_char buf t.[ti]; loop i si (ti + 1))
        else
          (Buffer.add_char buf s.[si]; loop i (si + 1) ti)
      else if si < sn then
        (Buffer.add_char buf s.[si]; loop i (si + 1) ti)
      else if ti < tn then
        (Buffer.add_char buf t.[ti]; loop i si (ti + 1))
    in loop 0 0 0;
    Buffer.contents buf


  let print = String.print
  let print_elt f elt = Char.print f (Elt.to_char elt)
  let print_quoted = String.print_quoted


  module Profile = struct
    let length pxm = Array.(length pxm.(0))

    let consensus pxm =
      let k = length pxm in
      Enum.init k (fun j ->
          Enum.init Elt.n (fun i -> pxm.(i).(j), i) |>
          Enum.reduce max |> Tuple2.second |> Elt.of_int
        ) |> of_enum
  end

  module Pfm = struct
    type t = int array array

    let consensus = Profile.consensus

    let make seqs =
      let n = match Enum.peek seqs with
        | Some s0 -> length s0
        | None -> invalid_arg "empty enum of sequences" in
      let ans = Array.make_matrix Elt.n n 0 in
      Enum.iter (fun t ->
          if length t <> n then
            invalid_arg "sequence length mismatch";
          iteri (fun j elt ->
              let i = Elt.to_int elt in
              ans.(i).(j) <- ans.(i).(j) + 1
            ) t
        ) seqs;
      ans

    let length = Profile.length

    let num_seqs pfm = Enum.(sum (init Elt.n (fun i -> pfm.(i).(0))))
  end

  module Ppm = struct
    type t = float array array

    let consensus = Profile.consensus

    let length = Profile.length

    let of_pfm pfm =
      let k = Pfm.length pfm in
      let n = Pfm.num_seqs pfm in
      Array.init Elt.n (fun i ->
          Array.init k (fun j ->
              float_of_int pfm.(i).(j) /. float_of_int n
            )
        )

    let prob ppm s =
      enum s |> Enum.mapi (fun i elt ->
          ppm.(Elt.to_int elt).(i)
        ) |> Enum.reduce ( *. )

    (** Profile-most probable sequence in a set of sequences. *)
    let most_prob ppm seqs =
      Enum.map (fun s -> prob ppm s, s) seqs |>
      Enum.reduce max |> Tuple2.second
  end


  module Align = struct
    module Scoring = struct
      type t = [ `Linear of int ] * (Elt.t -> Elt.t -> int)

      let default = `Linear (-1), (fun a b -> if a = b then 1 else 0)
    end

    type t = [ `Delete of Elt.t | `Insert of Elt.t
             | `Match of Elt.t | `Subst of Elt.t * Elt.t ] list


    let print_tables s b =
      let n = Array.length s in
      let m = Array.length s.(0) in
      for i = 0 to n - 1 do
        for j = 0 to m - 1 do
          Printf.eprintf " %2d%s" s.(i).(j)
            (match b.(i).(j) with
             | `Up   -> "↑"
             | `Diag -> "↖"
             | `Left -> "←"
             | `Stop -> " ")
        done;
        prerr_newline ()
      done


    let backtrack x y b =
      let rec loop ans i j =
        match b.(i).(j) with
        | `Up ->
          let xi = get x (i - 1) in
          let op = `Delete xi in
          loop (op :: ans) (i - 1) j
        | `Diag ->
          let xi = get x (i - 1) in
          let yj = get y (j - 1) in
          let op =
            if xi = yj then `Match xi
            else `Subst (xi, yj) in
          loop (op :: ans) (i - 1) (j - 1)
        | `Left ->
          let yj = get y (j - 1) in
          let op = `Insert yj in
          loop (op :: ans) i (j - 1)
        | `Stop -> ans in
      loop []


    let global_build ?(scoring=Scoring.default) x y =
      let `Linear gap, subst = scoring in
      let n = length x in
      let m = length y in
      let s = Array.make_matrix (n + 1) (m + 1) 0 in
      let b = Array.make_matrix (n + 1) (m + 1) `Stop in
      for i = 1 to n do
        s.(i).(0) <- i * gap;
        b.(i).(0) <- `Up
      done;
      for j = 1 to m do
        s.(0).(j) <- j * gap;
        b.(0).(j) <- `Left
      done;
      for i = 1 to n do
        let xi = get x (i - 1) in
        for j = 1 to m do
          let yj = get y (j - 1) in
          let up = s.(i - 1).(j) + gap in
          let diag = s.(i - 1).(j - 1) + subst xi yj in
          let left = s.(i).(j - 1) + gap in
          (* Counterclockwise selection policy *)
          s.(i).(j) <- up;
          b.(i).(j) <- `Up;
          if diag > s.(i).(j) then (
            s.(i).(j) <- diag;
            b.(i).(j) <- `Diag
          );
          if left > s.(i).(j) then (
            s.(i).(j) <- left;
            b.(i).(j) <- `Left
          )
        done
      done;
      s.(n).(m), b, (n, m)

    let global ?scoring x y =
      let score, b, (i, j) = global_build ?scoring x y in
      score, backtrack x y b i j


    let local_build ?(scoring=Scoring.default) x y =
      let `Linear gap, subst = scoring in
      let n = length x in
      let m = length y in
      let s = Array.make_matrix (n + 1) (m + 1) 0 in
      let b = Array.make_matrix (n + 1) (m + 1) `Stop in
      let best_ij = ref (0, 0) in
      let best_s = ref 0 in
      for i = 1 to n do
        let xi = get x (i - 1) in
        for j = 1 to m do
          let yj = get y (j - 1) in
          let up = s.(i - 1).(j) + gap in
          let diag = s.(i - 1).(j - 1) + subst xi yj in
          let left = s.(i).(j - 1) + gap in
          (* Counterclockwise selection policy *)
          if up > s.(i).(j) then (
            s.(i).(j) <- up;
            b.(i).(j) <- `Up;
          );
          if diag > s.(i).(j) then (
            s.(i).(j) <- diag;
            b.(i).(j) <- `Diag
          );
          if left > s.(i).(j) then (
            s.(i).(j) <- left;
            b.(i).(j) <- `Left
          );
          (* If new score is as best so far, consider new score the
             best, to maximize alignment length. *)
          if not (!best_s > s.(i).(j)) then (
            best_ij := (i, j);
            best_s := s.(i).(j)
          )
        done
      done;
      !best_s, b, !best_ij

    let local ?scoring x y =
      let score, b, (i, j) = local_build ?scoring x y in
      score, backtrack x y b i j


    let semi_global_build ?(scoring=Scoring.default) x y =
      let `Linear gap, subst = scoring in
      let n = length x in
      let m = length y in
      let s = Array.make_matrix (n + 1) (m + 1) 0 in
      let b = Array.make_matrix (n + 1) (m + 1) `Stop in
      if n > m then begin
        for i = 1 to n do
          b.(i).(0) <- `Up
        done;
        for j = 1 to m do
          s.(0).(j) <- j * gap;
          b.(0).(j) <- `Left
        done
      end else begin
        for j = 1 to m do
          b.(0).(j) <- `Left
        done;
        for i = 1 to n do
          s.(i).(0) <- i * gap;
          b.(i).(0) <- `Up
        done;
      end;
      for i = 1 to n do
        let xi = get x (i - 1) in
        for j = 1 to m do
          let yj = get y (j - 1) in
          let up = s.(i - 1).(j) + gap in
          let diag = s.(i - 1).(j - 1) + subst xi yj in
          let left = s.(i).(j - 1) + gap in
          (* Counterclockwise selection policy *)
          s.(i).(j) <- up;
          b.(i).(j) <- `Up;
          if diag > s.(i).(j) then (
            s.(i).(j) <- diag;
            b.(i).(j) <- `Diag
          );
          if left > s.(i).(j) then (
            s.(i).(j) <- left;
            b.(i).(j) <- `Left
          )
        done
      done;
      let best_ij = ref (n, m) in
      let best_s = ref s.(n).(m) in
      if n > m then begin
        for i = 1 to n do
          if not (!best_s > s.(i).(m)) then (
            best_ij := (i, m);
            best_s := s.(i).(m)
          )
        done;
        for i = n downto (fst !best_ij) + 1 do
          b.(i).(m) <- `Up
        done
      end else begin
        for j = 1 to m do
          if not (!best_s > s.(n).(j)) then (
            best_ij := (n, j);
            best_s := s.(n).(j)
          )
        done;
        for j = m downto (snd !best_ij) + 1 do
          b.(n).(j) <- `Left
        done
      end;
      !best_s, b, (n, m)

    let semi_global ?scoring x y =
      let score, b, (i, j) = semi_global_build ?scoring x y in
      score, backtrack x y b i j


    let to_string ops =
      let x = Buffer.create 512 in
      let y = Buffer.create 512 in
      let rec loop = function
        | [] -> Buffer.(contents x, contents y)
        | `Delete xi :: ops ->
          Buffer.(add_char x (Elt.to_char xi);
                  add_char y '-');
          loop ops
        | `Insert yj :: ops ->
          Buffer.(add_char x '-';
                  add_char y (Elt.to_char yj));
          loop ops
        | `Match elt :: ops ->
          let c = Elt.to_char elt in
          Buffer.(add_char x c;
                  add_char y c);
          loop ops
        | `Subst (xi, yj) :: ops ->
          Buffer.(add_char x (Elt.to_char xi);
                  add_char y (Elt.to_char yj));
          loop ops in
      loop ops
  end
end
