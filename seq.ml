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

  let of_enum enum = String.of_enum (Enum.map Elt.to_char enum)

  let of_list list = String.of_list (List.map Elt.to_char list)

  let of_string = String.map Elt.(to_char % of_char)


  let enum = Enum.map Elt.of_char % String.enum

  let to_array = Array.of_enum % enum

  let to_list = List.map Elt.of_char % String.to_list

  let to_string seq = seq


  let get seq i = Elt.of_char (String.get seq i)

  let is_empty = String.is_empty

  let left = String.left

  let length = String.length

  let rev = String.rev

  let right = String.right

  let sub seq ~start ~len = String.sub seq start len


  let fold_left f = String.fold_left (fun ans c -> f ans (Elt.of_char c))

  let fold_left2 f init s t =
    if length s <> length t then
      invalid_arg "Seq.fold_left2";
    String.fold_lefti (fun ans i si ->
        f ans (Elt.of_char si) (Elt.of_char t.[i])
      ) init s

  let iter f = String.iter (f % Elt.of_char)

  let iteri f = String.iteri (fun i c -> f i (Elt.of_char c))

  let map f = String.map Elt.(to_char % f % of_char)


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


  let print = String.print_quoted


  module Pfm = struct
    type t = int array array

    let consensus pfm =
      let n = Array.length pfm.(0) in
      Enum.init n (fun j ->
          let k = ref 0 in
          for i = 1 to Elt.n - 1 do
            if pfm.(i).(j) > pfm.(!k).(j) then k := i
          done;
          Elt.of_int !k
        ) |> of_enum

    let from_enum enum =
      let n = match Enum.peek enum with
        | Some s0 -> length s0
        | None -> invalid_arg "Seq.Pfm.from_enum: empty enum of sequences" in
      let ans = Array.make_matrix Elt.n n 0 in
      Enum.iter (fun t ->
          if length t <> n then
            invalid_arg "Seq.Pfm.from_enum: sequence length mismatch";
          iteri (fun j elt ->
              let i = Elt.to_int elt in
              ans.(i).(j) <- ans.(i).(j) + 1
            ) t
        ) enum;
      ans

    let from_array = from_enum % Array.enum

    let from_list = from_enum % List.enum
  end


  module Align = struct
    module Scoring = struct
      type t =
        [ `Linear of int ] *
        (Elt.t -> Elt.t -> int) *
        (int -> int -> bool)

      let default =
        `Linear (-1),
        (fun a b -> if a = b then 1 else 0),
        (>)
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
      let `Linear gap, subst, better = scoring in
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
          if better diag s.(i).(j) then (
            s.(i).(j) <- diag;
            b.(i).(j) <- `Diag
          );
          if better left s.(i).(j) then (
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
      let `Linear gap, subst, better = scoring in
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
          if better up s.(i).(j) then (
            s.(i).(j) <- up;
            b.(i).(j) <- `Up;
          );
          if better diag s.(i).(j) then (
            s.(i).(j) <- diag;
            b.(i).(j) <- `Diag
          );
          if better left s.(i).(j) then (
            s.(i).(j) <- left;
            b.(i).(j) <- `Left
          );
          (* If new score is as best so far, consider new score the
             best, to maximize alignment length. *)
          if not (better !best_s s.(i).(j)) then (
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
      let `Linear gap, subst, better = scoring in
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
          if better diag s.(i).(j) then (
            s.(i).(j) <- diag;
            b.(i).(j) <- `Diag
          );
          if better left s.(i).(j) then (
            s.(i).(j) <- left;
            b.(i).(j) <- `Left
          )
        done
      done;
      let best_ij = ref (n, m) in
      let best_s = ref s.(n).(m) in
      if n > m then begin
        for i = 1 to n do
          if not (better !best_s s.(i).(m)) then (
            best_ij := (i, m);
            best_s := s.(i).(m)
          )
        done;
        for i = n downto (fst !best_ij) + 1 do
          b.(i).(m) <- `Up
        done
      end else begin
        for j = 1 to m do
          if not (better !best_s s.(n).(j)) then (
            best_ij := (n, j);
            best_s := s.(n).(j)
          )
        done;
        for j = m downto (snd !best_ij) + 1 do
          b.(n).(j) <- `Left
        done
      end;
      print_tables s b;
      Printf.eprintf "%d,%d\n%!" (fst !best_ij) (snd !best_ij);
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
