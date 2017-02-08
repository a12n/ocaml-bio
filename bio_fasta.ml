open Batteries

module type Seq_sig = sig
  type t
  val length : t -> int
  val of_string : string -> t
  val sub : t -> start:int -> len:int -> t
  val to_string : t -> string
end

let from_lines lines =
  let starts_with sym line =
    String.(length line > 0 && get line 0 = sym) in
  let rec parse_header () =
    match Enum.get lines with
    | Some line when starts_with '>' line -> (
        let id, descr = String.(
            match index line ' ' with
            | i -> sub line 1 (i - 1), tail line (i + 1)
            | exception Not_found -> tail line 1, ""
          ) in
        parse_str id descr (Buffer.create 4096)
      )
    | Some _line -> failwith "entry start line has no leading '>' symbol"
    | None -> raise Enum.No_more_elements
  and parse_str id descr str_buf =
    match Enum.peek lines with
    | None ->
      (* End of file *)
      (id, descr, Buffer.contents str_buf)
    | Some line when starts_with '>' line ->
      (* Next entry *)
      (id, descr, Buffer.contents str_buf)
    | Some line -> (
        Enum.junk lines;
        Buffer.add_string str_buf line;
        parse_str id descr str_buf
      ) in
  Enum.from parse_header

module Make (Seq : Seq_sig) = struct
  module Entry = struct
    type t = string * string * Seq.t
  end

  let from_input ch =
    Enum.map (fun (id, descr, str) ->
        (id, descr, Seq.of_string str))
      (from_lines (IO.lines_of ch))

  let from_file fname =
    let f = File.open_in fname in
    Enum.suffix_action (fun () -> IO.close_in f) (from_input f)

  let to_output ch =
    Enum.iter (fun (id, descr, seq) ->
        Char.print ch '>';
        String.print ch id;
        Char.print ch ' ';
        String.println ch descr;
        let rec loop i = function
          | 0 -> ()
          | n ->
            let len = if n > 80 then 80 else n in
            String.println ch Seq.(to_string (sub seq ~start:(i * 80) ~len));
            loop (i + 1) (n - len) in
        loop 0 (Seq.length seq)
      )

  let to_file fname entries =
    let f = File.open_out fname in
    Enum.suffix_action (fun () -> IO.close_out f) entries |>
    to_output f
end

module Dna = Make (Bio_dna)
module Prot = Make (Bio_prot)
module Rna = Make (Bio_rna)
