open Batteries

module type Seq_sig = sig
  type t
  val length : t -> int
  val of_string : string -> t
  val sub : t -> start:int -> len:int -> t
  val to_string : t -> string
end

let from_enum chars =
  let id = Buffer.create 64 in
  let descr = Buffer.create 64 in
  let str = Buffer.create 1024 in
  let rec parse_start () =
    Buffer.(clear id; clear descr; clear str);
    match Enum.get chars with
    | Some '>' -> parse_id ()
    | Some _ -> failwith "not '>' symbol"
    | None -> raise Enum.No_more_elements
  and parse_id () =
    match Enum.get chars with
    | Some ' ' | Some '\t' -> parse_descr ()
    | Some '\n' -> parse_str ()
    | Some c -> (Buffer.add_char id c; parse_id ())
    | None -> failwith "end of input"
  and parse_descr () =
    match Enum.get chars with
    | Some '\n' -> parse_str ()
    | Some c -> (Buffer.add_char descr c; parse_descr ())
    | None -> failwith "end of input"
  and parse_str () =
      match Enum.peek chars with
        | Some '\n' -> (Enum.junk chars; parse_str ())
        | Some '>' | None -> Buffer.(contents id,
                                     contents descr,
                                     contents str)
        | Some c -> (Enum.junk chars;
                     Buffer.add_char str c;
                     parse_str ())
  in Enum.from parse_start

let from_lines lines =
  let id_buf, descr_buf, str_buf =
    Buffer.(create 64, create 64, create 1024) in
  let starts_entry line =
    String.(length line > 0 && get line 0 = '>') in
  let rec parse_header () =
    Buffer.(clear id_buf; clear descr_buf; clear str_buf);
    match Enum.get lines with
    | Some line when starts_entry line -> (
        let id, descr =
          let n = String.length line in
          match String.index line ' ' with
          | i ->
            String.(sub line 1 (i - 1),
                    sub line (i + 1) (n - (i + 1)))
          | exception Not_found ->
            String.(sub line 1 (n - 1), "") in
        Buffer.(add_string id_buf id;
                add_string descr_buf descr);
        parse_str ()
      )
    | Some _line -> failwith "entry start line has no leading '>' symbol"
    | None -> raise Enum.No_more_elements
  and parse_str () =
    match Enum.peek lines with
    | None ->
      (* End of file *)
      Buffer.(contents id_buf, contents descr_buf, contents str_buf)
    | Some line when starts_entry line ->
      (* Next entry *)
      Buffer.(contents id_buf, contents descr_buf, contents str_buf)
    | Some line -> (
        Enum.junk lines;
        Buffer.add_string str_buf line;
        parse_str ()
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
