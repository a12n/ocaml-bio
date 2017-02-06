open Batteries

module type Seq_sig = sig
  type t
  val is_empty : t -> bool
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

module Make (Seq : Seq_sig) = struct
  module Entry = struct
    type t = string * string * Seq.t
  end

  let from_input ch =
    Enum.map (fun (id, descr, str) ->
        (id, descr, Seq.of_string str))
      (from_enum (IO.chars_of ch))

  let from_file fname = File.with_file_in fname from_input

  let to_output ch =
    Enum.iter (fun (id, descr, seq) ->
        Char.print ch '>';
        String.print ch id;
        Char.print ch ' ';
        String.println ch descr;
        let rec loop i =
          match Seq.sub seq ~start:(i * 80) ~len:80 with
          | s when Seq.is_empty s -> ()
          | s ->
            String.println ch (Seq.to_string s);
            loop (i + 1) in
        loop 0
      )

  let to_file fname = File.with_file_out fname to_output
end

module Dna = Make (Dna)
module Prot = Make (Prot)
module Rna = Make (Rna)
