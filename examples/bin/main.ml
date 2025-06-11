let go () =
  (* A 20Mb bigstring *)
  let b = Bigstringaf.create (1024 * 1024 * 20) in
  (* Get the first megabyte as a new string *)
  let b2 = Bigstringaf.copy ~off:0 ~len:(1024 * 1024) b in
  (* Or, as a view on the original string, with no copying *)
  let b3 = Bigstringaf.sub ~off:0 ~len:(1024 * 1024) in
  (* Get a string from a section of a bigstring (avoids copy then convert to string) *)
  let s = Bigstringaf.substring b ~off:0 ~len:(1024 * 1024) in
  (* Get a 64 bit big endian value *)
  let i64 = Bigstringaf.get_int64_be b 0 in
  (* Blitting. Here from string to bigstring *)
  Bigstringaf.blit_from_string s ~src_off:0 b ~dst_off:(1024 * 1024) ~len:(1024 * 1024);
  (* Compare *)
  let cmp = Bigstringaf.memcmp b 0 b2 0 (1024 * 1024) in
  (* Look for a byte *)
  let found = Bigstringaf.memchr b 0 '\n' (1024 * 1024) in
    Printf.printf "Position of first \\n: %i\n" found

let () =
  match Sys.argv with
  | [|_|] -> go ()
  | _ -> Printf.eprintf "bigstringaf example: unknown command line\n"
