type t =
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

let[@inline never] invalid_bounds op buffer_len off len =
  let message =
    Printf.sprintf "Bigstringaf_unix.%s invalid range: { buffer_len: %d, off: %d, len: %d }"
    op buffer_len off len
  in
  raise (Invalid_argument message)

let get_bounds name ?(off=0) ?len t =
  let buffer_len = Bigarray.Array1.dim t in
  let len = match len with
    | Some len -> len
    | None -> buffer_len
  in
  if len < 0 || off < 0 || buffer_len - off < len
  then invalid_bounds name buffer_len off len
  else (off, len)

external read_fd  : Unix.file_descr -> t -> int -> int -> int = "bigstringaf_read"
external write_fd : Unix.file_descr -> t -> int -> int -> int = "bigstringaf_write"

let read fd ?off ?len t =
  let off, len = get_bounds "read" ?off ?len t in
  read_fd fd t off len
and write fd ?off ?len t =
  let off, len = get_bounds "write" ?off ?len t in
  write_fd fd t off len
