type t =
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

val read  : Unix.file_descr -> ?off:int -> ?len:int -> t -> int
val write : Unix.file_descr -> ?off:int -> ?len:int -> t -> int
