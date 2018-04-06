type bigstring =
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

type t = bigstring

let create size = Bigarray.(Array1.create char c_layout size)
let empty       = create 0

module BA1 = Bigarray.Array1

let length t = BA1.dim t

external get : t -> int -> char = "%caml_ba_ref_1"
external set : t -> int -> char -> unit = "%caml_ba_set_1"

external unsafe_get : t -> int -> char         = "%caml_ba_unsafe_ref_1"
external unsafe_set : t -> int -> char -> unit = "%caml_ba_unsafe_set_1"

external unsafe_blit            : t       -> src_off:int -> t       -> dst_off:int -> len:int -> unit =
  "bigstringaf_blit_to_bigstring" [@@noalloc]

external unsafe_blit_to_bytes   : t       -> src_off:int -> Bytes.t -> dst_off:int -> len:int -> unit =
  "bigstringaf_blit_to_bytes"     [@@noalloc]

external unsafe_blit_from_bytes : Bytes.t -> src_off:int -> t       -> dst_off:int -> len:int -> unit =
  "bigstringaf_blit_from_bytes"   [@@noalloc]

external unsafe_blit_from_string : string -> src_off:int -> t       -> dst_off:int -> len:int -> unit =
  "bigstringaf_blit_from_bytes"   [@@noalloc]

external unsafe_memcmp : t -> int -> t -> int -> int -> int =
  "bigstringaf_memcmp_bigstring" [@@noalloc]

external unsafe_memcmp_string : t -> int -> string -> int -> int -> int =
  "bigstringaf_memcmp_string" [@@noalloc]

let sub t ~off ~len =
  BA1.sub t off len

let[@inline never] invalid_bounds op buffer_len off len =
  let message =
    Printf.sprintf "Bigstringaf.%s invalid range: { buffer_len: %d, off: %d, len: %d }"
    op buffer_len off len
  in
  raise (Invalid_argument message)
;;

let copy t ~off ~len =
  let buffer_len = length t in
  if off < 0 || off + len > buffer_len then invalid_bounds "copy" buffer_len off len;
  let dst = create len in
  unsafe_blit t ~src_off:off dst ~dst_off:0 ~len;
  dst

let substring t ~off ~len =
  let buffer_len = length t in
  if off < 0 || off + len > buffer_len then invalid_bounds "substring" buffer_len off len;
  let b = Bytes.create len in
  unsafe_blit_to_bytes t ~src_off:off b ~dst_off:0 ~len;
  Bytes.unsafe_to_string b

let of_string ~off ~len s =
  let buffer_len = String.length s in
  if off < 0 || off + len > buffer_len then invalid_bounds "of_string" buffer_len off len;
  let b = create len in
  unsafe_blit_from_string s ~src_off:off b ~dst_off:0 ~len;
  b

(* Safe operations *)

external caml_bigstring_set_16 : bigstring -> int -> int   -> unit = "%caml_bigstring_set16"
external caml_bigstring_set_32 : bigstring -> int -> int32 -> unit = "%caml_bigstring_set32"
external caml_bigstring_set_64 : bigstring -> int -> int64 -> unit = "%caml_bigstring_set64"

external caml_bigstring_get_16 : bigstring -> int -> int   = "%caml_bigstring_get16"
external caml_bigstring_get_32 : bigstring -> int -> int32 = "%caml_bigstring_get32"
external caml_bigstring_get_64 : bigstring -> int -> int64 = "%caml_bigstring_get64"

module Swap = struct
  external bswap16 : int -> int = "%bswap16"
  external bswap_int32 : int32 -> int32 = "%bswap_int32"
  external bswap_int64 : int64 -> int64 = "%bswap_int64"

  let caml_bigstring_set_16 bs off i =
    caml_bigstring_set_16 bs off (bswap16 i)

  let caml_bigstring_set_32 bs off i =
    caml_bigstring_set_32 bs off (bswap_int32 i)

  let caml_bigstring_set_64 bs off i =
    caml_bigstring_set_64 bs off (bswap_int64 i)

  let caml_bigstring_get_16 bs off =
    bswap16 (caml_bigstring_get_16 bs off)

  let caml_bigstring_get_32 bs off =
    bswap_int32 (caml_bigstring_get_32 bs off)

  let caml_bigstring_get_64 bs off =
    bswap_int64 (caml_bigstring_get_64 bs off)

  let get_int16_sign_extended x off =
    ((caml_bigstring_get_16 x off) lsl (Sys.int_size - 16)) asr (Sys.int_size - 16)
end

let set_int16_le, set_int16_be =
  if Sys.big_endian
  then Swap.caml_bigstring_set_16, caml_bigstring_set_16
  else caml_bigstring_set_16     , Swap.caml_bigstring_set_16

let set_int32_le, set_int32_be =
  if Sys.big_endian
  then Swap.caml_bigstring_set_32, caml_bigstring_set_32
  else caml_bigstring_set_32     , Swap.caml_bigstring_set_32

let set_int64_le, set_int64_be =
  if Sys.big_endian
  then Swap.caml_bigstring_set_64, caml_bigstring_set_64
  else caml_bigstring_set_64     , Swap.caml_bigstring_set_64

let get_int16_le, get_int16_be =
  if Sys.big_endian
  then Swap.caml_bigstring_get_16, caml_bigstring_get_16
  else caml_bigstring_get_16     , Swap.caml_bigstring_get_16

let get_int16_sign_extended_noswap x off =
  ((caml_bigstring_get_16      x off) lsl (Sys.int_size - 16)) asr (Sys.int_size - 16)

let get_int16_sign_extended_le, get_int16_sign_extended_be  =
  if Sys.big_endian
  then Swap.get_int16_sign_extended  , get_int16_sign_extended_noswap
  else get_int16_sign_extended_noswap, Swap.get_int16_sign_extended

let get_int32_le, get_int32_be =
  if Sys.big_endian
  then Swap.caml_bigstring_get_32, caml_bigstring_get_32
  else caml_bigstring_get_32     , Swap.caml_bigstring_get_32

let get_int64_le, get_int64_be =
  if Sys.big_endian
  then Swap.caml_bigstring_get_64, caml_bigstring_get_64
  else caml_bigstring_get_64     , Swap.caml_bigstring_get_64

(* Unsafe operations *)

external caml_bigstring_unsafe_set_16 : bigstring -> int -> int   -> unit = "%caml_bigstring_set16u"
external caml_bigstring_unsafe_set_32 : bigstring -> int -> int32 -> unit = "%caml_bigstring_set32u"
external caml_bigstring_unsafe_set_64 : bigstring -> int -> int64 -> unit = "%caml_bigstring_set64u"

external caml_bigstring_unsafe_get_16 : bigstring -> int -> int   = "%caml_bigstring_get16u"
external caml_bigstring_unsafe_get_32 : bigstring -> int -> int32 = "%caml_bigstring_get32u"
external caml_bigstring_unsafe_get_64 : bigstring -> int -> int64 = "%caml_bigstring_get64u"

module USwap = struct
  external bswap16 : int -> int = "%bswap16"
  external bswap_int32 : int32 -> int32 = "%bswap_int32"
  external bswap_int64 : int64 -> int64 = "%bswap_int64"

  let caml_bigstring_unsafe_set_16 bs off i =
    caml_bigstring_unsafe_set_16 bs off (bswap16 i)

  let caml_bigstring_unsafe_set_32 bs off i =
    caml_bigstring_unsafe_set_32 bs off (bswap_int32 i)

  let caml_bigstring_unsafe_set_64 bs off i =
    caml_bigstring_unsafe_set_64 bs off (bswap_int64 i)

  let caml_bigstring_unsafe_get_16 bs off =
    bswap16 (caml_bigstring_unsafe_get_16 bs off)

  let caml_bigstring_unsafe_get_32 bs off =
    bswap_int32 (caml_bigstring_unsafe_get_32 bs off)

  let caml_bigstring_unsafe_get_64 bs off =
    bswap_int64 (caml_bigstring_unsafe_get_64 bs off)
end

let unsafe_set_int16_le, unsafe_set_int16_be =
  if Sys.big_endian
  then USwap.caml_bigstring_unsafe_set_16, caml_bigstring_unsafe_set_16
  else caml_bigstring_unsafe_set_16      , USwap.caml_bigstring_unsafe_set_16

let unsafe_set_int32_le, unsafe_set_int32_be =
  if Sys.big_endian
  then USwap.caml_bigstring_unsafe_set_32, caml_bigstring_unsafe_set_32
  else caml_bigstring_unsafe_set_32      , USwap.caml_bigstring_unsafe_set_32

let unsafe_set_int64_le, unsafe_set_int64_be =
  if Sys.big_endian
  then USwap.caml_bigstring_unsafe_set_64, caml_bigstring_unsafe_set_64
  else caml_bigstring_unsafe_set_64      , USwap.caml_bigstring_unsafe_set_64

let unsafe_get_int16_le, unsafe_get_int16_be =
  if Sys.big_endian
  then USwap.caml_bigstring_unsafe_get_16, caml_bigstring_unsafe_get_16
  else caml_bigstring_unsafe_get_16      , USwap.caml_bigstring_unsafe_get_16

let unsafe_get_int16_sign_extended_le x off =
  ((unsafe_get_int16_le x off) lsl (Sys.int_size - 16)) asr (Sys.int_size - 16)

let unsafe_get_int16_sign_extended_be x off =
  ((unsafe_get_int16_be x off ) lsl (Sys.int_size - 16)) asr (Sys.int_size - 16)

let unsafe_get_int32_le, unsafe_get_int32_be =
  if Sys.big_endian
  then USwap.caml_bigstring_unsafe_get_32, caml_bigstring_unsafe_get_32
  else caml_bigstring_unsafe_get_32      , USwap.caml_bigstring_unsafe_get_32

let unsafe_get_int64_le, unsafe_get_int64_be =
  if Sys.big_endian
  then USwap.caml_bigstring_unsafe_get_64, caml_bigstring_unsafe_get_64
  else caml_bigstring_unsafe_get_64      , USwap.caml_bigstring_unsafe_get_64
