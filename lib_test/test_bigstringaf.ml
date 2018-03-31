let index_out_of_bounds () =
  let open Bigstringaf in
  let exn    = Invalid_argument "index out of bounds" in
  let string = "\xde\xad\xbe\xef" in
  let buffer = of_string ~off:0 ~len:(String.length string) string in
  Alcotest.check_raises "get empty 0" exn (fun () -> ignore (get empty  0));
  let check_safe_getter name get =
    Alcotest.check_raises name exn (fun () -> ignore (get buffer (-1)));
    Alcotest.check_raises name exn (fun () -> ignore (get buffer (length buffer)));
  in
  check_safe_getter "get" get;
  check_safe_getter "get_int16_le" get_int16_le;
  check_safe_getter "get_int16_be" get_int16_be;
  check_safe_getter "get_int16_sign_extended_le" get_int16_sign_extended_le;
  check_safe_getter "get_int16_sign_extended_be" get_int16_sign_extended_be;
  check_safe_getter "get_int32_le" get_int32_le;
  check_safe_getter "get_int32_be" get_int32_be;
  check_safe_getter "get_int64_le" get_int64_le;
  check_safe_getter "get_int64_be" get_int64_be;
;;

let getters m () =
  let module Getters = (val m : S.Getters) in
  let open Getters in
  let string = "\xde\xad\xbe\xef\x8b\xad\xf0\x0d" in
  let buffer = Bigstringaf.of_string ~off:0 ~len:(String.length string) string in

  Alcotest.(check char "get" '\xde' (get buffer 0));
  Alcotest.(check char "get" '\xbe' (get buffer 2));

  Alcotest.(check int "get_int16_be" 0xdead (get_int16_be buffer 0));
  Alcotest.(check int "get_int16_be" 0xbeef (get_int16_be buffer 2));
  Alcotest.(check int "get_int16_le" 0xadde (get_int16_le buffer 0));
  Alcotest.(check int "get_int16_le" 0xefbe (get_int16_le buffer 2));
  Alcotest.(check int "get_int16_sign_extended_be" 0x7fffffffffffdead (get_int16_sign_extended_be buffer 0));
  Alcotest.(check int "get_int16_sign_extended_le" 0x7fffffffffffadde (get_int16_sign_extended_le buffer 0));
  Alcotest.(check int "get_int16_sign_extended_le" 0x0df0 (get_int16_sign_extended_le buffer 6));

  Alcotest.(check int32 "get_int32_be" 0xdeadbeefl (get_int32_be buffer 0));
  Alcotest.(check int32 "get_int32_be" 0xbeef8badl (get_int32_be buffer 2));
  Alcotest.(check int32 "get_int32_le" 0xefbeaddel (get_int32_le buffer 0));
  Alcotest.(check int32 "get_int32_le" 0xad8befbel (get_int32_le buffer 2));

  Alcotest.(check int64 "get_int64_be" 0xdeadbeef8badf00dL (get_int64_be buffer 0));
  Alcotest.(check int64 "get_int64_le" 0x0df0ad8befbeaddeL (get_int64_le buffer 0));
;;

let setters m () = 
  let module Setters = (val m : S.Setters) in
  let open Setters in
  let string = Bytes.make 16 '_' |> Bytes.unsafe_to_string in
  let with_buffer ~f =
    let buffer = Bigstringaf.of_string ~off:0 ~len:(String.length string) string in
    f buffer
  in
  let substring ~len buffer = Bigstringaf.substring ~off:0 ~len buffer in
  
  with_buffer ~f:(fun buffer ->
    set buffer 0 '\xde';
    Alcotest.(check string "set" "\xde___" (substring ~len:4 buffer)));

  with_buffer ~f:(fun buffer ->
    set buffer 2 '\xbe';
    Alcotest.(check string "set" "__\xbe_" (substring ~len:4 buffer)));

  with_buffer ~f:(fun buffer ->
    set_int16_be buffer 0 0xdead;
    Alcotest.(check string "set_int16_be" "\xde\xad__" (substring ~len:4 buffer)));

  with_buffer ~f:(fun buffer ->
    set_int16_be buffer 2 0xbeef;
    Alcotest.(check string "set_int16_be" "__\xbe\xef" (substring ~len:4 buffer)));

  with_buffer ~f:(fun buffer ->
    set_int16_le buffer 0 0xdead;
    Alcotest.(check string "set_int16_le" "\xad\xde__" (substring ~len:4 buffer)));

  with_buffer ~f:(fun buffer ->
    set_int16_le buffer 2 0xbeef;
    Alcotest.(check string "set_int16_le" "__\xef\xbe" (substring ~len:4 buffer)));

  with_buffer ~f:(fun buffer ->
    set_int32_be buffer 0 0xdeadbeefl;
    Alcotest.(check string "set_int32_be" "\xde\xad\xbe\xef____" (substring ~len:8 buffer)));

  with_buffer ~f:(fun buffer ->
    set_int32_le buffer 0 0xdeadbeefl;
    Alcotest.(check string "set_int32_le" "\xef\xbe\xad\xde____" (substring ~len:8 buffer)));

  with_buffer ~f:(fun buffer ->
    set_int32_be buffer 2 0xbeef8badl;
    Alcotest.(check string "set_int32_be" "__\xbe\xef\x8b\xad__" (substring ~len:8 buffer)));

  with_buffer ~f:(fun buffer ->
    set_int32_le buffer 2 0xbeef8badl;
    Alcotest.(check string "set_int32_le" "__\xad\x8b\xef\xbe__" (substring ~len:8 buffer)));

  with_buffer ~f:(fun buffer ->
    set_int64_be buffer 0 0xdeadbeef8badf00dL;
    Alcotest.(check string "set_int64_be" "\xde\xad\xbe\xef\x8b\xad\xf0\x0d" (substring ~len:8 buffer)));

  with_buffer ~f:(fun buffer ->
    set_int64_le buffer 0 0xdeadbeef8badf00dL;
    Alcotest.(check string "set_int64_le" "\x0d\xf0\xad\x8b\xef\xbe\xad\xde" (substring ~len:8 buffer)));
;;

let safe_operations = 
  let module Getters : S.Getters = Bigstringaf in
  let module Setters : S.Setters = Bigstringaf in
  [ "index out of bounds", `Quick, index_out_of_bounds
  ; "getters"            , `Quick, getters (module Getters)
  ; "setters"            , `Quick, setters (module Setters) ]

let unsafe_operations = 
  let module Getters : S.Getters = struct
    open Bigstringaf

    let get = unsafe_get 

    let get_int16_le = unsafe_get_int16_le 
    let get_int16_sign_extended_le = unsafe_get_int16_sign_extended_le 
    let get_int32_le = unsafe_get_int32_le 
    let get_int64_le = unsafe_get_int64_le 

    let get_int16_be = unsafe_get_int16_be 
    let get_int16_sign_extended_be = unsafe_get_int16_sign_extended_be 
    let get_int32_be = unsafe_get_int32_be 
    let get_int64_be = unsafe_get_int64_be
  end in
  let module Setters : S.Setters = struct
    open Bigstringaf

    let set = unsafe_set 

    let set_int16_le = unsafe_set_int16_le 
    let set_int32_le = unsafe_set_int32_le 
    let set_int64_le = unsafe_set_int64_le 

    let set_int16_be = unsafe_set_int16_be 
    let set_int32_be = unsafe_set_int32_be 
    let set_int64_be = unsafe_set_int64_be 
  end in
  [ "getters"            , `Quick, getters (module Getters)
  ; "setters"            , `Quick, setters (module Setters) ]

let unsafe_blit () =
  let string1 = "ABCDEFGHIJKLMNOPQRSTUVWXYZ" in
  let string2 = "abcdefghijklmnopqrstuvwxyz" in
  let with_buffers ~f =
    let buffer1 = Bigstringaf.of_string string1 ~off:0 ~len:(String.length string1) in
    let buffer2 = Bigstringaf.of_string string2 ~off:0 ~len:(String.length string2) in
    f buffer1 buffer2
  in
  with_buffers ~f:(fun buf1 buf2 ->
    Bigstringaf.unsafe_blit buf1 ~src_off:0 buf2 ~dst_off:0 ~len:0;
    let new_string2 = Bigstringaf.substring buf2 ~off:0 ~len:(Bigstringaf.length buf2) in
    Alcotest.(check string "empty blit" string2 new_string2));

  with_buffers ~f:(fun buf1 buf2 ->
    Bigstringaf.unsafe_blit buf1 ~src_off:0 buf2 ~dst_off:0 ~len:(Bigstringaf.length buf2);
    let new_string2 = Bigstringaf.substring buf2 ~off:0 ~len:(Bigstringaf.length buf2) in
    Alcotest.(check string "full blit to another buffer" string1 new_string2));

  with_buffers ~f:(fun buf1 _buf2 ->
    Bigstringaf.unsafe_blit buf1 ~src_off:0 buf1 ~dst_off:0 ~len:(Bigstringaf.length buf1);
    let new_string1 = Bigstringaf.substring buf1 ~off:0 ~len:(Bigstringaf.length buf1) in
    Alcotest.(check string "entirely overlapping blit (unchanged)" string1 new_string1));

  with_buffers ~f:(fun buf1 buf2 ->
    Bigstringaf.unsafe_blit buf1 ~src_off:0 buf2 ~dst_off:4 ~len:8;
    let new_string2 = Bigstringaf.substring buf2 ~off:0 ~len:(Bigstringaf.length buf2) in
    Alcotest.(check string "partial blit to another buffer" "abcdABCDEFGHmnopqrstuvwxyz" new_string2));

  with_buffers ~f:(fun buf1 _buf2 ->
    Bigstringaf.unsafe_blit buf1 ~src_off:0 buf1 ~dst_off:4 ~len:8;
    let new_string1 = Bigstringaf.substring buf1 ~off:0 ~len:(Bigstringaf.length buf1) in
    Alcotest.(check string "partially overlapping" "ABCDABCDEFGHMNOPQRSTUVWXYZ" new_string1));
;;

let unsafe_blit_to_bytes   () =
  let string1 = "ABCDEFGHIJKLMNOPQRSTUVWXYZ" in
  let string2 = "abcdefghijklmnopqrstuvwxyz" in
  let with_buffers ~f =
    let buffer1 = string1 in
    let buffer2 = Bigstringaf.of_string string2 ~off:0 ~len:(String.length string2) in
    f buffer1 buffer2
  in
  with_buffers ~f:(fun buf1 buf2 ->
    Bigstringaf.unsafe_blit_from_string buf1 ~src_off:0 buf2 ~dst_off:0 ~len:0;
    let new_string2 = Bigstringaf.substring buf2 ~off:0 ~len:(Bigstringaf.length buf2) in
    Alcotest.(check string "empty blit" string2 new_string2));

  with_buffers ~f:(fun buf1 buf2 ->
    Bigstringaf.unsafe_blit_from_string buf1 ~src_off:0 buf2 ~dst_off:0 ~len:(Bigstringaf.length buf2);
    let new_string2 = Bigstringaf.substring buf2 ~off:0 ~len:(Bigstringaf.length buf2) in
    Alcotest.(check string "full blit to another buffer" string1 new_string2));

  with_buffers ~f:(fun buf1 buf2 ->
    Bigstringaf.unsafe_blit_from_string buf1 ~src_off:0 buf2 ~dst_off:4 ~len:8;
    let new_string2 = Bigstringaf.substring buf2 ~off:0 ~len:(Bigstringaf.length buf2) in
    Alcotest.(check string "partial blit to another buffer" "abcdABCDEFGHmnopqrstuvwxyz" new_string2));
;;

let unsafe_blit_from_bytes () =
  let string1 = "ABCDEFGHIJKLMNOPQRSTUVWXYZ" in
  let string2 = "abcdefghijklmnopqrstuvwxyz" in
  let with_buffers ~f =
    let buffer1 = Bytes.of_string string1 in
    let buffer2 = Bigstringaf.of_string string2 ~off:0 ~len:(String.length string2) in
    f buffer1 buffer2
  in
  with_buffers ~f:(fun buf1 buf2 ->
    Bigstringaf.unsafe_blit_from_bytes buf1 ~src_off:0 buf2 ~dst_off:0 ~len:0;
    let new_string2 = Bigstringaf.substring buf2 ~off:0 ~len:(Bigstringaf.length buf2) in
    Alcotest.(check string "empty blit" string2 new_string2));

  with_buffers ~f:(fun buf1 buf2 ->
    Bigstringaf.unsafe_blit_from_bytes buf1 ~src_off:0 buf2 ~dst_off:0 ~len:(Bigstringaf.length buf2);
    let new_string2 = Bigstringaf.substring buf2 ~off:0 ~len:(Bigstringaf.length buf2) in
    Alcotest.(check string "full blit to another buffer" string1 new_string2));

  with_buffers ~f:(fun buf1 buf2 ->
    Bigstringaf.unsafe_blit_from_bytes buf1 ~src_off:0 buf2 ~dst_off:4 ~len:8;
    let new_string2 = Bigstringaf.substring buf2 ~off:0 ~len:(Bigstringaf.length buf2) in
    Alcotest.(check string "partial blit to another buffer" "abcdABCDEFGHmnopqrstuvwxyz" new_string2));
;;

let blit_operations = 
  [ "unsafe_blit"           , `Quick, unsafe_blit
  ; "unsafe_blit_to_bytes"  , `Quick, unsafe_blit_to_bytes
  ; "unsafe_blit_from_bytes", `Quick, unsafe_blit_from_bytes
  ]

let () =
  Alcotest.run "test suite"
    [ "safe operations"  , safe_operations
    ; "unsafe operations", unsafe_operations
    ; "blit operations"  , blit_operations ]
