(module
   (import "env" "caml_bigstring_blit_ba_to_ba"
      (func $bigstringaf_blit_to_bigstring
         (param (ref eq)) (param (ref eq)) (param (ref eq)) (param (ref eq))
         (param (ref eq)) (result (ref eq))))
   (import "env" "caml_bigstring_blit_bytes_to_ba"
      (func $bigstringaf_blit_from_bytes
         (param (ref eq)) (param (ref eq)) (param (ref eq)) (param (ref eq))
         (param (ref eq)) (result (ref eq))))
   (import "env" "caml_bigstring_blit_ba_to_bytes"
      (func $bigstringaf_blit_to_bytes
         (param (ref eq)) (param (ref eq)) (param (ref eq)) (param (ref eq))
         (param (ref eq)) (result (ref eq))))
   (import "env" "caml_bigstring_memcmp"
      (func $bigstringaf_memcmp_bigstring
         (param (ref eq)) (param (ref eq)) (param (ref eq)) (param (ref eq))
         (param (ref eq)) (result (ref eq))))
   (import "env" "caml_bigstring_memcmp_string"
      (func $bigstringaf_memcmp_string
         (param (ref eq)) (param (ref eq)) (param (ref eq)) (param (ref eq))
         (param (ref eq)) (result (ref eq))))
   (import "env" "caml_bigstring_memchr"
      (func $caml_bigstring_memchr
         (param (ref eq)) (param (ref eq)) (param (ref eq)) (param (ref eq))
         (result (ref eq))))

   (export "bigstringaf_blit_to_bytes" (func $bigstringaf_blit_to_bytes))
   (export "bigstringaf_blit_to_bigstring" (func $bigstringaf_blit_to_bigstring))
   (export "bigstringaf_blit_from_bytes" (func $bigstringaf_blit_from_bytes))
   (export "bigstringaf_memcmp_bigstring" (func $bigstringaf_memcmp_bigstring))
   (export "bigstringaf_memcmp_string" (func $bigstringaf_memcmp_string))

   (func (export "bigstringaf_memchr")
      (param $ba (ref eq)) (param $off (ref eq)) (param $chr (ref eq))
      (param $len (ref eq)) (result (ref eq))
      (return_call $caml_bigstring_memchr
         (local.get $ba) (local.get $chr) (local.get $off) (local.get $len)))
)
