open Js_of_ocaml
open Mylang_lib

let parse str = 
  Parser.main Lexer.token (Lexing.from_string str)

let tcheck str = 
  let exp = parse str in
  Tcheck.tcheck "static" [] exp 0

let cast_trans str =
  let exp = parse str in
  let _ = Tcheck.tcheck "static" [] exp 0 in
  Cast.cast_trans [] exp 0

let eval str =
  let exp = parse str in
  let _ = Tcheck.tcheck "static" [] exp 0 in
  let cexp = Cast.cast_trans [] exp 0 in
  Eval.eval cexp [] 0 0

let mylang_lib = 
  (object%js
    method parse s = Js.string(Print.string_of_exp (parse s))
    method tcheck s = Js.string(Print.string_of_tau (tcheck s))
    method cast s = Js.string(Print.string_of_exp (cast_trans s))
    method eval s = Js.string(Print.string_of_result (eval s))
  end)


let () = Js.export_all mylang_lib

let () = Js.Unsafe.set Js.Unsafe.global (Js.string "mylang_lib") mylang_lib
