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


let () =
  Js.export_all
    (object%js
       method parse s = Js.string(Print.string_of_exp (parse s))
       method tcheck s = Js.string(Print.string_of_tau (tcheck s))
       method cast s = Js.string(Print.string_of_exp (cast_trans s))
     end)