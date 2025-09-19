open Js_of_ocaml
open Mylang_lib

let parse str = 
  Parser.main Lexer.token (Lexing.from_string str)

let () =
  Js.export_all
    (object%js
       method parse s = Js.string(Print.string_of_exp (parse s))
     end)