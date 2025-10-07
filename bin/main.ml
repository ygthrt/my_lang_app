open Js_of_ocaml
open Mylang_lib

let parse str = 
  Parser.program Lexer.token (Lexing.from_string str)

let tcheck str =
  let initial_env = [] in
  let phrases = parse str in
  let ( _ , reversed_results) = List.fold_left
    (fun (current_env, results_so_far) exp ->
      let (next_env, new_tau) = Tcheck.tcheck_phrase current_env exp in
      (next_env, new_tau :: results_so_far)
    )
    (initial_env, [])
    phrases
  in
  List.rev reversed_results

let cast_trans str =
  let initial_env = [] in
  let phrases = parse str in
  let _ = tcheck str in
  let ( _ , reversed_results) = List.fold_left
    (fun (current_env, results_so_far) exp ->
      let (next_env, new_cexp) = Cast.cast_trans_phrase current_env exp in
      (next_env, new_cexp :: results_so_far)
    )
    (initial_env, [])
    phrases
  in
  List.rev reversed_results

let eval str =
  let initial_env = [] in
  let tcheck_res = tcheck str in
  let casted_phrases = cast_trans str in
  let ( _ , reversed_results) = List.fold_left
    (fun (current_env, results_so_far) exp ->
      let (next_env, new_result) = Eval.eval_phrase current_env exp in
      (next_env, new_result :: results_so_far)
    )
    (initial_env, [])
    casted_phrases
  in
  (List.rev reversed_results , tcheck_res)


let mylang_lib = 
  (object%js
    method parse s = Js.string(Print.string_of_program (parse s))
    method tcheck s = Js.string(Print.string_of_tau_list (tcheck s))
    method cast s = Js.string(Print.string_of_program (cast_trans s))
    method eval s = Js.string(Print.string_of_eval_result (eval s))
  end)


let () = Js.export_all mylang_lib

let () = Js.Unsafe.set Js.Unsafe.global (Js.string "mylang_lib") mylang_lib
