(*print.ml*)
(*printer of inner structure on Staged_Gradual_TypeSystem*)

open Syntax

(*string_of_exp : exp -> string*)
  let rec string_of_exp exp =
    match exp with
    | Const (IntConst i) -> string_of_int i
    | Const (BoolConst b) -> string_of_bool b
    | Const (FunConst s) -> Printf.sprintf "%s" s
    | Var x -> x
    | Fun (x, tau, body) ->
        if tau = TDynamic then
          "fun " ^ x ^ " -> " ^ string_of_exp body
        else "fun " ^ x ^ " : " ^ string_of_tau tau ^ " -> " ^ string_of_exp body
    | App (e1, e2) -> 
        "(" ^ string_of_exp e1 ^ ") " ^ string_of_exp e2
    | Brackets e -> ".<" ^ string_of_exp e ^ ">."
    | Escape e -> ".~" ^ string_of_exp e
    | Cast (tau, e) ->
      "〈" ^ string_of_tau tau ^ "〉" ^ string_of_exp e
    | If (e1, e2, e3) -> 
        "if " ^ string_of_exp e1 ^ " then " ^ string_of_exp e2 ^ " else " ^ string_of_exp e3
    | Let (x, tau, e1, e2) ->
        if tau = TDynamic then
          "let " ^ x ^ " = " ^ string_of_exp e1 ^ " in " ^ string_of_exp e2
        else "let " ^ x ^ " : " ^ string_of_tau tau ^ " = " ^ string_of_exp e1 ^ " in " ^ string_of_exp e2
    | LetRec (f, tau1, x, tau2, e1, e2) ->
        if tau1 = TDynamic then
          if tau2 = TDynamic then
            "let rec " ^ f ^ " " ^ x ^ " = " ^ string_of_exp e1 ^ " in " ^ string_of_exp e2
          else "let rec " ^ f ^ " " ^ x ^ " : " ^ string_of_tau tau2 ^ " = " ^ string_of_exp e1 ^ " in " ^ string_of_exp e2
        else
          "let rec " ^ f ^ " : " ^ string_of_tau tau1 ^ " " ^ x ^ " : " ^ string_of_tau tau2 ^ 
          " = " ^ string_of_exp e1 ^ " in " ^ string_of_exp e2
    | Eq (e1, e2) -> 
        string_of_exp e1 ^ " = " ^ string_of_exp e2
    | NotEq (e1, e2) -> 
        string_of_exp e1 ^ " <> " ^ string_of_exp e2
    | Greater (e1, e2) -> 
        string_of_exp e1 ^ " > " ^ string_of_exp e2
    | Less (e1, e2) -> 
        string_of_exp e1 ^ " < " ^ string_of_exp e2
    | Plus (e1, e2) -> 
        "(" ^ string_of_exp e1 ^ " + " ^ string_of_exp e2 ^ ")"
    | Minus (e1, e2) -> 
        if (string_of_exp e1) = "0" then  "(" ^ " -" ^ string_of_exp e2 ^ ")"
        else "(" ^ string_of_exp e1 ^ " - " ^ string_of_exp e2 ^ ")"
    | Times (e1, e2) -> 
        string_of_exp e1 ^ " * " ^ string_of_exp e2
    | Div (e1, e2) -> 
        string_of_exp e1 ^ " / " ^ string_of_exp e2

    | FunVal(x, tau, body, _) ->
      if tau = TDynamic then
          "fun " ^ x ^ " -> " ^ string_of_exp body
        else "fun " ^ x ^ " : " ^ string_of_tau tau ^ " -> " ^ string_of_exp body
    | RecFunVal(f,tau1,x,tau2,e1,_) ->
      if tau1 = TDynamic then
          if tau2 = TDynamic then
            "let rec " ^ f ^ " " ^ x ^ " = " ^ string_of_exp e1
          else "let rec " ^ f ^ " " ^ x ^ " : " ^ string_of_tau tau2 ^ " = " ^ string_of_exp e1
        else
          "let rec " ^ f ^ " : " ^ string_of_tau tau1 ^ " " ^ x ^ " : " ^ string_of_tau tau2 ^ 
          " = " ^ string_of_exp e1
    | CodeVal(e1,_) -> ".< " ^ string_of_exp e1 ^ " >."
    | CodeVar(tau) -> string_of_tau tau
    | IfVal(e1, e2, e3, _) ->
      "if " ^ string_of_exp e1 ^ " then " ^ string_of_exp e2 ^ " else " ^ string_of_exp e3
    | LetVal(x, tau, e1, e2) ->
      if tau = TDynamic then
          "let " ^ x ^ " = " ^ string_of_exp e1 ^ " in " ^ string_of_exp e2
        else "let " ^ x ^ " : " ^ string_of_tau tau ^ " = " ^ string_of_exp e1 ^ " in " ^ string_of_exp e2
    |LetRecVal(f, tau1, x, tau2, e1, e2, _) ->
        if tau1 = TDynamic then
          if tau2 = TDynamic then
            "let rec " ^ f ^ " " ^ x ^ " = " ^ string_of_exp e1 ^ " in " ^ string_of_exp e2
          else "let rec " ^ f ^ " " ^ x ^ " : " ^ string_of_tau tau2 ^ " = " ^ string_of_exp e1 ^ " in " ^ string_of_exp e2
        else
          "let rec " ^ f ^ " : " ^ string_of_tau tau1 ^ " " ^ x ^ " : " ^ string_of_tau tau2 ^ 
          " = " ^ string_of_exp e1 ^ " in " ^ string_of_exp e2
    | IsType(tau, e1) ->
      "is_type(" ^ string_of_tau tau ^ ", " ^ string_of_exp e1 ^ ")"

    | Seq (e1, e2) ->
      string_of_exp e1 ^ ";\n" ^ string_of_exp e2
  
    | _ -> failwith "unknown expression in print"

  and string_of_tau tau =
    match tau with
    | TAtom TInt -> "int"
    | TAtom TBool -> "bool"
    | TDynamic -> "?"
    | TArrow (t1, t2) -> 
        "(" ^ string_of_tau t1 ^ " --> " ^ string_of_tau t2 ^ ")"
    | TCode t -> 
        "code(" ^ string_of_tau t ^ ")"


(*string_of_result : result -> string*)
let rec string_of_result res =
  match res with
  | Exp e -> string_of_exp e
  | Error e -> string_of_error e
and string_of_error e =
  match e with
  | CastError -> "CastError"
  | TypeError(s) -> "TypeError: " ^ s
  | KillError -> "KillError"


(* string_of_env : env -> string *)
let string_of_env env =
  let string_of_binding (x, res, lvl) =
    Printf.sprintf "(%s, %s, %d)" x (string_of_result res) lvl
  in
  "[" ^ (String.concat "; " (List.map string_of_binding env)) ^ "]"


(* string_of_tenv : tyenv -> string *)
let string_of_tenv tyenv =
  let string_of_binding (x, tau, lvl) =
    Printf.sprintf "(%s, %s, %d)" x (string_of_tau tau) lvl
  in
  "[" ^ (String.concat "; " (List.map string_of_binding tyenv)) ^ "]"

let string_of_toplevel phrase =
  match phrase with
  | Expression(e) ->
    string_of_exp(e)
  | LetDefinition(x,tau,e) ->
    if tau = TDynamic then
      "let " ^ x ^ " = " ^ string_of_exp e
    else "let " ^ x ^ " : " ^ string_of_tau tau ^ " = " ^ string_of_exp e

let rec string_of_program phrase_list =
  match phrase_list with
  | [] -> ""
  | h :: tl -> string_of_toplevel h ^ "\n" ^ string_of_program tl 

let rec string_of_tau_list tau_list =
  match tau_list with
  | [] -> ""
  | h :: tl -> (string_of_tau h) ^ ";;\n" ^ (string_of_tau_list tl)

let string_of_toplevel_result_and_tau toplevel_result tau =
  match toplevel_result with
  | Result(res) -> "- : "^ (string_of_tau tau) ^" = " ^ (string_of_result res)
  | LetBinding(x, res) -> "val "^ x ^ " : "^ (string_of_tau tau) ^" = " ^ (string_of_result res)

let rec string_of_eval_result (toplevel_result_list ,tau_list) =
  match (toplevel_result_list, tau_list) with 
  | (h1 :: tl1 , h2 :: tl2) ->  (string_of_toplevel_result_and_tau h1 h2) ^"\n"^ string_of_eval_result (tl1, tl2) 
  | _ -> ""

(* print_result : tau -> result -> unit() *)
let print_result tau result =
  match result with
  | Exp(_) ->
    let res = string_of_tau tau ^ " = " ^ string_of_result result in
    print_endline(res);
  | Error(ep) ->
    let res = string_of_error ep in
    print_endline(res);
