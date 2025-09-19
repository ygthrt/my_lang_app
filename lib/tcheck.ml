(*tcheck.ml*)
(*Type check on Staged_Gradual_TypeSystem*) 

open Syntax
(*open Print*)


let emptyenv () = []

let ext env x v i = (x,v,i) :: env

let rec lookup (x,i) env =
  match env with
    | [] -> failwith ("unbound variable in tcheck: " ^ x)
    | (y,v,j)::tl -> if x=y && j <= i then v else lookup (x,i) tl


(* is_consist : tau -> tau -> bool*)
let rec is_consist tau sigma =
  match tau with
  | TDynamic  -> true

  | TCode(t1)  ->
    begin
      match sigma with
      | TDynamic -> true
      | TCode(t2) -> is_consist t1 t2
      | _  -> false
    end
  
  | TArrow(t1,t2)  ->
    begin
      match sigma with
      | TDynamic -> true
      | TArrow(s1,s2) -> (is_consist t1 s1) && (is_consist t2 s2)
      | _  -> false
    end
  
  | _ ->
    begin
      match sigma with
      | TDynamic -> true
      | _  -> tau = sigma
    end 

    


(* tcheck : flag -> tyenv -> exp -> level -> tau *)
let rec tcheck flag te e i =
  (*print_endline ("Lv" ^ string_of_int i ^ " Tcheck: " ^ string_of_exp e);
  print_endline ("Type Enviornment: " ^ string_of_tenv te);*)
  match e with
  | Const(c)  ->
    begin
      match c with
      | IntConst(_)   -> TAtom(TInt)
      | BoolConst(_)  -> TAtom(TBool)
      | FunConst(f)   ->
        begin
          match f with
          | "succ" -> TArrow(TAtom(TInt), TAtom(TInt))
          | _ ->  failwith "type error in Const"
        end
    end
  
  | Var(s)  -> 
    lookup (s,i) te
  
  | Fun(x, sigma, e1)  ->
    let tau = tcheck flag (ext te x sigma i) e1 i in
    TArrow(sigma, tau)
  
  | App(e1, e2)  ->
    let tau1 = tcheck flag te e1 i in
    begin
      match tau1 with
      | TDynamic -> TDynamic
      | TArrow(t1, t2)  ->
        let tau2 = tcheck flag te e2 i in
        if (is_consist t1 tau2) then t2 else failwith "type error in App"
      | _ -> failwith "type error in App"
    end
  
  | Brackets(e1)  ->
    let t = tcheck flag te e1 (i+1) in
    TCode(t)
  
  | Escape(e1)  ->
    if i < 1 then failwith "type error in Escape" 
    else
      let t1 = tcheck flag te e1 (i-1) in
      begin
        match t1 with
        | TDynamic  -> TDynamic
        | TCode(t2)  -> t2
        | _ -> failwith "type error in Escape"
      end
  
  | Cast(tau, e1)  ->
    if (is_consist tau (tcheck flag te e1 i)) then tau else failwith "type(cast) error in Cast"

  

  | Plus(e1, e2)  -> 
    let t1 = tcheck flag te e1 i in
    let t2 = tcheck flag te e2 i in
    if (is_consist t1 (TAtom(TInt))) && (is_consist t2 (TAtom(TInt))) then TAtom(TInt)
    else failwith "type error in Plus"
  
  | Minus(e1, e2)  -> 
    let t1 = tcheck flag te e1 i in
    let t2 = tcheck flag te e2 i in
    if (is_consist t1 (TAtom(TInt))) && (is_consist t2 (TAtom(TInt))) then TAtom(TInt)
    else failwith "type error in Minus"
  
  | Times(e1, e2)  -> 
    let t1 = tcheck flag te e1 i in
    let t2 = tcheck flag te e2 i in
    if (is_consist t1 (TAtom(TInt))) && (is_consist t2 (TAtom(TInt))) then TAtom(TInt)
    else failwith "type error in Times"
  
  | Div(e1, e2)  -> 
    let t1 = tcheck flag te e1 i in
    let t2 = tcheck flag te e2 i in
    if (is_consist t1 (TAtom(TInt))) && (is_consist t2 (TAtom(TInt))) then TAtom(TInt)
    else failwith "type error in Div"
  
  | If(e1, e2, e3) ->
    if flag = "dynamic" then TDynamic
    else
    let t1 = tcheck flag te e1 i in
    let t2 = tcheck flag te e2 i in
    let t3 = tcheck flag te e3 i in
    if (is_consist t1 (TAtom(TBool))) then
      if t2 = t3 then t2
      else TDynamic (*Union*)(*要検討*)
    else failwith "type error in If"
  
  | Eq(e1, e2) ->
    let t1 = tcheck flag te e1 i in
    let t2 = tcheck flag te e2 i in
    if (is_consist t1 t2) then TAtom(TBool)
    else failwith "type error in Eq"
  
  | NotEq(e1, e2) ->
    let t1 = tcheck flag te e1 i in
    let t2 = tcheck flag te e2 i in
    if (is_consist t1 t2) then TAtom(TBool) 
    else failwith "type error in NotEq"
  
  | Greater(e1, e2)  -> 
    let t1 = tcheck flag te e1 i in
    let t2 = tcheck flag te e2 i in
    if (is_consist t1 t2) && (is_consist t1 (TAtom(TInt))) && (is_consist t2 (TAtom(TInt))) then TAtom(TBool)
    else failwith "type error in Greater"
  
  | Less(e1, e2)  -> 
    let t1 = tcheck flag te e1 i in
    let t2 = tcheck flag te e2 i in
    if (is_consist t1 t2) && (is_consist t1 (TAtom(TInt))) && (is_consist t2 (TAtom(TInt))) then TAtom(TBool)
    else failwith "type error in Less"
  
  | Let(x, tau, e1, e2) ->
    let t1 = tcheck flag te e1 i in 
    let t2 = tcheck flag (ext te x tau i) e2 i in
    if (is_consist tau t1) then t2 else failwith "type error in Let"
  
  | LetRec(f, tau1, x, tau2, e1, e2) -> 
    let te1 = (ext (ext te f tau1 i) x tau2 i) in
    let te2 = (ext (ext te x tau2 i) f tau1 i) in
    let sig1 = tcheck flag te1 e1 i in
    let sig2 = tcheck flag te2 e2 i in
    begin
      match tau1 with
      | TArrow(t1, t2) ->
        if (is_consist t1 tau2) && (is_consist t2 sig1) then sig2
        else failwith "type error in LetRec"
      | TDynamic -> sig2
      | _ -> failwith "type error in LetRec"
    end
  
  | IsType(_, e1) ->
    let _ = tcheck flag te e1 i in TAtom(TBool)
  
  | FunVal(x, sigma, e1, env1) ->
    (*print_endline("FunVal has this env1: "^string_of_env env1);*)
    let te1 = make_tenv env1 in
    (*print_endline("Created te1: "^string_of_tenv te1);*)
    let tau = tcheck flag ((ext te x sigma i) @ te1) e1 i in
    TArrow(sigma, tau)
  
  | RecFunVal(_, tau1, _, tau2, _, _) -> 
    begin
      match tau1 with
      | TDynamic -> TArrow(tau2,TDynamic)
      | _ -> tau1
    end

  | CodeVal(e1, te1)  -> 
    let t = tcheck flag te1 e1 (i+1) in
    TCode(t)

  | CodeVar(tau) -> tau

  | IfVal(_, _, _, v4) ->
    begin
      match v4 with
      | Exp(exp)  -> tcheck flag te exp i
      | _ -> failwith "type error in If"
    end

  | LetVal(x, tau, e1, e2) ->
    if tau = TDynamic then
      begin
        match e1 with
        | Cast(TDynamic, exp) ->
          let t1 = tcheck flag te exp i in 
          tcheck flag (ext te x t1 i) e2 i
        | _ -> failwith "typeerror in LetVal"
      end
    else
      let t1 = tcheck flag te e1 i in 
      let t2 = tcheck flag (ext te x tau i) e2 i in
      if (is_consist tau t1) then t2 else failwith "type error in LetVal"

  | LetRecVal(_, tau1, _, tau2, e1, e2, te1) -> 
    let sig1 = tcheck flag te1 e1 i in
    let sig2 = tcheck flag te1 e2 i in
    begin
      match tau1 with
      | TArrow(t1, t2) ->
        if (is_consist t1 tau2) && (is_consist t2 sig1) then sig2
        else failwith "type error in LetRecVal"
      | TDynamic -> sig2
      | _ -> failwith "type error in LetRecVal"
    end

  | _ -> failwith "unknown expression in tcheck flag"

and
  make_tenv env =
    match env with
    | [] -> []
    | (x,v,j)::tl ->
      begin
        (*print_endline("match result: " ^ x ^", "^string_of_result v );*)
        match v with
        | Exp(Cast(tau, e1)) -> 
          if tau = TDynamic then
            begin
              match e1 with
              | Var(_) -> make_tenv tl
              | CodeVal(e2, te1) -> te1 @ (x, TCode(tcheck "static" te1 e2 (j+1)), j) :: make_tenv tl
              | _  -> (x, tcheck "static" [] e1 j, j) :: make_tenv tl
            end
          else
            (x, tau, j) :: make_tenv tl
        | Exp(Var(_)) -> make_tenv tl
        | Exp(CodeVal(e1, te1)) -> te1 @ (x, TCode(tcheck "static" te1 e1 (j+1)), j) :: make_tenv tl
        | Exp(exp) -> (x, tcheck "static" [] exp j, j) :: make_tenv tl
        | Error(_) -> failwith "error in env"
      end
