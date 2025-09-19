(* cast.ml*)
(* Cast Insertion on Staged_Gradual_TypeSystem *)

open Syntax
open Tcheck

type tyenv = (string * tau * level) list

let emptyenv () = []

let ext env x v i = (x,v,i) :: env

let rec lookup (x,i) env =
  match env with
    | [] -> failwith ("unbound variable: " ^ x)
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


(* cast_trans : tyenv -> exp -> level -> exp *)
let rec cast_trans te e i =
  match e with
  | Const(_)  -> e

  | Var(_)  -> e
  
  | Fun(x, sigma, e1)  -> 
    let ce1 = cast_trans (ext te x sigma i) e1 i in
    Fun(x, sigma, ce1)
  
  | App(e1, e2)  ->
    let ce1 = cast_trans te e1 i in
    let ce2 = cast_trans te e2 i in
    let tau1 = tcheck "static" te ce1 i in
    let tau2 = tcheck "static" te ce2 i in
    begin
      match tau1 with
      | TDynamic -> App(Cast(TArrow(tau2, TDynamic), ce1), ce2)
      | TArrow(tau11, _)  ->
        if tau2 = tau11 then App(ce1,ce2)
        else if (is_consist tau2 tau11) then App(ce1,Cast(tau11, ce2))
        else failwith "cast_trans fails in App"
      | _ -> failwith "cast_trans fails in App"
    end
  
  | Brackets(e1)  ->
    let ce1 = cast_trans te e1 (i+1) in
    Brackets(ce1)
  
  | Escape(e1)  ->
    if i < 1 then failwith "cast_trans fails in Escape" 
    else
      let ce1 = cast_trans te e1 (i-1) in
      let tau1 = tcheck "static" te ce1 (i-1) in
      begin
        match tau1 with
        | TDynamic  -> Escape(Cast(TCode(TDynamic), ce1))
        | TCode(_)  -> Escape(ce1) 
        | _ -> failwith "cast_trans fails in Escape"
      end
  
  | Cast(tau, e1)  ->
    let ce1 = cast_trans te e1 i in 
    Cast(tau, ce1)


  | Plus(e1,e2)  ->
    let e11 = cast_trans te e1 i in
    let e21 = cast_trans te e2 i in
    let t11 = tcheck "static" te e11 i in
    let t21 = tcheck "static" te e21 i in
    begin
      let ce1 = match t11 with
      | TDynamic  -> Cast(TAtom(TInt), e11)
      | TAtom(TInt)  -> e11
      | _ -> failwith "cast_trans fails in Plus"
      in
      let ce2 = match t21 with
      | TDynamic  -> Cast(TAtom(TInt), e21)
      | TAtom(TInt)  -> e21
      | _ -> failwith "cast_trans fails in Plus"
      in
      Plus(ce1,ce2)
    end
  
  | Minus(e1,e2)  -> 
    let e11 = cast_trans te e1 i in
    let e21 = cast_trans te e2 i in
    let t11 = tcheck "static" te e11 i in
    let t21 = tcheck "static" te e21 i in
    begin
      let ce1 = match t11 with
      | TDynamic  -> Cast(TAtom(TInt), e11)
      | TAtom(TInt)  -> e11
      | _ -> failwith "cast_trans fails in Minus"
      in
      let ce2 = match t21 with
      | TDynamic  -> Cast(TAtom(TInt), e21)
      | TAtom(TInt)  -> e21
      | _ -> failwith "cast_trans fails in Minus"
      in
      Minus(ce1,ce2)
    end
  
  | Times(e1,e2)  -> 
    let e11 = cast_trans te e1 i in
    let e21 = cast_trans te e2 i in
    let t11 = tcheck "static" te e11 i in
    let t21 = tcheck "static" te e21 i in
    begin
      let ce1 = match t11 with
      | TDynamic  -> Cast(TAtom(TInt), e11)
      | TAtom(TInt)  -> e11
      | _ -> failwith "cast_trans fails in Times"
      in
      let ce2 = match t21 with
      | TDynamic  -> Cast(TAtom(TInt), e21)
      | TAtom(TInt)  -> e21
      | _ -> failwith "cast_trans fails in Times"
      in
      Times(ce1,ce2)
    end
  
  | Div(e1,e2)  -> 
    let e11 = cast_trans te e1 i in
    let e21 = cast_trans te e2 i in
    let t11 = tcheck "static" te e11 i in
    let t21 = tcheck "static" te e21 i in
    begin
      let ce1 = match t11 with
      | TDynamic  -> Cast(TAtom(TInt), e11)
      | TAtom(TInt)  -> e11
      | _ -> failwith "cast_trans fails in Div"
      in
      let ce2 = match t21 with
      | TDynamic  -> Cast(TAtom(TInt), e21)
      | TAtom(TInt)  -> e21
      | _ -> failwith "cast_trans fails in Div"
      in
      Div(ce1,ce2)
    end
  
  | If(e1, e2, e3) ->(*要検討*)
    let e11 = cast_trans te e1 i in
    let e21 = cast_trans te e2 i in
    let e31 = cast_trans te e3 i in
    let t11 = tcheck "static" te e11 i in
    let t21 = tcheck "static" te e21 i in
    let t31 = tcheck "static" te e31 i in
    begin
      let ce1 = match t11 with
      | TDynamic -> Cast(TAtom(TBool), e11)
      | TAtom(TBool) -> e11
      | _ -> failwith "cast_trans fails in If"
      in
      match (t21, t31) with
      | (TDynamic, TDynamic) -> If(ce1, Cast(TDynamic, e21), Cast(TDynamic, e31))(*要検討*)
      | (TDynamic, _ ) -> If(ce1, e21, Cast(TDynamic, e31))
      | ( _ , TDynamic) -> If(ce1, Cast(TDynamic, e21), e31)
      | ( _ , _ ) -> if t21 = t31 then If(ce1, e21, e31) else If(ce1, Cast(TDynamic, e21), Cast(TDynamic, e31))
    end
  
  | Eq(e1,e2) ->
    let ce1 = cast_trans te e1 i in
    let ce2 = cast_trans te e2 i in
    let t1 = tcheck "static" te ce1 i in
    let t2 = tcheck "static" te ce2 i in
    if t1 = t2 then Eq(ce1, ce2)
    else
      begin
        match (t1, t2) with
        | (TDynamic, _ ) -> Eq(Cast(t2, ce1), ce2)
        | ( _ , TDynamic) -> Eq(ce1, Cast(t1,ce2))
        | _ -> failwith "cast_trans fails in Eq"
      end
  
  | NotEq(e1,e2) -> 
    let ce1 = cast_trans te e1 i in
    let ce2 = cast_trans te e2 i in
    let t1 = tcheck "static" te ce1 i in
    let t2 = tcheck "static" te ce2 i in
    if t1 = t2 then NotEq(ce1, ce2)
    else
      begin
        match (t1, t2) with
        | (TDynamic, _ ) -> NotEq(Cast(t2, ce1), ce2)
        | ( _ , TDynamic) -> NotEq(ce1, Cast(t1,ce2))
        | _ -> failwith "cast_trans fails in NotEq"
      end
  
  | Greater(e1,e2)  -> 
    let e11 = cast_trans te e1 i in
    let e21 = cast_trans te e2 i in
    let t1 = tcheck "static" te e11 i in
    let t2 = tcheck "static" te e21 i in
    begin
      let ce1 = match t1 with
      | TDynamic -> Cast(TAtom(TInt), e11)
      | TAtom(TInt) -> e11
      | _ -> failwith "cast_trans fails in Greater"
      in
      let ce2 = match t2 with
      | TDynamic -> Cast(TAtom(TInt), e21)
      | TAtom(TInt) -> e21
      | _ -> failwith "cast_trans fails in Greater"
      in
      Greater(ce1, ce2)
    end
  
  | Less(e1,e2)  -> 
    let e11 = cast_trans te e1 i in
    let e21 = cast_trans te e2 i in
    let t1 = tcheck "static" te e11 i in
    let t2 = tcheck "static" te e21 i in
    begin
      let ce1 = match t1 with
      | TDynamic -> Cast(TAtom(TInt), e11)
      | TAtom(TInt) -> e11
      | _ -> failwith "cast_trans fails in Less"
      in
      let ce2 = match t2 with
      | TDynamic -> Cast(TAtom(TInt), e21)
      | TAtom(TInt) -> e21
      | _ -> failwith "cast_trans fails in Less"
      in
      Less(ce1, ce2)
    end
  
  | Let(x,tau,e1,e2) ->
    let e11 = cast_trans te e1 i in 
    let e21 = cast_trans (ext te x tau i) e2 i in
    let t1 = tcheck "static" te e11 i in
    if (is_consist tau t1) then 
      if tau = t1 then Let(x,tau,e11,e21)
      else Let(x,tau,Cast(tau,e11),e21)
    else failwith "cast_trans fails in Let"
  
  | LetRec(f,tau1,x,tau2,e1,e2) -> 
    let te1 = (ext (ext te f tau1 i) x tau2 i) in
    let te2 = (ext (ext te x tau2 i) f tau1 i) in
    let e11 = cast_trans te1 e1 i in
    let e21 = cast_trans te2 e2 i in
    let t1 = tcheck "static" te1 e11 i in
    begin
      match tau1 with
      | TArrow(t11, t21) ->
        if (is_consist t11 tau2) then
          if (is_consist t21 t1) then 
            if t21 = t1 then LetRec(f,tau1,x,tau2,e11,e21)
            else LetRec(f,tau1,x,tau2,Cast(t21,e11),e21)
          else failwith "cast_trans fails in LetRec"
        else failwith "cast_trans fails in LetRec"
      | TDynamic -> LetRec(f,tau1,x,tau2,Cast(TDynamic,e11),e21)
      | _ -> failwith "cast_trans fails in LetRec"
    end
  
  | IsType(tau, e1) ->
    let ce1 = cast_trans te e1 i in
    IsType(tau, ce1)
  
  | FunVal(_) -> failwith "worng expression in cast_trans"
  
  | RecFunVal(_) -> failwith "worng expression in cast_trans"

  | IfVal(_) -> failwith "worng expression in cast_trans"

  | _ -> failwith "unknown expression in cast_trans"

