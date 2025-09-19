(*evalSG.ml*)
(*evaluation on Staged_Gradual_TypeSystem*)

open Syntax
open Tcheck
(*open Print*)

let emptyenv () = []

let ext env x v i = (x,v,i) :: env

let rec lookup (x,i) env =
  match env with
    | [] -> failwith ("unbound variable: " ^ x)
    | (y,v,j)::tl -> 
      if x=y && j <= i then v
      else lookup (x,i) tl

let counter = ref 1

(* eval : exp -> (string * result * level) list -> level -> steps -> result *)
let rec eval e env i n =
  (*print_endline ( "Lv."^string_of_int i ^" Eval:" ^ string_of_exp e);
  print_endline ("Enviornment:" ^ string_of_env env);*)
  if n > 50 then Error(KillError)
  else
  
  let unbox v =
    match v with
    | Exp(Cast(TDynamic, e)) -> Exp(e)
    | _ -> v
  in
  
  let binop f v2 v1 =
    (*print_endline(string_of_result v2);
    print_endline(string_of_result v1);*)
    match ((unbox v2), (unbox v1)) with
    | Exp(Const(IntConst(n2))), Exp(Const(IntConst(n1))) -> Exp(Const(IntConst(f n1 n2)))
    | _ -> Error(TypeError("error in binop"))
  in
  
  let rec fresh_var z env =
    let rec is_conflict x env1 =
      match env1 with
      | [] -> false
      | (y,_,_)::tl -> if x=y then true else is_conflict x tl
    in
    let current = !counter in
    let name = Printf.sprintf "%s_%d" z current in
    counter := current + 1;
    if (is_conflict name env) then (fresh_var z env) else name
  in

  let rec change_env_level env i j =
    match env with
    | [] -> []
    | (x, v, k) :: tl -> if i = k then (change_env_level tl i j) @ [(x, v, j)]  else (change_env_level tl i j)
  in

  match e with
  | Const(c)  -> Exp(Const(c))
  
  | Var(s)  -> (*要検討*)
    lookup (s, i) env
    (*if i = 0 then lookup (s, i) env
    else
      let v = lookup (s, i) env in
      begin
        match v with
        | Exp(Const(c)) -> Exp(Const(c))
        | _ -> Exp(Var(s))
      end*)

  | Fun(x, tau, e1)  ->
    if i = 0 then
      Exp(FunVal(x, tau, e1, env))
    else
      let new_x = fresh_var x env in
      let env1 = ext (ext env x (Exp(Var(new_x))) i) new_x (Exp(CodeVar(tau))) i in
      let v1 = eval e1 env1 i (n+1) in
      begin
        match v1 with
        | Error(ep) -> Error(ep)
        | Exp(exp) -> Exp(FunVal(new_x, tau, exp, env))
      end
  
  | FunVal(x, sigma, e1, env)  -> Exp(FunVal(x, sigma, e1, env))
  
  | App(e1, e2)  ->
    let argv = eval e2 env i (n+1) in
    let funpartv = eval e1 env i (n+1) in
    begin
      match (argv,funpartv) with
      | (Error(ep), _ ) -> Error(ep)
      | ( _ , Error(ep)) -> Error(ep)
      | (Exp(arg), Exp(funpart)) ->
        if i = 0 then
          begin
            match funpart with
            | Const(FunConst(f)) ->
              begin
                match f with
                | "succ" ->
                  begin
                    match arg with
                    | Const(IntConst(n)) -> Exp(Const(IntConst(n+1)))
                    | _ -> Error(TypeError("error in succ"))
                  end
                | _ -> Error(TypeError("unknown FunConst"))
              end
            | FunVal(x, _, body, env1) ->
              let env2 = ext env1 x (Exp(arg)) i in
              eval body env2 i (n+1)
            | RecFunVal(f, _, x, _, body ,env1) ->
              let env2 = ext (ext env1 f (Exp(funpart)) i) x (Exp(arg)) i in
              eval body env2 i (n+1)
            | _ -> Error(TypeError("There is no FunVal"))
          end
        else
          Exp(App(funpart, arg))
    end

  | Brackets(e1)  ->
    let v1 = eval e1 env (i+1) (n+1) in
    begin
      match v1 with
      | Error(ep) -> Error(ep)
      | Exp(exp) ->
        (*print_endline("Brackets makes te from: "^ string_of_env env);*)
        let te1 = make_tenv env in
        (*print_endline("CodeVal keeps this: "^string_of_tenv te1);*)
        Exp(CodeVal(exp, te1))
    end

  | CodeVal(e1, env1)  -> Exp(CodeVal(e1, env1))

  | Escape(e1)  ->
    if i < 1 then Error(TypeError("Escape used in level 0"))
    else
      let v1 = eval e1 env (i-1) (n+1) in
      begin
        match v1 with
        | Error(ep) -> Error(ep)
        | Exp(exp) ->
          begin
            match exp with
            | Brackets(exp1) -> if i=1 then Exp(exp1) else Exp(Escape(exp))
            | CodeVal(exp1,_) -> if i=1 then Exp(exp1) else Exp(Escape(exp))
            | _ -> Error(TypeError("There is no CodeVal"))
          end
      end
              
  | Cast(tau, e1)  ->
    (*print_endline("Evalating: "^string_of_exp (Cast(tau,e1)));*)
    let v1 = eval e1 env i (n+1) in
    (*print_endline("casted v1: "^string_of_result v1);*)
    begin
      match v1 with
      | Error(ep) -> Error(ep)
      | _ ->
        let uv1 = unbox v1 in
        (*print_endline("unboxed v1: " ^ string_of_result uv1);*)
        begin
          let uv2 = match uv1 with
          | Exp(Var(s)) -> unbox (lookup(s, i) env)
          | _ -> uv1
          in
          match uv2 with
          | Exp(exp) ->
            (*print_endline ("match exp: " ^ string_of_exp exp);
            print_endline ("create tenv from: " ^ string_of_env env);*)
            if tau = TDynamic then
              begin
                match uv1 with
                | Exp(Var(s)) -> Exp(Cast(tau, Var(s)))
                | _ -> Exp(Cast(tau, exp))
              end
            else
              let te = make_tenv env in
              (*print_endline ("Lv"^string_of_int i ^" Tcheck in Cast: " ^ string_of_exp exp);
              print_endline ("Created Type Enviornment: " ^ string_of_tenv te);*)
              let t1 = tcheck "dynamic" te exp i in
              (*print_endline (string_of_tau t1);*)
              if not (is_consist tau t1) then Error(CastError)
              else if t1 = TDynamic then
                if exp = CodeVar(TDynamic) then
                  begin
                    match uv1 with
                    | Exp(Var(s)) -> Exp(Cast(tau, Var(s)))
                    | _ -> Exp(Cast(tau, exp))
                  end
                else Error(TypeError("cannot use TDynamic in run-time"))
              else
                begin
                  match tau with
                  | TAtom(_) -> if t1 = tau then uv1 else Error(TypeError("typeerror in Atomic type"))
                  | TArrow(sig1, sig2) ->
                    begin
                      match t1 with
                      | TArrow(tau1,_) ->
                        if (is_consist tau t1) then
                          begin
                          (*print_endline("match exp: "^string_of_exp exp);*)
                            match exp with
                            | FunVal( _ , _ , _ , env1) ->
                              let z = fresh_var "z" env in
                              let arg_exp = Cast(tau1,Var(z)) in
                              let body_exp = Cast(sig2, App(exp,arg_exp)) in
                              (*print_endline("Created FunVal: "^string_of_result (Exp(FunVal(z, sig1, body_exp, env1))));
                              print_endline("Created FunVal env: " ^ string_of_env env1);*)
                              Exp(FunVal(z, sig1, body_exp, env1))
                            | RecFunVal( _ , _ , _ , _ , _ , env1) ->
                              let z = fresh_var "z" env in
                              let arg_exp = Cast(tau1,Var(z)) in
                              let body_exp = Cast(sig2, App(exp,arg_exp)) in
                              (*print_endline("Created FunVal: "^string_of_result (Exp(FunVal(z, sig1, body_exp, env1))));
                              print_endline("Created FunVal env: " ^ string_of_env env1);*)
                              Exp(FunVal(z, sig1, body_exp, env1))
                            | _ -> Error(TypeError("typeerror in TArrow"))
                          end
                        else Error(TypeError("cannot apply"))
                      | _ -> Error(TypeError("typeerror in Func type"))
                    end
                  | TCode(sig1) -> 
                    if sig1 = TDynamic then
                      begin
                        match t1 with
                        | TCode(_) ->
                          begin
                            match exp with
                            | Brackets(inner) -> Exp(Brackets(Cast(TDynamic, inner)))
                            | CodeVal(inner,te1) -> Exp(CodeVal(Cast(TDynamic,inner), te1))
                            | _ -> Error(TypeError("typeerror in Code type"))
                          end
                        | _ -> Error(TypeError("typeerror in Code type"))
                      end
                    else if t1 = tau then uv1
                    else Error(TypeError("typeerror in Code type"))
                  | _ -> Error(TypeError("unknown type in run-time tcheck"))
                end
          | _ -> Error(TypeError("maybe unbox is bad"))
        end
    end



  | Plus(e1, e2)  ->
    let v1 = eval e1 env i (n+1) in
    let v2 = eval e2 env i (n+1) in
    begin
      match (v1, v2) with
      | (Error(ep), _ ) -> Error(ep)
      | ( _ , Error(ep)) -> Error(ep)
      | (Exp(exp1), Exp(exp2)) -> 
        if i = 0 then binop (+) v2 v1
        else
          Exp(Plus(exp1,exp2))
    end

  | Minus(e1, e2)  ->
    let v1 = eval e1 env i (n+1) in
    let v2 = eval e2 env i (n+1) in
    begin
      match (v1, v2) with
      | (Error(ep), _ ) -> Error(ep)
      | ( _ , Error(ep)) -> Error(ep)
      | (Exp(exp1), Exp(exp2)) -> 
        if i = 0 then binop (-) v2 v1
        else
          Exp(Minus(exp1,exp2))
    end
  
  | Times(e1, e2)  ->
    let v1 = eval e1 env i (n+1) in
    let v2 = eval e2 env i (n+1) in
    begin
      match (v1, v2) with
      | (Error(ep), _ ) -> Error(ep)
      | ( _ , Error(ep)) -> Error(ep)
      | (Exp(exp1), Exp(exp2)) -> 
        if i = 0 then binop ( * ) v2 v1
        else
          Exp(Times(exp1,exp2))
    end
  
  | Div(e1, e2)  ->
    let v1 = eval e1 env i (n+1) in
    let v2 = eval e2 env i (n+1) in
    begin
      match (v1, v2) with
      | (Error(ep), _ ) -> Error(ep)
      | ( _ , Error(ep)) -> Error(ep)
      | (Exp(exp1), Exp(exp2)) -> 
        if i = 0 then 
        begin
          match v2 with
          | Exp(Const(IntConst(0))) -> failwith "Division_by_zero"
          | _ -> binop ( / ) v2 v1
        end
        else
          Exp(Div(exp1,exp2))
    end
  
  | If(e1, e2, e3)  ->
    let renamed_e1 = eval e1 env i (n+1) in
    begin
      match renamed_e1 with
      | Error(ep) -> Error(ep)
      | Exp(exp1) ->
        let env1 = (change_env_level env i 0) in
        let v1 = eval exp1 env1 0 (n+1) in
        begin
          match v1 with
          | Error(ep) -> Error(ep)
          | _ ->
            begin
              match v1 with
              | Exp(Const(BoolConst(true))) ->
                if i = 0 then eval e2 env i (n+1) 
                else
                  begin
                    match (eval e2 env i (n+1)) with
                    | Error(ep) -> Error(ep)
                    | Exp(_) -> Exp(IfVal(exp1,e2,e3,(unbox (eval e2 env i (n+1)))))(*要修正*)
                  end
              | Exp(Const(BoolConst(false))) ->
                if i = 0 then eval e3 env i (n+1)
                else
                  begin
                    match (eval e3 env i (n+1)) with
                    | Error(ep) -> Error(ep)
                    | Exp(_) -> Exp(IfVal(exp1,e2,e3,(unbox (eval e3 env i (n+1)))))(*要修正*)
                  end
              | _ -> Error(TypeError("need boolean in if"))
            end
        end
    end

  | IfVal(e1, e2, e3, e4)  -> Exp(IfVal(e1, e2, e3, e4))

  | Eq(e1, e2)  ->
    let v1 = eval e1 env i (n+1) in
    let v2 = eval e2 env i (n+1) in
    begin
      match (v1,v2) with
      | (Error(ep), _ ) -> Error(ep)
      | ( _ , Error(ep)) -> Error(ep)
      | (Exp(exp1), Exp(exp2)) ->
        if i = 0 then
          begin
            match (exp1,exp2) with
            | (Const(IntConst(n2)), Const(IntConst(n1))) -> Exp(Const(BoolConst(n1=n2)))
            | (Const(BoolConst(b2)), Const(BoolConst(b1))) -> Exp(Const(BoolConst(b1=b2)))
            | _ -> Exp(Const(BoolConst(false)))
          end
        else
          Exp(Eq(exp1, exp2))
    end
  
  | NotEq(e1, e2)  ->
    let v1 = eval e1 env i (n+1) in
    let v2 = eval e2 env i (n+1) in
    begin
      match (v1,v2) with
      | (Error(ep), _ ) -> Error(ep)
      | ( _ , Error(ep)) -> Error(ep)
      | (Exp(exp1), Exp(exp2)) ->
        if i = 0 then
          begin
            match (exp1,exp2) with
            | (Const(IntConst(n2)), Const(IntConst(n1))) -> Exp(Const(BoolConst(n1<>n2)))
            | (Const(BoolConst(b2)), Const(BoolConst(b1))) -> Exp(Const(BoolConst(b1<>b2)))
            | _ -> Exp(Const(BoolConst(true)))
          end
        else
          Exp(NotEq(exp1, exp2))
    end
  
  | Greater(e1, e2)  ->
    let v1 = eval e1 env i (n+1) in
    let v2 = eval e2 env i (n+1) in
    begin
      match (v1,v2) with
      | (Error(ep), _ ) -> Error(ep)
      | ( _ , Error(ep)) -> Error(ep)
      | (Exp(exp1), Exp(exp2)) ->
        if i = 0 then
          begin
            match (exp2,exp1) with
            | (Const(IntConst(n2)), Const(IntConst(n1))) -> Exp(Const(BoolConst(n1>n2)))
            | _ -> Error(TypeError("error in Greater"))
          end
       else
        Exp(Greater(exp1, exp2))
    end
  
  | Less(e1, e2)  ->
    let v1 = eval e1 env i (n+1) in
    let v2 = eval e2 env i (n+1) in
    begin
      match (v1,v2) with
      | (Error(ep), _ ) -> Error(ep)
      | ( _ , Error(ep)) -> Error(ep)
      | (Exp(exp1), Exp(exp2)) ->
        if i = 0 then
          begin
            match (exp2,exp1) with
            | (Const(IntConst(n2)), Const(IntConst(n1))) -> Exp(Const(BoolConst(n1<n2)))
            | _ -> Error(TypeError("error in Less"))
          end
       else
        Exp(Less(exp1, exp2))
    end

  | Let(x, tau, e1, e2)  ->
    let v1 = eval e1 env i (n+1) in
    begin
      match v1 with
      | Error(ep) -> Error(ep)
      | Exp(exp1) ->
        if i = 0 then
          let env1 = ext env x v1 i in 
          eval e2 env1 i (n+1)
        else
          let new_x = fresh_var x env in
          let env1 = ext (ext env new_x v1 i) x (Exp(Var(new_x))) i in
          let v2 = eval e2 env1 i (n+1) in
          begin
            match v2 with
            | Error(ep) -> Error(ep)
            | Exp(exp2) -> Exp(LetVal(new_x, tau, exp1, exp2))
          end
    end
  
  | LetVal(x, tau, e1, e2) -> Exp(LetVal(x, tau, e1, e2))

  | LetRec(f, tau1, x, tau2, e1, e2)  ->
    if i = 0 then
      let env1 = ext env f (Exp(RecFunVal(f, tau1, x, tau2, e1, env))) i in
      eval e2 env1 i (n+1)
    else
      let new_f = fresh_var f env in
      let new_f_env = ext env f (Exp(Var(new_f))) i in
      let new_x = fresh_var x new_f_env in
      let new_x_env = ext new_f_env x (Exp(Var(new_x))) i in
      let e1_rename_env1 = ext (ext new_x_env new_f (Exp(CodeVar(tau1))) i) new_x (Exp(CodeVar(tau2))) i in
      (*print_endline("Created e1_rename_env1 in LetRec: "^string_of_env e1_rename_env1);*)
      let v1 = eval e1 e1_rename_env1 i (n+1) in
      begin
        match v1 with
        | Error(ep) -> Error(ep)
        | Exp(exp1) ->
          let e2_rename_env1 = ext new_f_env new_f (Exp(RecFunVal(new_f, tau1, new_x, tau2, exp1, env))) i in
          (*print_endline("Created e2_rename_env1 in LetRec: "^string_of_env e2_rename_env1);*)
          let v2 = eval e2 e2_rename_env1 i (n+1) in
          begin
            match v2 with
            | Error(ep) -> Error(ep)
            | Exp(exp2) -> 
              let env1 = ext env new_f (Exp(RecFunVal(new_f, tau1, new_x, tau2, exp1, env))) i in
              (*print_endline("Created env1 in LetRec: "^string_of_env env1);*)
              let te = make_tenv env1 in
              (*print_endline("LetRecVal keeps this: "^string_of_tenv te);*)
              Exp(LetRecVal(new_f, tau1, new_x, tau2, exp1, exp2, te))
          end
      end      
  
  | RecFunVal(f, tau1, x, tau2, e1, env)  -> Exp(RecFunVal(f, tau1, x, tau2, e1, env))

  | IsType(tau, e1)  ->
    if i = 0 then
      if tau = TDynamic then Exp(Const(BoolConst(true)))
      else
        let v1 = eval e1 env i (n+1)in
        let te1 = make_tenv env in
        begin
          match unbox v1 with
          | Error(ep) -> Error(ep)
          | Exp(exp1) ->
            let t1 = tcheck "dynamic" te1 exp1 i in
            begin
              match t1 with
              | TDynamic -> Exp(Const(BoolConst(true)))
              | _ -> if (is_consist tau t1) then Exp(Const(BoolConst(true))) else Exp(Const(BoolConst(false)))
            end
        end
    else
      let v1 = eval e1 env i (n+1) in
      begin
        match v1 with
        | Error(ep) -> Error(ep)
        | Exp(exp1) -> Exp(IsType(tau, exp1))
      end


  | _ -> failwith "unknown expression in eval"
