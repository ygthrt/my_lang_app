(*syntax.ml*)
(*Syntax on Staged_Gradual_TypeSystem*)

(* Level *)
type level = int

(*Type*)
type atom =
| TInt
| TBool

type tau = 
| TAtom of atom
| TDynamic
| TArrow of tau * tau
| TCode of tau


(*Expression*)
type const =
| IntConst of int
| BoolConst of bool
| FunConst of string    (* functional constant e.g. succ *)

type exp =
| Const of const              (* Constants e.g. 1, true, succ*)
| Var of string               (* variable e.g. x, y*)
| Fun of string * tau * exp   (* lambda abstraction e.g fun x:int --> e*)
| App of exp * exp            (* function application i.e. e e *)
| Brackets of exp             (* brackets e.g. .<e>.*)
| Escape of exp               (* escape e.g. .~e*)

| Cast of tau * exp           (* cast (Programmer cannot use.) e.g Cast(int e) *)

| Plus of exp * exp     (* e + e *)
| Minus of exp * exp    (* e - e *)
| Times of exp * exp    (* e * e *)
| Div of exp * exp      (* e / e *)

| If of exp * exp * exp (* if e then e else e *)
| Eq of exp * exp       (* e = e *)
| NotEq of exp * exp       (* e <> e *)
| Greater of exp * exp  (* e > e *)
| Less of exp * exp     (* e < e *)

| Let of string * tau * exp * exp   (* let x :tau = e in e *)
| LetRec of string * tau * string * tau * exp * exp   (* let　rec f :tau x :tau = e in e *)

| IsType of tau * exp   (* is_type(tau, e) *)

| FunVal of string * tau * exp * env
| RecFunVal of string * tau * string * tau * exp * env

| CodeVal of exp * tyenv
| CodeVar of tau

| IfVal of exp * exp * exp * result
| LetVal of string * tau * exp * exp
| LetRecVal of string * tau * string * tau * exp * exp * tyenv

| TCase of exp * ((tau * exp) list)

| Unuse  (*avoidance for Warning*)


and error =
| CastError
| TypeError of string
| KillError

(*Result*)
and result =
| Exp of exp  (* Value *)
| Error of error

and
env = (string * result * level) list

and
tyenv = (string * tau * level) list