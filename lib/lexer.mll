(*lexer.ml*)
(*  Lexer on Staged_Gradual_TypeSystem *)

{
open Parser

let comment_depth = ref 0
}

let space = [' ' '\t' '\n' '\r']
let digit = ['0'-'9']
let alpha = ['A'-'Z' 'a'-'z' '_']
let alnum = digit | alpha | '\''

rule token = parse
  | "(*"
    { comment_depth := 1;
      comment lexbuf
    }
  (* 整数定数 *)
  | digit+
    { let str = Lexing.lexeme lexbuf in
      INT (int_of_string str) }

  (*Staging*)
  | ".<"      { LBRACKET }
  | ">."      { RBRACKET }
  | ".~"      { ESC }

  (* 演算子 *)
  | '+'       { PLUS }
  | '-'       { MINUS }
  | '*'       { ASTERISK }
  | '/'       { SLASH }
  | '='       { EQUAL }
  | '<'       { LESS }
  | '>'       { GREATER }
  | ','       { COMMA } (*Castで使用予定、他で使うなら演算子優先順位に要調整*)

  (* 括弧類 *)
  | '('       { LPAREN }
  | ')'       { RPAREN }
  
  (*
  | '['       { LBRA }
  | ']'       { RBRA }
  *)

  (* 区切り記号 *)
  | "-->"     { TARROW}
  | "->"      { ARROW }
  | '|'       { VBAR }
  | ";;"      { DSEMI }
  | ';'       { SEMI }

  (*Gradual_Typing*)
  | ":"       { COL }
  | "?"       { TDYNAMIC }
  | "int"     { TINT }
  | "bool"    { TBOOL }
  | "code"    { TCODE }
  | "Cast"    { CAST }
  | "is_type"  { ISTYPE }
  | "Typecase" { TCASE }

  (* キーワード *)
  | "true"    { TRUE }
  | "false"   { FALSE }
  | "fun"     { FUN }
  | "Fun"     { FUNC }
  | "let"     { LET }
  | "rec"     { REC }
  | "in"      { IN }
  | "if"      { IF }
  | "then"    { THEN }
  | "else"    { ELSE }
  | "succ"    { SUCC }
  | "with"    { WITH }
  | "end"      { END }
  (*
  | "match"   { MATCH }
  | "List.hd" { HEAD }
  | "List.tl" { TAIL }
  *)

  (* 変数 *)
  | alpha alnum*
    { VAR (Lexing.lexeme lexbuf) }
  
  (* 制御記号 *)
  | eof       { EOF }

  (* スペースを読み飛ばす *)
  | space+    { token lexbuf }

  | _
    {
      let message = Printf.sprintf
        "unknown token %s near characters %d-%d"
        (Lexing.lexeme lexbuf)
        (Lexing.lexeme_start lexbuf)
        (Lexing.lexeme_end lexbuf)
      in
      failwith message
    }
and comment = parse
  | "*)"
    { decr comment_depth;
      if !comment_depth = 0 then
        token lexbuf
      else
        comment lexbuf
    }
  | "(*"
    { incr comment_depth;
      comment lexbuf
    }
  | eof
    { failwith "Unterminated comment" }
  | _
    { comment lexbuf }