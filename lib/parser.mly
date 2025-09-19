// Parser on Staged_Gradual_TypeSystem

%{
open Syntax
%}

// リテラル
%token <string> VAR  // x, y, abc, ...
%token <int> INT     // 0, 1, 2, ...

//Staging
%token LBRACKET     // '.<'
%token RBRACKET     // '>.'
%token ESC      // '.~'

// 演算子
%token PLUS     // '+'
%token MINUS    // '-'
%token ASTERISK // '*'
%token SLASH    // '/'
%token EQUAL    // '='
%token LESS     // '<'
%token GREATER  // '>'
%token COMMA     // ','

// 括弧類
%token LPAREN   // '('
%token RPAREN   // ')'

%token FUNC     // 'Fun'

// 区切り記号
%token ARROW    // "->"
%token TARROW   // "-->"
%token VBAR     // "|"

//Gradual_Typing
%token COL      // ":"
%token TDYNAMIC // "?"
%token TINT     // "int"
%token TBOOL    // "bool"
%token TCODE    // "code"
%token CAST     // "Cast"
%token ISTYPE   // "is_type"
%token TCASE    // "Typecase"


// キーワード
%token TRUE     // "true"
%token FALSE    // "false"
%token FUN      // "fun"
%token LET      // "let"
%token REC      // "rec"
%token IN       // "in"
%token IF       // "if"
%token THEN     // "then"
%token ELSE     // "else"
%token SUCC     // "succ"
%token WITH     // "with"
%token END      // "end"

// 制御記号
%token EOF 

// 演算子優先順位 (優先度の低いものほど先)
%nonassoc IN ELSE ARROW
%right TARROW
%left EQUAL GREATER LESS
%left PLUS MINUS
%left ASTERISK SLASH
%nonassoc UNARY
%right TCODE
%right ESC
%right ISTYPE
%left VAR INT TRUE FALSE LBRACKET LPAREN FUNC CAST SUCC


%start main
%type <Syntax.exp> main
%type <Syntax.exp> exp
%type <Syntax.exp> arg_exp
%type <Syntax.atom> atom
%type <Syntax.tau> tau


%%

// 開始記号
main:
  | exp EOF
    { $1 }
;

//型
atom:
  | TINT
    { TInt }
  | TBOOL
    { TBool }
tau:
  | atom
    { TAtom ($1) }
  | TDYNAMIC
    { TDynamic }
  | tau TARROW tau
    { TArrow ($1, $3) }
  | tau TCODE
    { TCode ($1)}
  | LPAREN tau RPAREN
    { $2 }
;

// 関数の引数になれる式
arg_exp:
  | INT
    { Const (IntConst ($1)) }
  
  | TRUE
    { Const (BoolConst (true)) }
  
  | FALSE
    { Const (BoolConst (false)) }
  
  | FUNC LPAREN VAR RPAREN
    { Const (FunConst ($3))}
  
  | SUCC
    { Const (FunConst("succ")) }

  | ISTYPE LPAREN tau COMMA exp RPAREN
    { IsType ($3, $5) }

  | CAST LPAREN tau COMMA exp RPAREN
    { Cast ($3, $5) }

  | VAR
    { Var $1 }
  
  // Brackets
  | LBRACKET exp RBRACKET
    { Brackets $2}
    
  // 括弧で囲まれた式
  | LPAREN exp RPAREN
    { $2 }
;


tcases_rev:
  | tau ARROW exp
    { [($1, $3)] }

  | VBAR tau ARROW exp
    { [($2, $4)] }

  | tcases_rev VBAR tau ARROW exp
    { ($3, $5) :: $1 }
;


// 式
exp:
  | arg_exp
    { $1 }
    
  // 関数適用 (e1 e2)
  | exp arg_exp
    { App ($1, $2) }
  
  // 符号の反転 -e
  | MINUS exp %prec UNARY
    { Minus (Const(IntConst(0)) , $2) }
  
  // e1 + e2
  | exp PLUS exp
    { Plus ($1, $3) }
  
  // e1 - e2
  | exp MINUS exp
    { Minus ($1, $3) }
  
  // e1 * e2
  | exp ASTERISK exp
    { Times ($1, $3) }
  
  // e1 / e2
  | exp SLASH exp
    { Div ($1, $3) }
    
  // e1 = e2
  | exp EQUAL exp
    { Eq ($1, $3) }
  
  // e1 < e2
  | exp LESS exp
    { Less ($1, $3) }
    
  // e1 > e2
  | exp GREATER exp
    { Greater ($1, $3) }

  // fun x : tau -> e1
  | FUN VAR COL tau ARROW exp
    { Fun ($2, $4, $6)}

  // fun x -> e
  | FUN VAR ARROW exp
    { Fun ($2, TDynamic, $4) }

  // let x = e1 in e2
  | LET VAR EQUAL exp IN exp
    { Let ($2, TDynamic, $4, $6) }
  
  // let x :tau = e1 in e2
  | LET VAR COL tau EQUAL exp IN exp
    { Let ($2, $4, $6, $8) }
  
  // let rec f x = e1 in e2
  | LET REC VAR VAR EQUAL exp IN exp
    { LetRec ($3, TDynamic, $4, TDynamic, $6, $8) }

  // let rec f x :tau = e1 in e2
  | LET REC VAR VAR COL tau EQUAL exp IN exp
    { LetRec ($3, TDynamic, $4, $6, $8, $10) }

  // let rec f :tau x = e1 in e2
  | LET REC VAR COL tau VAR EQUAL exp IN exp
    { LetRec ($3, $5, $6, TDynamic, $8, $10) }

  // let rec f :tau x :tau = e1 in e2
  | LET REC VAR COL tau VAR COL tau EQUAL exp IN exp
    { LetRec ($3, $5, $6, $8, $10, $12) }
  
  // if e1 then e2 else e3
  | IF exp THEN exp ELSE exp
    { If ($2, $4, $6) }
  
  | ESC  exp
    { Escape ($2) }
  
  | TCASE exp WITH tcases_rev END
    { TCase ($2, List.rev $4) }

  | error
    { 
      let message =
        Printf.sprintf 
          "parse error near characters %d-%d"
          (Parsing.symbol_start ())
	        (Parsing.symbol_end ())
	    in
	    failwith message
	  }
;
