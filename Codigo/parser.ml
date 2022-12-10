type token =
  | LAMBDA
  | TRUE
  | FALSE
  | IF
  | THEN
  | ELSE
  | SUCC
  | PRED
  | ISZERO
  | LET
  | LETREC
  | IN
  | BOOL
  | NAT
  | LPAREN
  | RPAREN
  | DOT
  | EQ
  | COLON
  | ARROW
  | EOF
  | STRING
  | CONCAT
  | END_PROCESSING
  | OPEN_TUPLE
  | CLOSE_TUPLE
  | COMA
  | INTV of (int)
  | STRINGV of (string)
  | STRINGVAL of (string)

open Parsing;;
let _ = parse_error;;
# 3 "parser.mly"
  open Lambda;;
# 38 "parser.ml"
let yytransl_const = [|
  257 (* LAMBDA *);
  258 (* TRUE *);
  259 (* FALSE *);
  260 (* IF *);
  261 (* THEN *);
  262 (* ELSE *);
  263 (* SUCC *);
  264 (* PRED *);
  265 (* ISZERO *);
  266 (* LET *);
  267 (* LETREC *);
  268 (* IN *);
  269 (* BOOL *);
  270 (* NAT *);
  271 (* LPAREN *);
  272 (* RPAREN *);
  273 (* DOT *);
  274 (* EQ *);
  275 (* COLON *);
  276 (* ARROW *);
    0 (* EOF *);
  277 (* STRING *);
  278 (* CONCAT *);
  279 (* END_PROCESSING *);
  280 (* OPEN_TUPLE *);
  281 (* CLOSE_TUPLE *);
  282 (* COMA *);
    0|]

let yytransl_block = [|
  283 (* INTV *);
  284 (* STRINGV *);
  285 (* STRINGVAL *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\002\000\002\000\002\000\002\000\002\000\002\000\
\003\000\003\000\003\000\003\000\003\000\003\000\005\000\005\000\
\005\000\005\000\005\000\005\000\004\000\004\000\006\000\006\000\
\006\000\006\000\000\000"

let yylen = "\002\000\
\005\000\003\000\001\000\006\000\006\000\006\000\008\000\005\000\
\001\000\002\000\002\000\002\000\003\000\002\000\003\000\001\000\
\001\000\001\000\001\000\001\000\001\000\003\000\003\000\001\000\
\001\000\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\016\000\017\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\019\000\000\000\
\020\000\027\000\000\000\000\000\009\000\000\000\018\000\000\000\
\010\000\011\000\012\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\014\000\000\000\000\000\000\000\000\000\015\000\
\013\000\000\000\000\000\002\000\024\000\025\000\000\000\026\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\008\000\001\000\023\000\
\005\000\022\000\004\000\006\000\000\000\000\000\007\000"

let yydgoto = "\002\000\
\018\000\019\000\020\000\049\000\021\000\050\000"

let yysindex = "\001\000\
\011\255\000\000\229\254\000\000\000\000\058\255\043\255\043\255\
\043\255\233\254\235\254\058\255\043\255\058\255\000\000\005\255\
\000\000\000\000\002\255\043\255\000\000\015\255\000\000\032\255\
\000\000\000\000\000\000\029\255\030\255\034\255\043\255\022\255\
\058\255\056\000\000\000\252\254\058\255\058\255\252\254\000\000\
\000\000\058\255\051\255\000\000\000\000\000\000\252\254\000\000\
\040\255\059\255\072\255\077\255\073\255\067\255\093\000\079\255\
\058\255\252\254\058\255\058\255\058\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\084\255\058\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\001\255\
\000\000\000\000\000\000\078\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\025\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\250\255\000\000\005\000\068\000\000\000"

let yytablesize = 104
let yytable = "\024\000\
\022\000\001\000\018\000\018\000\028\000\030\000\029\000\032\000\
\045\000\046\000\047\000\003\000\004\000\005\000\006\000\018\000\
\048\000\007\000\008\000\009\000\010\000\011\000\033\000\018\000\
\034\000\012\000\043\000\018\000\018\000\018\000\051\000\052\000\
\013\000\036\000\014\000\054\000\037\000\015\000\016\000\017\000\
\021\000\021\000\021\000\053\000\004\000\005\000\038\000\042\000\
\039\000\040\000\065\000\056\000\067\000\068\000\069\000\044\000\
\057\000\012\000\003\000\004\000\005\000\006\000\066\000\071\000\
\007\000\008\000\009\000\010\000\011\000\015\000\023\000\017\000\
\012\000\055\000\025\000\026\000\027\000\059\000\058\000\013\000\
\031\000\014\000\003\000\003\000\015\000\023\000\017\000\035\000\
\060\000\003\000\061\000\062\000\063\000\003\000\064\000\070\000\
\000\000\000\000\041\000\000\000\003\000\000\000\003\000\003\000"

let yycheck = "\006\000\
\028\001\001\000\002\001\003\001\028\001\012\000\028\001\014\000\
\013\001\014\001\015\001\001\001\002\001\003\001\004\001\015\001\
\021\001\007\001\008\001\009\001\010\001\011\001\018\001\023\001\
\023\001\015\001\033\000\027\001\028\001\029\001\037\000\038\000\
\022\001\019\001\024\001\042\000\005\001\027\001\028\001\029\001\
\016\001\017\001\018\001\039\000\002\001\003\001\018\001\026\001\
\019\001\016\001\057\000\047\000\059\000\060\000\061\000\000\000\
\017\001\015\001\001\001\002\001\003\001\004\001\058\000\070\000\
\007\001\008\001\009\001\010\001\011\001\027\001\028\001\029\001\
\015\001\023\001\007\000\008\000\009\000\006\001\020\001\022\001\
\013\000\024\001\005\001\006\001\027\001\028\001\029\001\020\000\
\012\001\012\001\018\001\025\001\000\000\016\001\016\001\012\001\
\255\255\255\255\031\000\255\255\023\001\255\255\025\001\026\001"

let yynames_const = "\
  LAMBDA\000\
  TRUE\000\
  FALSE\000\
  IF\000\
  THEN\000\
  ELSE\000\
  SUCC\000\
  PRED\000\
  ISZERO\000\
  LET\000\
  LETREC\000\
  IN\000\
  BOOL\000\
  NAT\000\
  LPAREN\000\
  RPAREN\000\
  DOT\000\
  EQ\000\
  COLON\000\
  ARROW\000\
  EOF\000\
  STRING\000\
  CONCAT\000\
  END_PROCESSING\000\
  OPEN_TUPLE\000\
  CLOSE_TUPLE\000\
  COMA\000\
  "

let yynames_block = "\
  INTV\000\
  STRINGV\000\
  STRINGVAL\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    Obj.repr(
# 49 "parser.mly"
      ( Bind (_1, _3) )
# 201 "parser.ml"
               : Lambda.comando))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    Obj.repr(
# 51 "parser.mly"
      ( Eval _1 )
# 208 "parser.ml"
               : Lambda.comando))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'appTerm) in
    Obj.repr(
# 55 "parser.mly"
      ( _1 )
# 215 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'term) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 57 "parser.mly"
      ( TmIf (_2, _4, _6) )
# 224 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'ty) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 59 "parser.mly"
      ( TmAbs (_2, _4, _6) )
# 233 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 61 "parser.mly"
      ( TmLetIn (_2, _4, _6) )
# 242 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : 'ty) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 63 "parser.mly"
      ( TmLetIn (_2, TmFix (TmAbs (_2, _4, _6)), _8) )
# 252 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'term) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    Obj.repr(
# 65 "parser.mly"
      ( TmTuple (_2, _4) )
# 260 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 69 "parser.mly"
      ( _1 )
# 267 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 71 "parser.mly"
      ( TmSucc _2 )
# 274 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 73 "parser.mly"
      ( TmPred _2 )
# 281 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 75 "parser.mly"
      ( TmIsZero _2 )
# 288 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'atomicTerm) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 77 "parser.mly"
      ( TmConcat (_2, _3) )
# 296 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'appTerm) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 79 "parser.mly"
      ( TmApp (_1, _2) )
# 304 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    Obj.repr(
# 83 "parser.mly"
      ( _2 )
# 311 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    Obj.repr(
# 85 "parser.mly"
      ( TmTrue )
# 317 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    Obj.repr(
# 87 "parser.mly"
      ( TmFalse )
# 323 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 89 "parser.mly"
      ( TmVar _1 )
# 330 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 91 "parser.mly"
      ( let rec f = function
            0 -> TmZero
          | n -> TmSucc (f (n-1))
        in f _1 )
# 340 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 96 "parser.mly"
      ( TmString (_1) )
# 347 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTy) in
    Obj.repr(
# 100 "parser.mly"
      ( _1 )
# 354 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'atomicTy) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'ty) in
    Obj.repr(
# 102 "parser.mly"
      ( TyArr (_1, _3) )
# 362 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'ty) in
    Obj.repr(
# 106 "parser.mly"
      ( _2 )
# 369 "parser.ml"
               : 'atomicTy))
; (fun __caml_parser_env ->
    Obj.repr(
# 108 "parser.mly"
      ( TyBool )
# 375 "parser.ml"
               : 'atomicTy))
; (fun __caml_parser_env ->
    Obj.repr(
# 110 "parser.mly"
      ( TyNat )
# 381 "parser.ml"
               : 'atomicTy))
; (fun __caml_parser_env ->
    Obj.repr(
# 112 "parser.mly"
      ( TyString )
# 387 "parser.ml"
               : 'atomicTy))
(* Entry s *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let s (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Lambda.comando)
