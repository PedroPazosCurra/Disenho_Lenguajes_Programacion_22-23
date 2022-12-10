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

val s :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Lambda.comando
