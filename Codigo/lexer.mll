
{
  open Parser;;
  exception Lexical_error;; 
}

rule token = parse
    [' ' '\t' '\r' '\n']*  { token lexbuf }
  | "lambda"       { LAMBDA }
  | "L"            { LAMBDA }
  | "true"         { TRUE }
  | "false"        { FALSE }
  | "if"           { IF }
  | "then"         { THEN }
  | "else"         { ELSE }
  | "succ"         { SUCC }
  | "pred"         { PRED }
  | "iszero"       { ISZERO }
  | "let"          { LET }
  | "letrec"       { LETREC }
  | "in"           { IN }
  | "Bool"         { BOOL }
  | "Nat"          { NAT }
  | "concat"       { CONCAT }
  | "String"       { STRING }
  | "Unit"         { UNIT }
  | "unit"         { UNITVAL }
  | "X"            { X }
  | "print_nat"    { PRINT_NAT }
  | "print_string" { PRINT_STRING }
  | "print_newline"{ PRINT_NEWLINE }
  | "read_nat"     { READ_NAT }
  | "read_string"  { READ_STRING }
  | '('            { LPAREN }
  | ')'            { RPAREN }
  | ','            { COMA }
  | ';'            { SEMICOLON }
  | ";;"           { END_PROCESSING }
  | '{'            { OPEN_BRACKET }
  | '}'            { CLOSE_BRACKET }
  | '.'            { DOT }
  | '='            { EQ }
  | ':'            { COLON }
  | "->"           { ARROW }
  | '"'['A'-'Z''a'-'z''0'-'9' ' ']*'"' as string     { STRINGVAL (String.sub string 1 (String.index_from string 1 '"' - 1)) }
  | ['0'-'9']+  { INTV (int_of_string (Lexing.lexeme lexbuf)) }
  | ['a'-'z']['a'-'z' '_' '0'-'9']*
                { STRINGV (Lexing.lexeme lexbuf) }
  | eof         { raise End_of_file }
  | _           { raise Lexical_error } 
