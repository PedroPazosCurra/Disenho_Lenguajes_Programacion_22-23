
%{
  open Lambda;;
%}

/* The functionalities that have been added to the parser have been pointed out. */

%token LAMBDA
%token TRUE
%token FALSE
%token IF
%token THEN
%token ELSE
%token SUCC
%token PRED
%token ISZERO
%token LET
%token LETREC   /* 2.1- Internal fix point combinator.  */
%token IN
%token BOOL
%token NAT

%token LPAREN
%token RPAREN
%token DOT
%token EQ
%token COLON
%token ARROW
%token EOF

%token STRING                /* 2.3- String type.  */
%token CONCAT                /* 2.3- String type.  */
%token UNIT                  /* 2.9- Unit type     */
%token UNITVAL               /* 2.9- Unit type     */
%token END_PROCESSING        /* 1.1- Multiline expressions */
%token OPEN_BRACKET          /* 2.5- Tuple type    */
%token CLOSE_BRACKET         /* 2.5- Tuple type    */
%token COMA                  /* 2.5- Tuple type    */
%token X                     /* Cartesian product  */
%token PRINT_NAT             /* 2.10- IO operations*/
%token PRINT_STRING          /* 2.10- IO operations*/
%token PRINT_NEWLINE         /* 2.10- IO operations*/
%token READ_NAT              /* 2.10- IO operations*/
%token READ_STRING           /* 2.10- IO operations*/
%token SEMICOLON

%token <int> INTV
%token <string> STRINGV

%token <string> STRINGVAL    /* 2.3- String type.  */

%start s
%type <Lambda.comando> s

%%


 
s : 
    STRINGV EQ term END_PROCESSING        /*| For the context functionality, it's detected the format "[name] = [term];;"  */
       { Bind ($1, $3) }                  /*| and the corresponding function is called to make the association.            */
  | term END_PROCESSING
      { Eval $1 }

term :
    appTerm
      { $1 }
  | IF term THEN term ELSE term
      { TmIf ($2, $4, $6) }
  | LAMBDA STRINGV COLON ty DOT term
      { TmAbs ($2, $4, $6) }
  | LET STRINGV EQ term IN term
      { TmLetIn ($2, $4, $6) }
  | LETREC STRINGV COLON ty EQ term IN term               /* For recursivity, it's detected the format "letrec [name]. [types] = [term] in [term]"   */
      { TmLetIn ($2, TmFix (TmAbs ($2, $4, $6)), $8) }    
      
appTerm :
    atomicTerm
      { $1 }
  | SUCC atomicTerm
      { TmSucc $2 }
  | PRED atomicTerm
      { TmPred $2 }
  | ISZERO atomicTerm
      { TmIsZero $2 }
  | CONCAT atomicTerm atomicTerm    /* For strings concatenation, it's detected the format "concat [atomic term] [atomic term]" */
      { TmConcat ($2, $3) }
  | PRINT_NAT atomicTerm
      { TmPrintNat $2 }
  | PRINT_STRING atomicTerm
      { TmPrintString $2 }
  | PRINT_NEWLINE atomicTerm
      { TmPrintNewline $2 }
  | READ_NAT atomicTerm
      { Tm ReadNat $2 }
  | READ_STRING atomicTerm
      { Tm ReadString $2 }
  | appTerm atomicTerm
      { TmApp ($1, $2) }

atomicTerm :
    LPAREN term RPAREN
      { $2 }
  | TRUE
      { TmTrue }
  | FALSE
      { TmFalse }
  | STRINGV
      { TmVar $1 }
  | INTV
      { let rec f = function
            0 -> TmZero
          | n -> TmSucc (f (n-1))
        in f $1 }
  | STRINGVAL                       /* Built-in for String type. Passes the value as TmString */
      { TmString ($1) }
  | UNITVAL                         /* Built-in for Unit type. */
      { TmUnit }
  | OPEN_BRACKET tupleFields CLOSE_BRACKET  /* Pattern-matching for tuple type, with format "{ [term], [term] }"                         */
      { TmTuple $2 }
  | OPEN_BRACKET tupleFields CLOSE_BRACKET DOT INTV
      { TmTupleProj (TmTuple $2, $5) }

tupleFields:
    term 
        { [$1] }
  | term COMA tupleFields
        { $1 :: $3 }


ty :
    atomicTy
      { $1 }
  | atomicTy ARROW ty
      { TyArr ($1, $3) }

atomicTy :
    LPAREN ty RPAREN  
      { $2 } 
  | BOOL
      { TyBool }
  | NAT
      { TyNat }
  | STRING
      { TyString }
  | UNIT                            /* Built-in for Unit type. */
      { TyUnit }
  | OPEN_BRACKET tupleTypes CLOSE_BRACKET           /* Built-in type for cartesian product. Detects the format "[type] x [type]" */
      { TyTuple ($2) }
      
tupleTypes:
    atomicTy
    { [$1] }
  | atomicTy COMA tupleTypes
    { $1 :: $3 }
