
%{
  open Lambda;;
%}

/* Han sido marcadas las funcionalidades que se han añadido al analizador sintáctico */

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
%token LETREC   /* 2.1- Combinador de punto fijo interno.  */
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

%token STRING   /* 2.3- Tipo String.  */
%token CONCAT   /* 2.3- Tipo String.  */

%token END_PROCESSING   /* 1.1- Expresiones multilínea */
%token OPEN_TUPLE       /* 2.5- Tipo Tupla */
%token CLOSE_TUPLE      /* 2.5- Tipo Tupla */
%token COMA             /* 2.5- Tipo Tupla */
%token X                /* Producto cartesiano */

%token <int> INTV
%token <string> STRINGV

%token <string> STRINGVAL

%start s
%type <Lambda.comando> s

%%


 
s : 
    STRINGV EQ term END_PROCESSING EOF    /*| Para la funcionalidad de contexto, se detecta el formato "[nombre] = [termino];;"  */
       { Bind ($1, $3) }                  /*| y se llama a la función correspondiente para hacer la asociación.                  */
  | term END_PROCESSING EOF
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
  | LETREC STRINGV COLON ty EQ term IN term               /* Para la recursividad, se detecta el formato "letrec [nombre]. [tipos] = [termino] in [termino]"  */
      { TmLetIn ($2, TmFix (TmAbs ($2, $4, $6)), $8) }    
  | OPEN_TUPLE term COMA term CLOSE_TUPLE                 /* Pattern-matching para tipo tupla, con formato "{ [termino], [termino] }"                         */
      { TmTuple ($2, $4) }
  | OPEN_TUPLE term COMA term CLOSE_TUPLE DOT INTV        /* Pattern-matching para tipo tupla, con formato "{ [termino], [termino] }.[int]"                   */
      { TmTupleProj (TmTuple ($2, $4), $7) }
      
appTerm :
    atomicTerm
      { $1 }
  | SUCC atomicTerm
      { TmSucc $2 }
  | PRED atomicTerm
      { TmPred $2 }
  | ISZERO atomicTerm
      { TmIsZero $2 }
  | CONCAT atomicTerm atomicTerm    /* Para la concatenación de strings, se detecta el formato "concat [termino atómico] [termino atómico]" */
      { TmConcat ($2, $3) }
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
  | STRINGVAL                       /* Incorporado para el tipo String. Pasa el valor como TmString*/
      { TmString ($1) }

ty :
    atomicTy
      { $1 }
  | atomicTy ARROW ty
      { TyArr ($1, $3) }
  | atomicTy X atomicTy             /* Tipo implementado para producto cartesiano. Detecta el formato "[tipo] x [tipo]" */
      { TyCartesian ($1, $3) }

atomicTy :
    LPAREN ty RPAREN  
      { $2 } 
  | BOOL
      { TyBool }
  | NAT
      { TyNat }
  | STRING
      { TyString }

