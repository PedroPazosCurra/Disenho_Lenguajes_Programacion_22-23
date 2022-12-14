
(* TYPE DEFINITIONS *)

type ty =
    TyBool
  | TyNat
  | TyArr of ty * ty
  | TyString
  | TyTuple of ty list          (* Added for Tuple type (2.5) *)
  | TyUnit                      (* Added for Unit type (2.9)  *)
;;

type 'a context =               (* Added for global context (2.2). A context is a list of "string * [anything]" items. *)
  (string * 'a) list
;;

type term =
    TmTrue
  | TmFalse
  | TmIf of term * term * term
  | TmZero
  | TmSucc of term
  | TmPred of term
  | TmIsZero of term
  | TmVar of string
  | TmAbs of string * ty * term
  | TmApp of term * term
  | TmLetIn of string * term * term
  | TmFix of term                       (* Added for fix point combinator (2.1) *)
  | TmString of string	                (* Added for String type (2.3)          *)
  | TmConcat of term * term             (* Added for String type (2.3)          *)
  | TmTuple of term list                (* Added for Tuple type (2.5)           *)
  | TmTupleProj of term * int           (* Added for Tuple type (2.5)           *)
  | TmUnit                              (* Added for Unit type (2.9)            *)
  | TmPrintNat of term                  (* Added for I/O operations (2.10)      *)
  | TmPrintString of term               (* Added for I/O operations (2.10)      *)
  | TmPrintNewline of term              (* Added for I/O operations (2.10)      *)
  | TmReadNat of term                   (* Added for I/O operations (2.10)      *)
  | TmReadString of term                (* Added for I/O operations (2.10)      *)
;;

type comando =                          (* Added for global context (2.2)                          *)
    Eval of term                        (*  Given the input from the command line, the whole input *)
  | Bind of string * term               (*  it should be evaluated or binded                       *)


(* CONTEXT MANAGEMENT *)

let emptyctx =      (* Empty context -> empty list *)
  []
;;

let addbinding ctx x bind = (* addbinding: "Assign 'x' to 'bind' in 'ctx' context" *)
  (x, bind) :: ctx
;;

let getbinding ctx x =      (* getbinding: "Search the bound term on 'x' free variable in 'ctx' context" *)
  List.assoc x ctx
;;

(* Auxiliar function for tuple representation*)
let string_of_tuple l =
  let rec aux l1 str = match l1 with 
    [] -> str ^ " }"
    | h::[] -> aux [] (str ^ h)
    | h::t -> aux t (str ^ h ^ ", ")
  in aux l "{ "
  
(* TYPE MANAGEMENT (TYPING) *)

(* "String_of_ty" method returns the string value of certain type. *)
let rec string_of_ty ty = match ty with
    TyBool ->
      "Bool"
  | TyNat ->
      "Nat"
  | TyArr (ty1, ty2) ->
      "(" ^ string_of_ty ty1 ^ ")" ^ " -> " ^ "(" ^ string_of_ty ty2 ^ ")"
  | TyString ->
      "String"    
  | TyTuple types ->                                        (* Added for cartesian: passed to string as "([type] X [type])" *)
      let list = List.map (fun t -> string_of_ty t) types
      in string_of_tuple list 
  | TyUnit ->                                               (* Added for unit: passed to string as "Unit" *)
      "Unit"
;;

exception Type_error of string
;;


(* "Typeof" method returns the type of certain term. *)
let rec typeof ctx tm = match tm with
    (* T-True *)
    TmTrue ->
      TyBool

    (* T-False *)
  | TmFalse ->
      TyBool

    (* T-If *)
  | TmIf (t1, t2, t3) ->
      if typeof ctx t1 = TyBool then
        let tyT2 = typeof ctx t2 in
        if typeof ctx t3 = tyT2 then tyT2
        else raise (Type_error "arms of conditional have different types")
      else
        raise (Type_error "guard of conditional not a boolean")
      
    (* T-Zero *)
  | TmZero ->
      TyNat

    (* T-Succ *)
  | TmSucc t1 ->
      if typeof ctx t1 = TyNat then TyNat
      else raise (Type_error "argument of succ is not a number")

    (* T-Pred *)
  | TmPred t1 ->
      if typeof ctx t1 = TyNat then TyNat
      else raise (Type_error "argument of pred is not a number")

    (* T-Iszero *)
  | TmIsZero t1 ->
      if typeof ctx t1 = TyNat then TyBool
      else raise (Type_error "argument of iszero is not a number")

    (* T-Var *)
  | TmVar x ->
      (try getbinding ctx x with
       _ -> raise (Type_error ("no binding type for variable " ^ x)))

    (* T-Abs *)
  | TmAbs (x, tyT1, t2) ->
      let ctx' = addbinding ctx x tyT1 in
      let tyT2 = typeof ctx' t2 in
      TyArr (tyT1, tyT2)

    (* T-App *)
  | TmApp (t1, t2) ->
      let tyT1 = typeof ctx t1 in
      let tyT2 = typeof ctx t2 in
      (match tyT1 with
           TyArr (tyT11, tyT12) ->
             if tyT2 = tyT11 then tyT12
             else raise (Type_error "Parameter type mismatch")
         | _ -> raise (Type_error "Arrow type expected"))

    (* T-Let *)
  | TmLetIn (x, t1, t2) ->
      let tyT1 = typeof ctx t1 in
      let ctx' = addbinding ctx x tyT1 in
      typeof ctx' t2
      
      (* T-Fix *)
  | TmFix t1 ->                                 (* Addition for recursion: a Fix term comes from the "letrec" command.                            *)
        let tyT1 = typeof ctx t1 in             (*  In this concrete match, the type of the Fix term is the specified type on the letrec parameter*)
        (match tyT1 with
            TyArr (tyT11, tyT12) ->
                if tyT11 = tyT12 then tyT12
                else raise (Type_error "Body result is not compatible with domain")
            | _ -> raise (Type_error "Arrow type expected")
         )

    (*T-String*)
  | TmString _ ->     (* Addition for string type *)
     TyString
  
    (*T-Concat*)
  | TmConcat (t1, t2) ->            (* Functionality added for string type:                          *)
      let tyT1 = typeof ctx t1 in   (*  takes both terms and type is string only if both are strings *)
      let tyT2 = typeof ctx t2 in
      (match (tyT1, tyT2) with 
          (TyString, TyString) -> TyString
        | _ -> raise (Type_error "arguments of concat are not strings") 
      )

    (*T-Tuple*)
  | TmTuple terms ->             (* Addition for tuple type:  tuple type is a cartesian product of both items *)
      TyTuple (List.map (function t -> typeof ctx t) terms)
  
    (*T-Proj*)
  | TmTupleProj (t, pos) -> (match typeof ctx t with 
      
      TyTuple terms -> (try List.nth terms (pos - 1) with
          _ -> raise (Type_error "Invalid position")
      )
      | _ -> raise (Type_error "Invalid tuple")
  )

   (*T-Unit*)
  | TmUnit ->           (* Addition for Unit type *)
      TyUnit
  
  (* print_nat *)
  | TmPrintNat t ->
        if typeof ctx t = TyNat then TyUnit
        else raise (Type_error "argument of print_nat is not a natural number")
  
  (* print_string *)
  | TmPrintString t ->
        if typeof ctx t = TyString then TyUnit
        else raise (Type_error "argument of print_string is not a string")
    (* print_newline *)
  | TmPrintNewline t ->
      if typeof ctx t = TyUnit then TyUnit
      else raise (Type_error "argument of print_newline is not a unit")
  
    (* read_nat *)
  | TmReadNat t ->
      if typeof ctx t = TyUnit then TyNat
      else raise (Type_error "argument of read_nat is not a unit")
  
    (* read_string *)
  | TmReadString t ->
      if typeof ctx t = TyUnit then TyString
      else raise (Type_error "argument of read_string is not a unit")
;;


(* TERMS MANAGEMENT (EVALUATION) *)

(* "string_of_term" method returns the corresponding string of certain term. *)
let rec string_of_term = function
    TmTrue ->
      "true"
  | TmFalse ->
      "false"
  | TmIf (t1,t2,t3) ->
      "if " ^ "(" ^ string_of_term t1 ^ ")" ^
      " then " ^ "(" ^ string_of_term t2 ^ ")" ^
      " else " ^ "(" ^ string_of_term t3 ^ ")"
  | TmZero ->
      "0"
  | TmSucc t ->
     let rec f n t' = match t' with
          TmZero -> string_of_int n
        | TmSucc s -> f (n+1) s
        | _ -> "succ " ^ "(" ^ string_of_term t ^ ")"
      in f 1 t
  | TmPred t ->
      "pred " ^ "(" ^ string_of_term t ^ ")"
  | TmIsZero t ->
      "iszero " ^ "(" ^ string_of_term t ^ ")"
  | TmVar s ->
      s
  | TmAbs (s, tyS, t) ->
      "(lambda " ^ s ^ ":" ^ string_of_ty tyS ^ ". " ^ string_of_term t ^ ")"
  | TmApp (t1, t2) ->
      "(" ^ string_of_term t1 ^ " " ^ string_of_term t2 ^ ")"
  | TmLetIn (s, t1, t2) ->
      "let " ^ s ^ " = " ^ string_of_term t1 ^ " in " ^ string_of_term t2
  | TmFix termino ->                                                            (* Added for fix point combinator: returns "(fix [string_of_inner_term])" *)
      "(fix " ^ string_of_term termino ^ ")"                                   
  | TmString s ->                                                               (* Added for string type: returns the own string value                    *)
      s 
  | TmConcat (t1, t2) ->                                                        (* Added for string type: string a concat returns                         *)
       string_of_term t1 ^ string_of_term t2                                    (* the string of both inner terms (ideally string)                        *)
  | TmTuple terms ->                                                            (* Added for tuple type: corresponding string is:                         *)
      let list = List.map (fun t -> string_of_term t) terms                     (*  "{ item1_string, item2_string }"                                      *)
      in string_of_tuple list                                                   
  | TmTupleProj (terms, pos) ->    
      string_of_term terms ^ "." ^ string_of_int pos 
  | TmUnit ->                                                                   (* Added for unit type: Unit terms string are "unit"                      *)
      "unit"
  | TmPrintNat t  ->
      "print_nat(" ^ string_of_term t ^ ")"
  | TmPrintString t ->
      "print_string(" ^ string_of_term t ^ ")"
  | TmPrintNewline t ->
      "print_newline(" ^ string_of_term t ^ ")"
  | TmReadNat t ->
      "read_nat(" ^ string_of_term t ^ ")"
  | TmReadString t ->
      "read_string(" ^ string_of_term t ^ ")"
;;

let rec ldif l1 l2 = match l1 with
    [] -> []
  | h::t -> if List.mem h l2 then ldif t l2 else h::(ldif t l2)
;;

let rec lunion l1 l2 = match l1 with
    [] -> l2
  | h::t -> if List.mem h l2 then lunion t l2 else h::(lunion t l2)
;;


(* "free_vars" method searchs recursively the irreducible terms of an expression and clears their memory by assigning them to an empty item *)
let rec free_vars tm = match tm with
    TmTrue ->
      []
  | TmFalse ->
      []
  | TmIf (t1, t2, t3) ->
      lunion (lunion (free_vars t1) (free_vars t2)) (free_vars t3)
  | TmZero ->
      []
  | TmSucc t ->
      free_vars t
  | TmPred t ->
      free_vars t
  | TmIsZero t ->
      free_vars t
  | TmVar s ->
      [s]
  | TmAbs (s, _, t) ->
      ldif (free_vars t) [s]
  | TmApp (t1, t2) ->
      lunion (free_vars t1) (free_vars t2)
  | TmLetIn (s, t1, t2) ->
      lunion (ldif (free_vars t2) [s]) (free_vars t1)
  | TmFix termino ->
      free_vars termino
  | TmString _ ->
    []
  | TmConcat (t1, t2) ->
      lunion (free_vars t1) (free_vars t2)
  | TmTuple terms ->
      List.fold_left (fun freeVars term -> lunion (free_vars term) freeVars) [] terms
  | TmTupleProj (term, pos) ->
      free_vars term
  | TmUnit ->
      []
  | TmPrintNat term  ->
      free_vars term
  | TmPrintString term ->
      free_vars term
  | TmPrintNewline term ->
      free_vars term
  | TmReadNat term ->
      free_vars term
  | TmReadString term ->
      free_vars term
;;


(* "fresh_name" method searches a free_variable name that is not currently assigned in the global context. *)
let rec fresh_name x l =
  if not (List.mem x l) then x else fresh_name (x ^ "'") l
;;


(* "subst" method performs lambda substitution. *)
(*						                        *)
(* On abstractions, substitution works like	    *)
(*	        (λx. y)z  ==  y[x ← z]  		    *)
(*						                        *)
let rec subst x s tm = match tm with
    TmTrue ->
      TmTrue
  | TmFalse ->
      TmFalse
  | TmIf (t1, t2, t3) ->
      TmIf (subst x s t1, subst x s t2, subst x s t3)
  | TmZero ->
      TmZero
  | TmSucc t ->
      TmSucc (subst x s t)
  | TmPred t ->
      TmPred (subst x s t)
  | TmIsZero t ->
      TmIsZero (subst x s t)
  | TmVar y ->
      if y = x then s else tm
  | TmAbs (y, tyY, t) -> 
      if y = x then tm
      else let fvs = free_vars s in
           if not (List.mem y fvs)
           then TmAbs (y, tyY, subst x s t)
           else let z = fresh_name y (free_vars t @ fvs) in
                TmAbs (z, tyY, subst x s (subst y (TmVar z) t))  
  | TmApp (t1, t2) ->
      TmApp (subst x s t1, subst x s t2)
  | TmLetIn (y, t1, t2) ->
      if y = x then TmLetIn (y, subst x s t1, t2)
      else let fvs = free_vars s in
           if not (List.mem y fvs)
           then TmLetIn (y, subst x s t1, subst x s t2)
           else let z = fresh_name y (free_vars t2 @ fvs) in
                TmLetIn (z, subst x s t1, subst x s (subst y (TmVar z) t2))
  | TmFix t ->						                    (* Addition for Fix term (2.2) 		*)
       TmFix (subst x s t)				                (*| Addition for String type terms (2.3)*)
  | TmString t -> TmString t				            (*| 				  	*)
  | TmConcat (t1, t2) ->
      TmConcat(subst x s t1, subst x s t2)
  | TmTuple terms ->				                    (*| Addition for Tuple type terms (2.5)	*)
      TmTuple (List.map (fun t -> subst x s t) terms)   (*|                 					*)
  | TmTupleProj(t, pos) ->				                (*|					                    *)
      TmTupleProj(subst x s t, pos)                     (*|				                    	*)
  | TmUnit ->                                           (*|  Addition for Unit type terms (2.9) *)
      TmUnit                                            (*|                                     *)
  | TmPrintNat t  ->
      TmPrintNat (subst x s t)
  | TmPrintString t ->
      TmPrintString (subst x s t)
  | TmPrintNewline t ->
      TmPrintNewline (subst x s t)
  | TmReadNat t ->
      TmReadNat (subst x s t)
  | TmReadString t ->
      TmReadString (subst x s t)
;;


(* "isnumericval" method*)
let rec isnumericval tm = match tm with
    TmZero -> true
  | TmSucc t -> isnumericval t
  | _ -> false
;;


(* "isval" method checks if certain term has a value assigned. *)
let rec isval tm = match tm with
    TmTrue  -> true
  | TmFalse -> true
  | TmAbs _ -> true
  | TmString _ -> true
  | TmTuple terms-> List.for_all (fun t -> isval t) terms
  | TmUnit -> true
  | t when isnumericval t -> true
  | _ -> false
;;

exception NoRuleApplies
;;


(* "eval1" is the evaluation method and the core of the evaluation process. *)
(* 	It implements the intended logic of every built term.	 	            *)
let rec eval1 vctx tm = match tm with
    (* E-IfTrue *)
    TmIf (TmTrue, t2, _) ->
      t2

    (* E-IfFalse *)
  | TmIf (TmFalse, _, t3) ->
      t3

    (* E-If *)
  | TmIf (t1, t2, t3) ->
      let t1' = eval1 vctx t1 in
      TmIf (t1', t2, t3)

    (* E-Succ *)
  | TmSucc t1 ->
      let t1' = eval1 vctx t1 in
      TmSucc t1'

    (* E-PredZero *)
  | TmPred TmZero ->
      TmZero

    (* E-PredSucc *)
  | TmPred (TmSucc nv1) when isnumericval nv1 ->
      nv1

    (* E-Pred *)
  | TmPred t1 ->
      let t1' = eval1 vctx t1 in
      TmPred t1'

    (* E-IszeroZero *)
  | TmIsZero TmZero ->
      TmTrue

    (* E-IszeroSucc *)
  | TmIsZero (TmSucc nv1) when isnumericval nv1 ->
      TmFalse

    (* E-Iszero *)
  | TmIsZero t1 ->
      let t1' = eval1 vctx t1 in
      TmIsZero t1'

    (* E-AppAbs *)
  | TmApp (TmAbs(x, _, t12), v2) when isval v2 ->
      subst x v2 t12

    (* E-App2: evaluate argument before applying function *)
  | TmApp (v1, t2) when isval v1 ->
      let t2' = eval1 vctx t2 in
      TmApp (v1, t2')

    (* E-App1: evaluate function before argument *)
  | TmApp (t1, t2) ->
      let t1' = eval1 vctx t1 in
      TmApp (t1', t2)

    (* E-LetV *)
  | TmLetIn (x, v1, t2) when isval v1 ->
      subst x v1 t2

    (* E-Let *)
  | TmLetIn(x, t1, t2) ->
      let t1' = eval1 vctx t1 in
      TmLetIn (x, t1', t2) 
      
    (* E-FixBeta *)
  | TmFix (TmAbs (x, _, t2)) ->             (* | Addition for Fix term (2.2) 		                        *)
      subst x tm t2                         (* | Fix of an abstraction is evaluated as the substituted term *)
                                            (* |                                                            *)
    (* E-Fix *)                             (* |                                                            *)
  | TmFix t1 ->                             (* |                                                            *)
      let t1' = eval1 vctx t1 in            (* |                                                            *)
      TmFix t1'                             (* |                                                            *)

    (*E-Concat1*)
  | TmConcat (TmString v1, TmString v2) ->  (* | Addition for String type (2.3)                        *)
      TmString (v1 ^ v2)                    (* | Given two strings returns the concatenation           *)
                                            (* |                                                       *)
  (*E-Concat2*)                             (* |                                                       *)
  | TmConcat (TmString v1, t2) ->           (* | If one or both items are not inmediately string terms *)
    let t2' = eval1 vctx t2 in              (* | context is looked up for coincidences                 *)
    TmConcat (TmString v1, t2')             (* |                                                       *)
                                            (* |                                                       *)
  (*E-Concat3*)                             (* |                                                       *)
  | TmConcat (t1, t2) ->                    (* |                                                       *)
    let t1' = eval1 vctx t1 in              (* |                                                       *)
    TmConcat (t1', t2)                      (* |                                                       *)

    (*E- ProjTuple*)
  | TmTupleProj (t, pos) when (isval t) -> (match t with
      TmTuple terms -> (try List.nth terms (pos - 1) with 
        _ -> raise NoRuleApplies)  
      | _ -> raise (Type_error "Term is not a tuple")
  )

      (*E-Proj*)
  | TmTupleProj (t1, pos) ->                                    (* | Addition for Tuple type (2.5) 		                        *)
      let t1' = eval1 vctx t1 in                                (* |                            		                        *)
      TmTupleProj(t1', pos)                                     (* | For tuple projections, it's evaluated the tuple and passed *)
                                                                (* | as parameter to another projection eval      		        *)
    (*E-Tuple*)                                                 (* |                            		                        *)
    | TmTuple terms ->                                          (* |                             		                        *)
        let rec evaluate terms = (match terms with              (* | For tuples, all individual terms are evaluated recursively *)
          [] -> raise NoRuleApplies                             (* |                                 	                        *)
          | v::t when isval v -> let t' = evaluate t in v::t'   (* |                                	                        *)
          | t1::t -> let t1' = eval1 vctx t1 in t1'::t          (* |                                	                        *)
        )                                                       (* |                                	                        *)
        in let terms' = evaluate terms in TmTuple terms'        (* |                                	                        *)
        

      
  
  | TmPrintNat TmZero  ->                           (* | Additions for Tuple type (2.5)                          *)
        (print_int 0; TmUnit)                       (* |                                                         *)
                                                    (* | When print_nat is evaluated, first match ocurs at TmZero*)
  | TmPrintNat (TmSucc t)  ->                       (* | "0" prints out in that case.                            *)
      (let rec f n = function                       (* |                                                         *)
        TmZero -> print_int n                       (* | Then, the value is taken if it's the                    *)
      | TmSucc s -> f (n + 1) s                     (* | successor or predecessor a natural number. In that case:*)
      | _ -> ()                                     (* | - 0 is printed if its value is 0                        *)
      in f 1 t; TmUnit)                               (* | - Next number is printed if it's a successor            *)
                                                    (* | - Previous number is printed if it's a predecessor      *)
  | TmPrintNat (TmPred t)  ->                       (* |                                                         *)
      (let rec f n = function                       (* | If it matchs any other term the inner expression        *)
        TmZero -> print_int n                       (* | is evaluated for looking for coincidences               *)
      | TmSucc s -> f (n - 1) s                     (* |                                                         *)
      | _ -> ()                                     (* |                                                         *)
                                                    (* |                                                         *)
      in f 1 t; TmUnit)                               (* |                                                         *)
  | TmPrintNat t  ->                                (* |                                                         *)
        let t' = eval1 vctx t in TmPrintNat t'      (* |                                                         *)
                                                    (* |                                                         *)
  | TmPrintString (TmString s) ->                   (* | When print_string is evaluated, inner expression        *)
        (print_string s; TmUnit)                    (* | is evaluated:                                           *)
                                                    (* | - If it's a string, it's printed out                    *)
  | TmPrintString t ->                              (* | - If it's not, inner expression is evaluated further    *)
     let t' = eval1 vctx t in TmPrintString t'       (* |        until a string is found                          *)
                                                    (* |                                                         *)
  | TmPrintNewline t ->                             (* | When print_newline is evaluated, a newline is printed.  *)
        (print_newline (); TmUnit)                  (* |                                                         *)
                                                    (* |                                                         *)
  | TmReadNat TmUnit ->                             (* | In read_nat, the input is parsed and results in         *) 
        let rec f = function                        (* |   - a TmZero item if it's 0                             *)
            0 -> TmZero                             (* |   - The succ. of the prev. (the number) if it's a num.  *)
         |  n -> TmSucc (f (n-1))                   (* |   - If it's any other term, it's looked further         *)
         in f (int_of_string (read_line ()))        (* |                                                         *)
                                                    (* |                                                         *)
  | TmReadNat t ->                                  (* |                                                         *)
        let t' = eval1 vctx t in TmReadNat t'        (* |                                                         *)
  | TmReadString TmUnit ->                          (* | In read_string, the input is parsed and results in      *)
        (let s = read_line () in TmString s)        (* |   - a TmString item if it's a TmUnit                    *)
                                                    (* |   - If it's any other term, it's looked further         *)
  | TmReadString t ->                               (* |                                                         *)
        let t' = eval1 vctx t in TmReadString t'     (* |                                                         *)
  
    (* TmVar *)                 (*| Addition for context *)
  | TmVar s ->                  (*|                      *)
      getbinding vctx s         (*|                      *)
  | _ ->                       
      raise NoRuleApplies      
;;


(* "apply_ctx" method *)
let apply_ctx ctx termino =
  (* "List.fold_left" applies certain function to a list *)
  List.fold_left (fun t x -> subst x (getbinding ctx x) t) termino (free_vars termino)


(* Evaluation method that abstracts "eval1" from error catching *)
let rec eval vctx tm =
  try
    let tm' = eval1 vctx tm in
    eval vctx tm'
  with
    NoRuleApplies -> apply_ctx vctx tm
;;


(* "execute" method is the first one called after processing the input and matches it either with a evaluation or a bind. *)
(*  If it has to make an evaluation, it calls "eval" method that does the evaluation work *)
(*  If a binding has to be made, "addbinding" method is called *)
let execute (vctx, tctx) = function
    Eval tm ->
       let tyTm = typeof tctx tm in
       let tm' = eval vctx tm in
       print_endline (string_of_term tm' ^ " : " ^ string_of_ty tyTm);
       (vctx, tctx)
       
  | Bind (s, tm) ->
     let tyTm = typeof tctx tm in
     let tm' = eval vctx tm in
     print_endline ( s ^ " : " ^ string_of_ty tyTm ^ " = " ^ string_of_term tm' );
     (addbinding vctx s tm', addbinding tctx s tyTm)
