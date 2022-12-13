
open Parsing;;
open Lexing;;

open Lambda;;
open Parser;;
open Lexer;;


(* top_level_loop method: Interface superior iterative loop.                                                       *)
(*  As an implementation of the multi-line expressions functionality (1.1), each of the sentences being processed  *)
(*  are determined on the basis of the defined separator ";;".                                                     *)
(*                                                                                                                 *)
(*  For the implementation of the global definition context (2.2), the input will no longer be passed directly 	   *)
(*  to the evaluator, but to the new execute function, which will determine whether to evaluate the term or whether*)
(*  it should be assigned to a free variable name. 								                                                 *)
(*  This is explored in more detail in the code of its implementation.						                                 *)
(*                                                                                                                 *)
(*  Throughout the subsequent code, "vctx" and "tctx" are used for context.  					                             *)
(*  These will be our two lists for defining the context of free variables and terms, respectively.                *)
(*  They will be used throughout the lower implementation to play with 	     					                             *)
(*  the association of free variables with values or terms.       						                                     *)


let top_level_loop () =
  print_endline "Evaluator of lambda expressions...";
  let rec loop (vctx, tctx) =
    print_string ">> ";
    flush stdout;
    try
      let c = s token (Lexing.from_channel stdin)  in
      loop (execute (vctx, tctx) c)
    with
       Lexical_error ->
         print_endline "Lexical error";
         loop (vctx, tctx)
     | Parse_error ->
         print_endline "Syntax error";
         loop (vctx, tctx)
     | Type_error e ->
         print_endline ("Type error: " ^ e);
         loop (vctx, tctx)
     | End_of_file ->
         print_endline "Farewell!"
  in
    loop (emptyctx, emptyctx)
  ;;

top_level_loop () 
;;

