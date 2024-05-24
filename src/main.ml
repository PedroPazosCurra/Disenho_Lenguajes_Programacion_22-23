
open Parsing;;
open Lexing;;

open Lambda;;
open Parser;;
open Lexer;;


(* top_level_loop method: Interface superior iterative loop.                                                       *)
(*  As an implementation of the multi-line expressions functionality (1.1), each of the sentences being processed  *)
(*  are determined on the basis of the defined separator ";;".                                                     *)
(*                                                                                                                 *)
(*  For the implementation of the global definition context (2.2), the input will no longer be passed directly     *)
(*  to the evaluator, but to the new execute function, which will determine whether to evaluate the term or whether*)
(*  it should be assigned to a free variable name. 								   *)
(*  This is explored in more detail in the code of its implementation.						   *)
(*                                                                                                                 *)
(*  Throughout the subsequent code, "vctx" and "tctx" are used for context.  					   *)
(*  These will be our two lists for defining the context of free variables and terms, respectively.                *)
(*  They will be used throughout the lower implementation to play with 	     					   *)
(*  the association of free variables with values or terms.       						   *)
(*													 	   *)
(* file_eating_loop has been added as a new functionality for IO operations (2.10).                                *)
(*  The core of this new method is very similar to top_level_loop but it instead opens, reads and closes a file from parameter *)
(*  Besides, top_level_loop has been abstracted into a new method that differentiates the now two different executable files   *)
(*   that are created on compilation. "top" works as usual while "run" takes a file as parameter and processes it  *)



(* Note: We are aware that "file_eating_loop" method isn't working as it should. But it was decided to be kept in the file due *)
(*        to it being innocuous to the rest of the program.                                                                    *)

exception Invalid_input of string;;

let file_eating_loop () =
  if((Array.length Sys.argv) != 2) then
  	raise (Invalid_input "\nStop right there. \n\t Usage: ./run [text file]");
  print_endline "I'm the batch compilator. Let's read your file!";
  let file = open_in Sys.argv.(1) in
  let rec loop (vctx, tctx) =
    flush stdout;
    try
      let c = s token (Lexing.from_channel file)  in
      loop (execute (vctx, tctx) c);
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
     	 close_in file;
         print_endline "Farewell!";
  in
    loop (emptyctx, emptyctx)
  ;;



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

let top_run () = match Sys.argv.(0) with
	|"./top" -> top_level_loop()
	|"./run" -> file_eating_loop()
	| _ 	 -> print_endline "...Oops"
;;

top_run() ;;

