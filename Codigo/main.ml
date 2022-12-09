
open Parsing;;
open Lexing;;

open Lambda;;
open Parser;;
open Lexer;;

(* Bucle iterativo superior de la interfaz *)

let top_level_loop () =
  print_endline "Evaluator of lambda expressions...";
  let rec loop (vctx, tctx) =
    print_string ">> ";
    flush stdout;
    try
      let c = s token (from_string (read_line ())) in
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

