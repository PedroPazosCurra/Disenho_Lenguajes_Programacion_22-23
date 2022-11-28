
open Parsing;;
open Lexing;;

open Lambda;;
open Parser;;
open Lexer;;

(* Bucle iterativo superior de la interfaz *)

let top_level_loop () =
  print_endline "Evaluator of lambda expressions...";
  let rec loop ctx =
    print_string ">> ";
    flush stdout;
    try
      let tm = s token (from_string (read_line ())) in
      let tyTm = typeof ctx tm in
      print_endline (string_of_term (eval tm) ^ " : " ^ string_of_ty tyTm);
      loop ctx
    with
       Lexical_error ->
         print_endline "Lexical error";
         loop ctx
     | Parse_error ->
         print_endline "Syntax error";
         loop ctx
     | Type_error e ->
         print_endline ("Type error: " ^ e);
         loop ctx
     | End_of_file ->
         print_endline "Farewell!"
  in
    loop emptyctx
  ;;

top_level_loop () ;;

