
open Parsing;;
open Lexing;;

open Lambda;;
open Parser;;
open Lexer;;

(* Bucle iterativo superior de la interfaz *)

let rec process_line line =
  if String.contains line ';' then s token (from_string (String.sub line 0 (1 + String.index line ';')))
  else begin
    flush stdout;
    process_line (line ^ "\n" ^ read_line ())
  end

let top_level_loop () =
  print_endline "Evaluator of lambda expressions...";
  let rec loop (vctx, tctx) =
    print_string ">> ";
    flush stdout;
    try
      let c = process_line (String.trim (read_line ()))  in
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

