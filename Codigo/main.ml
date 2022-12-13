
open Parsing;;
open Lexing;;

open Lambda;;
open Parser;;
open Lexer;;


(* Método top_level_loop: Bucle iterativo superior de la interfaz.                                                 *)
(*  Como implementación de la funcionalidad de expresiones multilínea (1.1), cada una de las frases que se procesan*)
(*  son determinadas en base al separador definido ";;".                                                           *)
(*                                                                                                                 *)
(*  Para la implementación del contexto de definiciones globales (2.2),                                            *)
(*  la entrada será pasada ya no directamente al evaluador, sino a la nueva función execute,                       *)
(*  que determinará si evaluar el término o por el contrario debe ser asignado a un nombre de variable libre.      *)
(*  Se explora con más detenimiento en el código de su implementación.                                             *)
(*                                                                                                                 *)
(*  Durante todo el código posterior se usan para el contexto "vctx" y "tctx". Estas serán nuestras dos listas para*)
(*  definir el contexto de variables libres y términos, respectivamente. Se usarán a lo largo de toda la           *)
(*  implementación inferior para jugar con la asociación de variables libres con valores o términos.               *)

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

