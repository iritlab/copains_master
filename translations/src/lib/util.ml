(*########################################################*)
(*         Ast pour les formules de logique modale        *)
(*########################################################*)

(** This module explicits the AST of modal logic formulas *)

(*open Types.Ast*)

type ident = string

type axiom = 
  | AxS
  | AxB
  | Ax4
  | Ax5
  | AxCD





let rev list =
    let rec aux acc = function
      | [] -> acc
      | h::t -> aux (h::acc) t in
    aux [] list;;


  let rec insert_at x n = function
  | [] -> rev [x]
  | h :: t as l ->  rev (if n = 0 then x :: l else h :: insert_at x (n-1) t);;





