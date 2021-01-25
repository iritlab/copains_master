(*########################################################*)
(*         Ast pour les formules de logique modale        *)
(*########################################################*)

(** This module explicits the AST of modal logic formulas *)

open Types.Ast

let rec getSimp = function
     | Prop p -> Prop p
     | Not (Prop p) -> Not (Prop p)
     | And (x,y) -> And (getSimp x,getSimp y)     
     | Or (x,y) -> Or (getSimp x,getSimp y)
     | Ba x -> 
        begin
            match x with
              | Ta y -> Ta y
              | Ha y -> Ha y
              | Th y -> Th y
              | Hh y -> Hh y
              | Ba y -> getSimp (Ba (getSimp y))
              | Da y -> Da y
              | _ -> Ba x
        end
     | Da x -> 
        begin
            match x with
              | Ta y -> Ta y
              | Ba y -> Ba y
              | Th y -> Th y
              | Hh y -> Hh y              
              | Ha y -> Ha y
              | Da y -> getSimp (Da (getSimp y))
              | _ -> Da x
        end        

     | Ta x -> Ta (getSimp x)
     | Th x -> Th x 
     | Ha x -> Ha (getSimp x)
     | Hh x -> Hh x               
     | _ ->  Top


