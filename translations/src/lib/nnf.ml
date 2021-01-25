(*########################################################*)
(*         Ast pour les formules de logique modale        *)
(*########################################################*)

(** This module explicits the AST of modal logic formulas *)

open Types.Ast

type ident = string

type axiom = 
  | AxS
  | AxB
  | Ax4
  | Ax5
  | AxCD

  (** This function propagate a negation in a modal formula.
     Helps to compute the NNF of the formula *)
     let rec prop_neg = function
     | Prop p ->  Not (Prop p)
     | Not x -> getNNF x     

     | Ta x -> Ha (prop_neg x)  
     | Th x -> Hh (prop_neg x)


     | Ba x -> Da (prop_neg x)
     | Bh x -> Dh (prop_neg x)


     | Da x -> Ba (prop_neg x)
     | Dh x -> Bh (prop_neg x)

     | And (x,y) -> Or (prop_neg x,prop_neg y)
     | Or (x, y) -> And (prop_neg x,prop_neg y)
     | Implies (x,y) -> And (getNNF x,prop_neg y)

     | Top -> Bottom
     | Bottom -> Top
     | _ -> Bottom
   
   
    (** Returns the negativ-normal form of the formula *)
   and getNNF = function
     | Prop p -> Prop p
     | Not (Prop p) -> Not (Prop p)
     | Not x -> begin
                  match x with
                  | _ -> prop_neg x
                end                
    | Implies (x, y) -> getNNF (Or (Not x, y))

     | Th x ->  Th (getNNF x)
     | Ta x -> Ta (getNNF x)

     | Ba x -> Ba (getNNF x)
     | Bh x -> Bh (getNNF x)     

     | Da x -> Da (getNNF x)
     | Dh x -> Dh (getNNF x)   

     | Hh x -> Hh (getNNF x)
     | Ha x -> Ha (getNNF x)       


     | And (x, y) -> And (getNNF x, getNNF y)
     | Or (x, y) ->  Or (getNNF x, getNNF y)

 
     | Top -> Top
     | Bottom -> Bottom
     | _ -> Bottom
     
   
let rec formLength = function
  | Top | Prop _ -> 1
  | Not f | Box f | Dia f -> formLength f + 1  
  | And (f1,f2) | Or (f1,f2) | Implies (f1,f2) -> (formLength f1) + (formLength f2) + 1
  | _ -> 1

  let rec modDegree = function
  | Top | Bottom | Prop _ -> 1
  | Not p -> modDegree p + 1
  | Dia x ->  modDegree x + 1
  | Box x ->  modDegree x + 1  
  | And (x,y) -> (modDegree x) + (modDegree y) + 1
  | Or (x,y) ->  (modDegree x) + (modDegree y) + 1
  | _ -> 1;
