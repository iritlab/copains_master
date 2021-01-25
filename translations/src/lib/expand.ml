(*########################################################*)
(*         Ast pour les formules de logique modale        *)
(*########################################################*)

(** This module explicits the AST of modal logic formulas *)

open Types.Ast
let rec getExpand = function
     | Prop p -> Prop p
     | Not (Prop p) -> Not (Prop p)
     | Not x -> Not (x)     
     | And (x,y) -> And (getExpand x,getExpand y)     
     | Or (x,y) ->  Or (getExpand x,getExpand y)
     | Ba x -> 
        begin
            match x with

            | And(Ta y, Ta z) -> Ba (Ta (And(getExpand y, getExpand z)))             
            | And(Th y, Th z) -> Ba (Th (And(getExpand y, getExpand z)))                         

            | And(y, Ta z) -> getExpand(And(Ba (getExpand y),Ta (getExpand z)))
            | And(y, Ha z) -> getExpand(And(Ba (getExpand y),Ha (getExpand z)))
            | And(y, Th z) -> getExpand(And(Ba (getExpand y),Th (getExpand z)))
            | And(y, Hh z) -> getExpand(And(Ba (getExpand y),Hh (getExpand z))) 
            | And(y, Ba z) -> getExpand(And(Ba (getExpand y),Ba (getExpand z))) 
            | And(y, Da z) -> getExpand(And(Ba (getExpand y),Da (getExpand z))) 
            | And(Ta y, z) -> getExpand(And(Ta (getExpand y),Ba (getExpand z)))
            | And(Ha y, z) -> getExpand(And(Ha (getExpand y),Ba (getExpand z))) 
            | And(Th y, z) -> getExpand(And(Th (getExpand y),Ba (getExpand z)))
            | And(Hh y, z) -> getExpand(And(Hh (getExpand y),Ba (getExpand z))) 
            | And(Ba y, z) -> getExpand(And(Ba (getExpand y),Ba (getExpand z))) 
            | And(Da y, z) -> getExpand(And(Da (getExpand y),Ba (getExpand z))) 

            | And(y, z)    -> getExpand(And(Ba (getExpand y),Ba (getExpand z)))                                     


            | Or(Ta y, Ta z) -> Ba (Ta (Or(getExpand y, getExpand z)))             
            | Or(y, Ta z) -> getExpand(Or(Ba (getExpand y),Ta (getExpand z))) 
            | Or(y, Ha z) -> getExpand(Or(Ba (getExpand y),Ha (getExpand z))) 
            | Or(y, Th z) -> getExpand(Or(Ba (getExpand y),Th (getExpand z)))
            | Or(y, Hh z) -> getExpand(Or(Ba (getExpand y),Hh (getExpand z))) 
            | Or(y, Ba z) -> getExpand(Or(Ba (getExpand y),Ba (getExpand z))) 
            | Or(y, Da z) -> getExpand(Or(Ba (getExpand y),Da (getExpand z))) 
            | Or(Ta y, z) -> getExpand(Or(Ta (getExpand y),Ba (getExpand z)))
            | Or(Ha y, z) -> getExpand(Or(Ha (getExpand y),Ba (getExpand z))) 
            | Or(Th y, z) -> getExpand(Or(Th (getExpand y),Ba (getExpand z)))    
            | Or(Hh y, z) -> getExpand(Or(Hh (getExpand y),Ba (getExpand z))) 
            | Or(Ba y, z) -> getExpand(Or(Ba (getExpand y),Ba (getExpand z))) 
            | Or(Da y, z) -> getExpand(Or(Da (getExpand y),Ba (getExpand z)))  

            | Or(y, z)    -> getExpand(Or(Ba (getExpand y),Ba (getExpand z)))                         

            | x -> Ba (getExpand x)
        end

      | Box x -> 
        begin
            match x with
            | And(Ta y, Ta z) -> Box (Ta (And(getExpand y, getExpand z)))             
            | And(Th y, Th z) -> Box (Th (And(getExpand y, getExpand z)))                         

            | And(y, Ta z) -> getExpand(And(Box (getExpand y),Ta (getExpand z)))
            | And(y, Ha z) -> getExpand(And(Box (getExpand y),Ha (getExpand z)))
            | And(y, Th z) -> getExpand(And(Box (getExpand y),Th (getExpand z)))
            | And(y, Hh z) -> getExpand(And(Box (getExpand y),Hh (getExpand z))) 
            | And(y, Box z) -> getExpand(And(Box (getExpand y),Box (getExpand z))) 
            | And(y, Da z) -> getExpand(And(Box (getExpand y),Da (getExpand z))) 
            | And(Ta y, z) -> getExpand(And(Ta (getExpand y),Box (getExpand z)))
            | And(Ha y, z) -> getExpand(And(Ha (getExpand y),Box (getExpand z))) 
            | And(Th y, z) -> getExpand(And(Th (getExpand y),Box (getExpand z)))
            | And(Hh y, z) -> getExpand(And(Hh (getExpand y),Box (getExpand z))) 
            | And(Box y, z) -> getExpand(And(Box (getExpand y),Box (getExpand z))) 
            | And(Da y, z) -> getExpand(And(Da (getExpand y),Box (getExpand z))) 

            | And(y, z)    -> getExpand(And(Box (getExpand y),Box (getExpand z)))                                     


            | Or(Ta y, Ta z) -> Box (Ta (Or(getExpand y, getExpand z)))             



            | Or(y, Ta z) -> getExpand(Or(Box (getExpand y),Ta (getExpand z))) 
            | Or(y, Ha z) -> getExpand(Or(Box (getExpand y),Ha (getExpand z))) 
            | Or(y, Th z) -> getExpand(Or(Box (getExpand y),Th (getExpand z)))
            | Or(y, Hh z) -> getExpand(Or(Box (getExpand y),Hh (getExpand z))) 
            | Or(y, Box z) -> getExpand(Or(Box (getExpand y),Box (getExpand z))) 
            | Or(y, Da z) -> getExpand(Or(Box (getExpand y),Da (getExpand z))) 
            | Or(Ta y, z) -> getExpand(Or(Ta (getExpand y),Box (getExpand z)))
            | Or(Ha y, z) -> getExpand(Or(Ha (getExpand y),Box (getExpand z))) 
            | Or(Th y, z) -> getExpand(Or(Th (getExpand y),Box (getExpand z)))
            | Or(Hh y, z) -> getExpand(Or(Hh (getExpand y),Box (getExpand z))) 
            | Or(Box y, z) -> getExpand(Or(Box (getExpand y),Box (getExpand z))) 
            | Or(Da y, z) -> getExpand(Or(Da (getExpand y),Box (getExpand z)))  

            | Or(y, z)    -> getExpand(Or(Box (getExpand y),Box (getExpand z)))                         

            | x -> Box (getExpand x)
        end        


      | Dia x -> 
        begin
            match x with

            | And(Ta y, Ta z) -> Dia (Ta (And(getExpand y, getExpand z)))             
            | And(Th y, Th z) -> Dia (Th (And(getExpand y, getExpand z)))                         

            | And(y, Ta z) -> getExpand(And(Dia (getExpand y),Ta (getExpand z)))
            | And(y, Ha z) -> getExpand(And(Dia (getExpand y),Ha (getExpand z)))
            | And(y, Th z) -> getExpand(And(Dia (getExpand y),Th (getExpand z)))
            | And(y, Hh z) -> getExpand(And(Dia (getExpand y),Hh (getExpand z))) 
            | And(y, Dia z) -> getExpand(And(Dia (getExpand y),Dia (getExpand z))) 
            | And(y, Da z) -> getExpand(And(Dia (getExpand y),Da (getExpand z))) 
            | And(Ta y, z) -> getExpand(And(Ta (getExpand y),Dia (getExpand z)))
            | And(Ha y, z) -> getExpand(And(Ha (getExpand y),Dia (getExpand z))) 
            | And(Th y, z) -> getExpand(And(Th (getExpand y),Dia (getExpand z)))
            | And(Hh y, z) -> getExpand(And(Hh (getExpand y),Dia (getExpand z))) 
            | And(Dia y, z) -> getExpand(And(Dia (getExpand y),Dia (getExpand z))) 
            | And(Da y, z) -> getExpand(And(Da (getExpand y),Dia (getExpand z))) 

            | And(y, z)    -> getExpand(And(Dia (getExpand y),Dia (getExpand z)))                                     


            | Or(Ta y, Ta z) -> Dia (Ta (Or(getExpand y, getExpand z)))             



            | Or(y, Ta z) -> getExpand(Or(Dia (getExpand y),Ta (getExpand z))) 
            | Or(y, Ha z) -> getExpand(Or(Dia (getExpand y),Ha (getExpand z))) 
            | Or(y, Th z) -> getExpand(Or(Dia (getExpand y),Th (getExpand z)))
            | Or(y, Hh z) -> getExpand(Or(Dia (getExpand y),Hh (getExpand z))) 
            | Or(y, Dia z) -> getExpand(Or(Dia (getExpand y),Dia (getExpand z))) 
            | Or(y, Da z) -> getExpand(Or(Dia (getExpand y),Da (getExpand z))) 
            | Or(Ta y, z) -> getExpand(Or(Ta (getExpand y),Dia (getExpand z)))
            | Or(Ha y, z) -> getExpand(Or(Ha (getExpand y),Dia (getExpand z))) 
            | Or(Th y, z) -> getExpand(Or(Th (getExpand y),Dia (getExpand z)))
            | Or(Hh y, z) -> getExpand(Or(Hh (getExpand y),Dia (getExpand z))) 
            | Or(Dia y, z) -> getExpand(Or(Dia (getExpand y),Dia (getExpand z))) 
            | Or(Da y, z) -> getExpand(Or(Da (getExpand y),Dia (getExpand z)))  

            | Or(y, z)    -> getExpand(Or(Dia (getExpand y),Dia (getExpand z)))                         

            | x -> Dia (getExpand x)
        end


     | Da x -> 
        begin
            match x with
            | And(y, z) -> getExpand(And(Da (getExpand y),Da (getExpand z)))
            | Or(y, z) -> getExpand(Or(Da (getExpand y), Da (getExpand z))) 
            | x -> Da (getExpand x)
        end        

     | Ta x -> 
        begin
            match x with
            | And(Th y, Th z) -> getExpand(And( (getExpand y), (getExpand z))) 

            | And(y, z) -> getExpand(And(Ta (getExpand y),Ta (getExpand z)))
            | Or(y, z) -> getExpand(Or(Ta (getExpand y), Ta (getExpand z))) 
            | x -> Ta (getExpand x)
        end             
        

     | Th x ->     
        begin
            match x with
            | x ->    Th x            
        end                  

     | Ha x -> 
        begin
            match x with
            | And(Th y, Hh z) -> getExpand(And(Th (getExpand y),Hh (getExpand z))) 
            | And(Hh y, Hh z) -> getExpand(And(Ha (getExpand y),Ha (getExpand z)))             
            | And(y, z) -> getExpand(And(Ha (getExpand y), Ha (getExpand z)))                             
            | Or(y, z) ->  getExpand(Or(Ha (getExpand y), Ha (getExpand z))) 
            | Hh x -> (getExpand (Ha (x)))
            | x -> Ha (getExpand x)
        end                  
     | Hh x -> 
        begin
            match x with
            | x -> Hh x
        end                  

     | _ ->   Top

