(*########################################################*)
(*         Ast pour les formules de logique modale        *)
(*########################################################*)

(** This module explicits the AST of modal logic formulas *)

open Types.Ast

let dummy_term_count = ref 0
let fv_num = ref 1
let vars = ref (Array.make 100000 "");;

let rec find a x n =
    if a.(n) = x then n 
    else find a x (n+1);;

let fresh_dummy () =
  incr dummy_term_count; Prop ("fv" ^ (string_of_int !dummy_term_count))

  let fresh_dummy2 () =
    incr dummy_term_count; Prop ((string_of_int !dummy_term_count))  ;;

    let find2 xs x =
        let i = ref (-1) in
        let () = Array.iteri (fun n elt -> if x = elt then i := n else ()) xs in
        !i;;

    let rec getTra = function
     | Prop p -> Prop p
     | Not Prop p -> Not (Prop p)
     | And (x,y) -> And (getTra x,getTra y)     
     | Or (x,y) -> Or (getTra x,getTra y)          
     | Ba x -> Box x
     | Da x -> Dia (getTra x)
     | Ta x -> 
        begin
            match x with
            | Not x -> let t =  (Prop("fv" ^ string_of_int ((int_of_char (Char.chr (Char.code ((Pprint.string_of_ast x).[0]))))))) in And (t, (Box (Not(getTra x))))
            | x -> let t = Prop("fv" ^ string_of_int ((int_of_char (Char.chr (Char.code ((Pprint.string_of_ast x).[0])))))) in And (t, (Box (getTra x)))
            
        end             

    | Th x  ->        


                if (find2 !vars (Pprint.string_of_ast x) < 0) then 
                    begin    
                        incr dummy_term_count;
                        !vars.(!dummy_term_count)  <- (Pprint.string_of_ast x);
                        fv_num := !dummy_term_count;
                    end
                else
                        fv_num := (find2 !vars (Pprint.string_of_ast x));
                

                begin
                    match x with
                    | _ -> Prop("fv" ^ string_of_int (!fv_num))

                end  

    | Ha x ->   begin
                    match x with
                    | Not x | x -> Not (Prop("fv" ^ string_of_int ((int_of_char (Char.chr (Char.code ((Pprint.string_of_ast x).[0])))))))
                end  


    | Hh x  -> 

            begin
                match x with
                | Not x -> Not (getTra(Th x))
                | x -> getTra(Th x) 

                end  

    | _ ->   Top
            
