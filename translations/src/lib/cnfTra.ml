open Types
open Types.Ast
open Pprint

(* [is_clause] checks that the given AST is a clause. This function can only
   be called on an AST containing Or, And or Not. No Equiv or Implies! *)

let rec is_clause (ast:Ast.t) : bool = 
  match ast with
  | Top | Bottom | Prop _ | Not (Prop _) -> true
  | Or (x,y) -> is_clause x && is_clause y
  | And _ -> false
  | _ -> false


let debug = ref false (* The debug flag activated by --debug-cnf *)

(** [print_debug] is just printing debug info in [to_cnf] *)
let print_debug (prefix:string) depth (formulas:Ast.t list) : unit =
  (* [indent] creates a string that contains N indentations *)
  (*Printf.printf "aca_43 \n";*)
  let rec indent = function 0 -> "" | i -> (indent (i-1))^"\t"
  and string_of_asts = function
    | [] -> ""
    | cur::[] -> string_of_ast ~utf8:true cur
    | cur::next -> (string_of_ast ~utf8:true cur)^", "^(string_of_asts next)
  in print_endline ((indent depth) ^ (string_of_int depth) ^ " - " ^ prefix
                      ^ (string_of_asts formulas))
                    (*^ (string_of_asts formulas)^ "\n ")*)

(** [stop] is a type is used in [to_cnf] in order to stop it after a number of
   recursions. See (1) below *)
type stop = No | Yes of int

let rec ast_to_cnf ?debug_cnf:(d=false) (ast:Ast.t) : Ast.t = debug := d;
  to_cnf 0 No ast
  
and to_cnf depth (stop:stop) (ast:Ast.t) : Ast.t =
  (* I think HERE is the final translation to PL *)
  (*if !debug then print_debug "in:  " depth [ast];*)
  print_debug "Propositional Language :  " depth [ast];
  flush stdout; flush stderr;
  
  if (match stop with Yes 0 -> true | _ -> false) then ast else (* See (1) above*)
    let to_cnf = to_cnf (depth+1) (match stop with Yes i->Yes (i-1) | No->No) in
    let cnf = begin match ast with
    | Top -> Top
    | _ ->   ast       
    end
    in
    
    (*if depth=0 then *)
        Printf.printf("\n\n Translation to L_Prop completed ... \n\n");  
        flush stdout; flush stderr;
     
        
    (* Last important thing: make sure no more Bot/Top are in the formula. *)
    if depth=0 && Eval.has_top_or_bot cnf then to_cnf cnf 
      (*if depth=2 then print_debug "FINISH : " depth [cnf];*)
      else  cnf 
let clauses_of_cnf (neg:'a->'a) (fresh:unit->'a) (ast:Ast.t)
  : 'a list list * ('a, string) Hashtbl.t * (string, 'a) Hashtbl.t =
  (* num = a number that will serve to identify a literal
      lit = a literal that has a number inside it to identify it *)
  let str_to_lit = Hashtbl.create 500 in
  let lit_to_str = Hashtbl.create 500 in (* this duplicate is for the return value *)
  let failwith msg = raise (Failure msg) in
  let rec process_cnf ast : 'a list list = match ast with
    | And  (x,y) -> (process_cnf x) @ (process_cnf y)
    | x when is_clause x -> [process_clause x]
    | _ -> assert false    
(*    | _ -> failwith ("Reduction Process Completed '" ^ "Ok" ^ "'")*)
    
(*    | _ -> failwith ("CNF: was expecting a conjunction of clauses but got '" ^ (string_of_ast ~debug:true ast) ^ "'")*)    
  and process_clause (ast:Ast.t) : 'a list = match ast with
    | Prop str        -> (gen_lit str)::[]
    | Not (Prop str) -> (neg (gen_lit str))::[]
    | _ -> failwith ("CNF: was expecting a clause but got '" ^ (string_of_ast ~debug:true ast) ^ "'")
    
  and gen_lit (s:string) : 'a =
    try Hashtbl.find str_to_lit s
    with Not_found ->
      (let lit = fresh () in
       Hashtbl.add str_to_lit s lit;
       Hashtbl.add lit_to_str lit s;
       lit)
  in let clauses = process_cnf ast in clauses, lit_to_str, str_to_lit

let print_table (int_of_lit: 'a->int) (out:out_channel) ?(prefix="") (table:('a,string) Hashtbl.t) =
  let print_lit_and_name lit name = Printf.fprintf out "%s%s %d\n" prefix name (int_of_lit lit)
  in Hashtbl.iter print_lit_and_name table

let print_clauses (out:out_channel) ?(prefix="") (str_of_lit: 'a->string) (clauses:'a list list) : unit =
  let rec string_of_clause (cl:'a list) = match cl with
    | [] -> "0"
    | cur::next -> (str_of_lit cur) ^" "^ (string_of_clause next)
  and print_listclause (cl:'a list list) = match cl with
    | [] -> ()
    | cur::next -> Printf.fprintf out "%s%s\n" prefix (string_of_clause cur); print_listclause next
  in
  print_listclause clauses

  