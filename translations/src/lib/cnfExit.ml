open Types
open Types.Ast
open Pprint

module E = Expand
module N = Nnf
module R = Reduc
module S = Simp
module T = Tra
module T2 = Tra2


(* [is_clause] checks that the given AST is a clause. This function can only
   be called on an AST containing Or, And or Not. No Equiv or Implies! *)
let rec is_clause (ast:Ast.t) : bool = match ast with
  | Top | Bottom | Prop _ | Not (Prop _) -> true
  | Or (x,y) -> is_clause x && is_clause y
  | And _ -> false
  | _ -> false

(** [push_lit] allows to translate into CNF the non-CNF disjunction [d or cnf]
    ([d] is the literal we want to add, [cnf] is the existing CNF form).
    For example:
    {[
          d  or  ((a or not b) and (not c))            <- is not in CNF
    <=>   push_lit (d) ((a or not b) and not c)
    <=>   (d or a or b) and (d or not c)               <- is in CNF
    ]}
    This function is necessary because [d or cnf] (with [cnf] an arbitrary CNF
    form) is not a CNF form and must be modified. Conversely, the form
          [d  and  ((a or not b) and (not c))]
    doesn't need to be modified because it is already in CNF.  *)

(** [fresh_dummy] generates a 'dummy' proposition named ["&i"] with [i] being a
    self-incrementing index.
    This function allows to speed up and simplify the translation of some
    forms of Or.
    NOTE: OCaml's functions can't have 0 param: we use the unit [()]. *)
let dummy_term_count = ref 0
let fresh_dummy () =
  incr dummy_term_count; Prop ("&" ^ (string_of_int !dummy_term_count))

let is_dummy (name:string) : bool = (name).[0] = '&'

let debug = ref false (* The debug flag activated by --debug-cnf *)

(** [print_debug] is just printing debug info in [to_cnf] *)
let print_debug (prefix:string) depth (formulas:Ast.t list) : unit =
  (* [indent] creates a string that contains N indentations *)
  let rec indent = function 0 -> "" | i -> (indent (i-1))^"\t"
  and string_of_asts = function
    | [] -> ""
    | cur::[] -> string_of_ast ~utf8:true cur
    | cur::next -> (string_of_ast ~utf8:true cur)^", "^(string_of_asts next)
  in print_endline ((indent depth) ^ (string_of_int depth) ^ " " ^ prefix
                    ^ (string_of_asts formulas))

(** [stop] is a type is used in [to_cnf] in order to stop it after a number of
   recursions. See (1) below *)
type stop = No | Yes of int

let rec ast_to_cnf ?debug_cnf:(d=false) (ast:Ast.t) : Ast.t =
  debug := d;
  to_cnf 0 No ast

(** Actual logic of [ast_to_cnf]
    [depth] tells what is the current level of recursion and
    helps for debugging the translation to CNF.
    [stop] tells to_cnf if it should stop or continue the recursion.

    - (1) When transforming to CNF, we want to make sure that "outer" to_cnf
        transformations are made before inner ones. For example, in
        {v
            to_cnf (Not (to_cnf ((a and b) => c)))
        v}
        we want to limit the inner [to_cnf] expansion to let the possibily for
        the outer to_cnf to "simplify" with the Not as soon as possible.
        For inner [to_cnf], we simply use [to_cnf_once] to prevent the inner
        [to_cnf] from recursing more than once.
    - (2) All bottom and top must disappear for the CNF transformation; we use
        the standard transformation to remove them (a and not a, b or not b) *)
and to_cnf depth (stop:stop) (ast:Ast.t) : Ast.t =
  if !debug then print_debug "in:  " depth [ast];
  if depth = 0 then
    let string_of_command () =
      let tmp_file = "../sdata/00_cp_pre_syntax.txt" in
      let _ = Sys.command @@ "echo '"^ (Pprint.string_of_ast ast)^"' >>" ^ tmp_file in
      let chan = open_in tmp_file in
      close_in chan
      in
      string_of_command();
      ;

      let string_of_command1 () =
        let tmp_file = "../sdata/00_cp_pre_syntax.txt" in
        let _ = Sys.command @@ "sed '/<=>/q;p' ../sdata/00_cp_pre_syntax.txt > ../sdata/00_cp_pre_syntax_xx.txt" in
        let chan = open_in tmp_file in
        close_in chan
        in
        string_of_command1();

      
        let string_of_command2 () =
          let tmp_file = "../sdata/00_cp_pre_syntax.txt" in
          let _ = Sys.command @@ "sed -i 's/\"ass(/ass_/g' ../sdata/00_cp_pre_syntax_xx.txt" in
          let chan = open_in tmp_file in
          close_in chan
          in
          string_of_command2();

          let string_of_command2b () =
            let tmp_file = "../sdata/00_cp_pre_syntax.txt" in
            let _ = Sys.command @@ "sed -i 's/ass(/ass_/g' ../sdata/00_cp_pre_syntax_xx.txt" in
            let chan = open_in tmp_file in
            close_in chan
            in
            string_of_command2b();     

          let string_of_command3() =
            let tmp_file = "../sdata/00_cp_pre_syntax.txt" in
            let _ = Sys.command @@ "sed -i 's/)\"/_/g' ../sdata/00_cp_pre_syntax_xx.txt" in
            let chan = open_in tmp_file in
            close_in chan
            in
            string_of_command3();

            let string_of_command4() =
              let tmp_file = "../sdata/00_cp_pre_syntax.txt" in
              let _ = Sys.command @@ "sed -i 's/ideal(/ideal_/g' ../sdata/00_cp_pre_syntax_xx.txt" in
              let chan = open_in tmp_file in
              close_in chan
              in
              string_of_command4();



            let string_of_command5() =
                let tmp_file = "../sdata/00_cp_pre_syntax.txt" in
                let _ = Sys.command @@ "sed -i 's/) <=>/_ <=>/g' ../sdata/00_cp_pre_syntax_xx.txt" in
                let chan = open_in tmp_file in
                close_in chan
                in
                string_of_command5();


            let string_of_command6() =
                  let tmp_file = "../sdata/00_cp_pre_syntax.txt" in
                  let _ = Sys.command @@ "sed -i 's/val(/val_/g' ../sdata/00_cp_pre_syntax_xx.txt" in
                  let chan = open_in tmp_file in
                  close_in chan
                  in
                  string_of_command6();
                  
            let string_of_command7() =
                    let tmp_file = "../sdata/00_cp_pre_syntax.txt" in
                    let _ = Sys.command @@ "sed -i 's/_)//g' ../sdata/00_cp_pre_syntax_xx.txt" in
                    let chan = open_in tmp_file in
                    close_in chan
                    in
                    string_of_command7();
                    
            let string_of_command8() =
                      let tmp_file = "../sdata/00_cp_pre_syntax.txt" in
                      let _ = Sys.command @@ "sed -i 's/,/_/g' ../sdata/00_cp_pre_syntax_xx.txt" in
                      let chan = open_in tmp_file in
                      close_in chan
                      in
                      string_of_command8();


            let string_of_command9() =
                      let tmp_file = "../sdata/00_cp_pre_syntax.txt" in
                      let _ = Sys.command @@ "sed -i 's/not th_val(/th not val_/g' ../sdata/00_cp_pre_syntax_xx.txt" in
                      let chan = open_in tmp_file in
                      close_in chan
                      in
                      string_of_command9();

            let string_of_command10() =
                      let tmp_file = "../sdata/00_cp_pre_syntax.txt" in
                      let _ = Sys.command @@ "sed -i 's/th_/{2} /g' ../sdata/00_cp_pre_syntax_xx.txt" in
                      let chan = open_in tmp_file in
                      close_in chan
                      in
                      string_of_command10();


            let string_of_command11() =
                      let tmp_file = "../sdata/00_cp_pre_syntax.txt" in
                      let _ = Sys.command @@ "sed -i 's/th/{2}/g' ../sdata/00_cp_pre_syntax_xx.txt" in
                      let chan = open_in tmp_file in
                      close_in chan
                      in
                      string_of_command11();


            let string_of_command12() =
                      let tmp_file = "../sdata/00_cp_pre_syntax.txt" in
                      let _ = Sys.command @@ "sed -i 's/justif(/justif_/g' ../sdata/00_cp_pre_syntax_xx.txt" in
                      let chan = open_in tmp_file in
                      close_in chan
                      in
                      string_of_command12();  
                      
                      
            let string_of_command13() =
                      let tmp_file = "../sdata/00_cp_pre_syntax.txt" in
                      let _ = Sys.command @@ "sed -i 's/__x)/_/g' ../sdata/00_cp_pre_syntax_xx.txt" in
                      let chan = open_in tmp_file in
                      close_in chan
                      in
                      string_of_command13();  

            let returncode = (Sys.command "grep -o 'px.*' ../sdata/00_cp_pre_syntax_xx.txt > ../sdata/03_goal.txt") in
                      flush stdout; flush stderr;
                      if returncode == 0 then Printf.printf ""
                      else Printf.printf "[Error ] not possible to generate file solvedatacp/solver_ini_state 1.txt ";

            let string_of_command14() =
                      let tmp_file = "../sdata/00_cp_pre_syntax.txt" in
                      let _ = Sys.command @@ "sed -i 's/px) and //g' ../sdata/03_goal.txt" in
                      let chan = open_in tmp_file in
                      close_in chan
                      in
                      string_of_command14();

            let string_of_command15() =
                        let tmp_file = "../sdata/00_cp_pre_syntax.txt" in
                        let _ = Sys.command @@ "sed 's/px.*//' ../sdata/00_cp_pre_syntax_xx.txt > ../sdata/01_ini_state.txt" in
                        let chan = open_in tmp_file in
                        close_in chan
                        in
                        string_of_command15();  

            let string_of_command2c () =
                          let tmp_file = "../sdata/00_cp_pre_syntax.txt" in
                          let _ = Sys.command @@ "sed -i 's/_)//g' ../sdata/01_ini_state.txt" in
                          let chan = open_in tmp_file in
                          close_in chan
                          in
                          string_of_command2c();  



            let string_of_command16() =
                        let tmp_file = "../sdata/00_cp_pre_syntax.txt" in
                        let _ = Sys.command @@ "sed -i '$ s/....$//' ../sdata/01_ini_state.txt" in
                        let chan = open_in tmp_file in
                        close_in chan
                        in
                          string_of_command16();




            let string_of_command17() =
                          let tmp_file = "../sdata/00_cp_pre_syntax.txt" in
                          let _ = Sys.command @@ "sed -i -r '1s/^.{2}//' ../sdata/01_ini_state.txt" in
                          let chan = open_in tmp_file in
                          close_in chan
                          in
                          string_of_command17();


  if depth = 0 then    
    exit 0;



  if (match stop with Yes 0 -> true | _ -> false) then ast else (* See (1) above*)
    let to_cnf_once = to_cnf (depth+1) (match stop with Yes i->Yes (i-1) | No->Yes 1) in
    let to_cnf = to_cnf (depth+1) (match stop with Yes i->Yes (i-1) | No->No) in
    let cnf = begin match ast with
    | Top when depth=0 -> let t = fresh_dummy () in Or (t,Not t) (* See (2) above *)
    | Top -> Top
    | Bottom when depth=0 -> let t = fresh_dummy () in And (t,Not t) (* See (2) *)
    | Bottom -> Bottom
    | Prop x -> Prop x
    | And (x,y) -> let (x,y) = (to_cnf x, to_cnf y) in
      begin
        match x,y with
        | Top,x | x,Top     -> x
        | Bottom,_|_,Bottom -> Bottom
        | x,y               -> And (x,y)
      end

    | Or (x,y) -> let (x,y) = (to_cnf x, to_cnf y) in
      begin
        match x,y with
        | Top,x | x,Top     -> x
        | Bottom,_|_,Bottom -> Bottom
        | x,y               -> Or (x,y)
      end


    | Th x ->
      begin
        match x with
        | Top        -> Bottom
        | Bottom     -> Top
        | x          -> Th x
      end

    | Not x ->
      begin
        match x with
        | Top        -> Bottom
        | Bottom     -> Top
        | Prop x     -> Not (Prop x)
        | Not x     -> to_cnf x
        | Ba x     -> Not (Ba (to_cnf x)) 
        | Box x    -> Not (Box (to_cnf x))         
        | Bh x     -> Not (Bh (to_cnf x)) 
        | Da x     -> Not (Da (to_cnf x)) 
        | Dia x     -> Not (Dia (to_cnf x))         
	      | Dh x     -> Not (Dh (to_cnf x)) 
        | Ta x     -> Not (Ta (to_cnf x)) 
        | Th x     -> Not (to_cnf (Th x)) 
        | And (x,y) -> Or  (to_cnf (Not x), to_cnf (Not y)) (* De Morgan *)
        | Or (x,y)  -> And (to_cnf (Not x), to_cnf (Not y)) (* De Morgan *)
        | _ -> to_cnf (Not (to_cnf_once x)) (* See (1) above*)
      end

    | Red x ->
      begin
        match x with
        | x          -> to_cnf x |> N.getNNF |> E.getExpand |> S.getSimp |> T.getTra 
      end

    | Fx (x,y) -> let (x,y) = (to_cnf x, to_cnf y)  in 
      begin 
        match x, y with
            | x, y -> Ba (Implies(to_cnf x, to_cnf y))
      end  

    | Nnf x ->
      begin
        match x with
        | x      -> N.getNNF x |> S.getSimp |> E.getExpand |> S.getSimp |> T.getTra |> E.getExpand 
      end    
      
    | Simp x ->
      begin
        match x with
        | x      -> S.getSimp x 
      end    
  
    | Tra x ->
      begin
        match x with
        | x      -> T.getTra x         
      end    
  
    | Plusa (x,y) -> let (x,y) = (to_cnf x, to_cnf y)  in 
      begin 
        match x, y with
            | x, y -> Ba (Implies(to_cnf x, to_cnf y))
      end  


    | Plush (x,y) -> let (x,y) = (to_cnf x, to_cnf y)  in 
      begin 
        match x, y with
        | r, And(x,y) -> to_cnf (And(Plush(r,x), Plush(r,y)))
        | r, Or(x,y) -> to_cnf (Or(Plush(r,x), Plush(r,y)))      
        | x, Ta y when x = y -> Top
        | x, Th y when x <> y -> Th y
        | x, Ba y -> Ba (Implies(to_cnf x,to_cnf y)) 
        | x, Box y -> Box (Implies(to_cnf x,to_cnf y))         
        | _, Bh y -> Bh (to_cnf y)
        | x, Da y -> Da (And(to_cnf x,to_cnf y)) 
        | x, Dia y -> Dia (And(to_cnf x,to_cnf y))         
        | _, Dh y -> Dh (to_cnf y)
        | _,x     -> to_cnf x    
      end  




    | Plus (x,y) -> let (x,y) = (to_cnf x, to_cnf y) in
      begin
        Printf.printf "Cambio E.L.+a \n"; 
        match x,y with
        | x, Ta y when x = y -> Top
        | x, Ba y -> Printf.printf "Entro a : x, Ba y \n";
                    Ba (Implies(to_cnf x, to_cnf y))
        | x, Box y -> Box (Implies(x,y))

        | x, Not Ba y -> Not (Ba(Implies(x,y)))        
        | x, Bh y -> Bh (Implies(x,y))
        | x, Not Bh y -> Not (Bh(Implies(x,y)))                             
        | _, Th x -> Th x
        | _, Ta x -> Ta x
        | r, And(x,y) -> to_cnf(And(Plusa(r,x), Plusa(r,y)))
        | _,x     -> to_cnf x
      end

    | Hh x ->
      begin
        match x with
        | Top        -> Bottom
        | Bottom     -> Top
        | x          -> Hh (to_cnf x)
      end



    | Ta x ->
      begin
        match x with
        | Top        -> Bottom
        | Bottom     -> Top
        | x          -> Ta (to_cnf x)
      end  

      
      
    | Ha x ->
      begin
        match x with
        | Top        -> Bottom
        | Bottom     -> Top
        | x          -> Ha (to_cnf x)
      end      

        
    | Ba x ->
      begin
        match x with
        | Top        -> Bottom
        | Bottom     -> Top
        | x          -> Ba (to_cnf x)
      end

    | Box x ->
      begin
        match x with
        | Top        -> Bottom
        | Bottom     -> Top
        | x          -> Box (to_cnf x)
      end      

    | Bh x ->
      begin
        match x with
        | Top        -> Bottom
        | Bottom     -> Top
        | x          -> Bh (to_cnf x)
      end  

      
    | Da x ->
      begin
        match x with
        | Top        -> Bottom
        | Bottom     -> Top
        | x          -> Da (to_cnf x)
      end

    | Dia x ->
      begin
        match x with
        | Top        -> Bottom
        | Bottom     -> Top
        | x          -> Dia (to_cnf x)
      end

    | Dh x ->
      begin
        match x with
        | Top        -> Bottom
        | Bottom     -> Top
        | x          -> Dh (to_cnf x)
      end



    (* Note on [Or] and the Tseytin transform:
           When translating [x or y] into CNF and that either x or y is a
           conjunction (= isn't a clause), we must avoid the 'natural' translation
           that should occur when translating (1) into (2): (2) would have an
           exponential number of clauses. Instead, we use arbitrary variables
           created by [genterm], denoted by &1, &2... and use the Tseytin
           transform (3) which yields a linear number of clauses.
                (x1 and y1)  or  (x2 and y2)                                (1)
                (x1 or x2) and (x1 or y2) and (y1 or x2) or (etc...)        (2)
                (&1 or &2) and (not &1 or x1) and (not &1 or y1)            (3)
                          and (not &2 or x2) and (not &2 or y2)
        *)
    | Implies (x,y) -> to_cnf (Or (Not x, y))
    | Equiv (x,y) -> to_cnf (And (Implies (x,y), Implies (y,x)))
    | Xor (x,y) -> to_cnf (And (Or (x,y), Or (Not x, Not y)))
    | _ -> failwith ("cnf + [shouldnt happen] this doesn't seem to be a formula: '"
      ^ (string_of_ast ~debug:true ast) ^ "'")
    end
    in

    (*print_debug "out : " depth [cnf];*)
    if !debug then print_debug "out: " depth [cnf];
    (* Last important thing: make sure no more Bot/Top are in the formula. *)
    if depth=0 && Eval.has_top_or_bot cnf then to_cnf cnf else cnf


let clauses_of_cnf (neg:'a->'a) (fresh:unit->'a) (ast:Ast.t)
  : 'a list list * ('a, string) Hashtbl.t * (string, 'a) Hashtbl.t =
  (* num = a number that will serve to identify a literal
      lit = a literal that has a number inside it to identify it *)
  let str_to_lit = Hashtbl.create 500 in
  let lit_to_str = Hashtbl.create 500 in (* this duplicate is for the return value *)
  let rec process_cnf ast : 'a list list = match ast with
    | And  (x,y) -> (process_cnf x) @ (process_cnf y)
    | x when is_clause x -> [process_clause x]
    | _ -> failwith ("CNF: was expecting a conjunction of clauses but got '" ^ (string_of_ast ~debug:true ast) ^ "'")
  and process_clause (ast:Ast.t) : 'a list = match ast with
    | Prop str        -> (gen_lit str)::[]
    | Not (Prop str) -> (neg (gen_lit str))::[]
    | Or (x,y) -> process_clause x @ process_clause y
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