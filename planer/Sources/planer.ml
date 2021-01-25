open Printf

class t  (model:string) (actions:string) (goal:string) (encoding : int) =
object (self)


method search = (* notimed_search *)

let command cmd =
  Sys.command cmd
in


(*
let filemodel = "planerdata/"^model in
let fileactions = "planerdata/"^actions in
let filegoal = "planerdata/"^goal in
*)

let filemodel = "../sdata/"^model in
let fileactions = "../sdata/"^actions in
let filegoal = "../sdata/"^goal in



(*Printf.printf "FILEMODEL ==> %s \n" filemodel;*)


flush stdout; flush stderr;
Utils.print "Select TouIST\n";
flush stdout; flush stderr;
let returncode = (command "./main.exe --version") in
Utils.print "\n";



flush stdout; flush stderr;
if returncode == 0 then Utils.print ""
else begin Utils.eprint "[Error %d] please install TouIST with : brew install touist\n" returncode; exit returncode; end;


(** START SATPLAN-SOLVE - JFDX 20200408 **)
if (encoding = 1) then
begin

let read_line i = try Some (input_line i) with End_of_file -> None  in
let lines_from_files filename = 
  let rec lines_from_files_aux i acc = match (read_line i) with 
    | None -> List.rev acc
    | Some s -> lines_from_files_aux i (s :: acc) in 
  lines_from_files_aux (open_in filename) [] in



let plan m = 

(*Printf.printf " ACA estamos despues del : plan m : %i  \n" !m;*)

let rec extract k list =
    if k <= 0 then [ [] ]
    else match list with
         | [] -> []
         | h :: tl ->
            let with_h = List.map (fun l -> h :: l) (extract (k-1) tl) in
            let without_h = extract k tl in
            with_h @ without_h in

let flatten l =
  let rec loop res = function
    | [] -> List.rev res
    | h::t -> loop (List.rev_append h res) t
  in
    loop [] l in

(*let reslst = flatten (extract !m (lines_from_files "planerdata/02_op.txt")) in*)

let string_of_command4 () =
let tmp_file = "planerdata/99_tmp.txt" in
let _ = Sys.command (Printf.sprintf "cat %s > solvedatacp/tmp_actions.txt" fileactions) in
let chan = open_in tmp_file in
          close_in chan
      in
      string_of_command4(); 



let string_of_command4 () =
let tmp_file = "planerdata/99_tmp.txt" in
let _ = Sys.command (Printf.sprintf "sed -i -e 's/inform(1,2,//g' -e 's/.$//' -e 's/\[/(/g' -e 's/]/)/g' solvedatacp/tmp_actions.txt") in
let chan = open_in tmp_file in
          close_in chan
      in
      string_of_command4(); 







let reslst = flatten (extract !m (lines_from_files "solvedatacp/tmp_actions.txt")) in
  (*Printf.printf " ACa estamos 003 : \n";*)
  (*List.iter(printf "OP flat ...: %s\n") reslst;*)

      let a = ref 1 in
      let ll = ref [] in
      ll := reslst;
      let s = ref 0 in 
        s := List.length reslst;

        (* OJO - OJO esta es la parte que llena el file pato.txt  - VERY IMPORTANT*)
            while (!a <= List.length reslst ) do
              a := !a + 1;
                let string_of_command4 () =
                let tmp_file = "planerdata/99_tmp.txt" in
                let _ = Sys.command (Printf.sprintf "echo '%s' >> solvedatacp/pato_%i.txt" (List.hd !ll) !m) in
                let chan = open_in tmp_file in
                close_in chan
              in
              string_of_command4();  
              ll := List.tl !ll;
            done ;
(**)            

                let string_of_command4 () =
                let tmp_file = "planerdata/99_tmp.txt" in
                let _ = Sys.command (Printf.sprintf "sed -i -e 's_.*_plusa[{2}[&_' -e 's/$/]/' solvedatacp/pato_%i.txt" !m) in
                let chan = open_in tmp_file in
                close_in chan
              in
              string_of_command4();  



let parse3ac() = 
    let lines = ref "" in
    let lines2 = ref "" in
    let action = ref "" in
    let i = ref 1 in
    let tmp_file = Printf.sprintf "solvedatacp/pato_%i.txt"!m in
    let chan = open_in tmp_file in
    let j = ref 0 in
    let touistcode2 = ref 0 in
    j := 0;
    try
      while true do
        j := !j + 1;
        action := input_line chan;
        lines := !action ^", "^ !lines;
        lines2 := !action ^"\n"^ !lines2;
        (*Printf.printf "ANTES DE ENTRAR AL IF %s - %i \n\n" !lines !m;*)
        if !j = !m then  begin 
                        (*Printf.printf "DESPUES DEL IF %s - %i \n" !lines !m;*)
                        let string_of_command4 () =
                                
                                let tmp_file = "planerdata/01_model.txt" in
                                let _ = (*Printf.printf "DESPUES DEL IF 2222 %s %i %i \n" !lines !m !i;*)
                                        Sys.command (Printf.sprintf "echo '%s' >> solvedatacp/pato_seq_%i_%i.txt" !lines !m !i);  
                                        Sys.command (Printf.sprintf "echo '%s' > solvedatacp/plan_final2.txt" !lines2);
                                        (*Printf.printf "DESPUES DEL IF 333 %s %i %i \n" !lines !m !i;*) in
                                let chan = open_in tmp_file in
                                close_in chan
                              in
                              string_of_command4();  

                        let string_of_command4 () =
                                
                                let tmp_file = "planerdata/01_model.txt" in
                                let _ = Sys.command (Printf.sprintf "sed -i -e 's/]/)/g' -e 's/\[/(/g' solvedatacp/pato_seq_%i_%i.txt" !m !i)   in
                                let chan = open_in tmp_file in
                                close_in chan
                              in
                              string_of_command4();  


                        let string_of_command4 () =
                                
                                let tmp_file = "planerdata/01_model.txt" in
                                let _ = Sys.command (Printf.sprintf "cat %s > solvedatacp/03_goalb.txt" filegoal)   in
                                let chan = open_in tmp_file in
                                close_in chan
                              in
                              string_of_command4();  


                        let string_of_command4 () =
                                
                                let tmp_file = "planerdata/01_model.txt" in
                                let _ = Sys.command (Printf.sprintf "sed -i '1 i\ [1]' solvedatacp/03_goalb.txt")   in
                                let chan = open_in tmp_file in
                                close_in chan
                              in
                              string_of_command4();  





                        let string_of_command5 () =
                                let tmp_file = "planerdata/01_model.txt" in
                                let _ = Sys.command (Printf.sprintf "cat %s solvedatacp/pato_seq_%i_%i.txt solvedatacp/03_goalb.txt >  solvedatacp/formula_%i_%i.txt" filemodel !m !i !m !i)   in 
                                let chan = open_in tmp_file in
                                close_in chan
                                in
                                string_of_command5();  

 


                        let string_of_command4 () =
                                
                                let tmp_file = "planerdata/01_model.txt" in
                                (*let _ = Sys.command (Printf.sprintf "sed -i -e 's/)plusa/))=> plusa/g' -e 's/)),/)),[a]/g' -e 's/~/not /g' solvedatacp/formula_%i_%i.txt" !m !i)   in*)
                                let _ = Sys.command (Printf.sprintf "sed -i '0,/plusa/s/plusa/)=> plusa/' solvedatacp/formula_%i_%i.txt" !m !i)   in
                                let chan = open_in tmp_file in
                                close_in chan
                              in
                              string_of_command4();  

                            for j = 0 to !m + 1 do


                              let string_of_command4 () =
                                      
                                      let tmp_file = "planerdata/01_model.txt" in
                                      let _ = Sys.command (Printf.sprintf "echo ')' >> solvedatacp/formula_%i_%i.txt" !m !i)   in
                                      let chan = open_in tmp_file in
                                      close_in chan
                                    in
                                    string_of_command4(); 

                            done;          

                              let string_of_command4 () =
                                
                                let tmp_file = "planerdata/01_model.txt" in
                                let _ = Sys.command (Printf.sprintf "sed -i '1 i\ red(not ([1](' solvedatacp/formula_%i_%i.txt" !m !i)   in
                                
                                (*sed -i '1 i\anything' file*)

                                let chan = open_in tmp_file in
                                close_in chan;

                              in
                              string_of_command4(); 


                              let string_of_command4d() =
                                let tmp_file = "planerdata/01_model.txt" in
                                let _ = Sys.command (Printf.sprintf "sed -i 's/),/,/g' solvedatacp/formula_%i_%i.txt" !m !i)   in
                                let chan = open_in tmp_file in
                                close_in chan
                                in
                                string_of_command4d();                                  


                              let string_of_command4e() =
                                let tmp_file = "cp_pre_syntax.txt" in
                                let _ = Sys.command (Printf.sprintf "sed -i '$ s/.$//' solvedatacp/formula_%i_%i.txt" !m !i)   in
                                let chan = open_in tmp_file in
                                close_in chan
                                in
                                string_of_command4e();                                


                                let string_of_command4c() =
                                  let tmp_file = "planerdata/01_model.txt" in
                                  let _ = Sys.command (Printf.sprintf "sed -i 's/{2}(/{2} /g' solvedatacp/formula_%i_%i.txt" !m !i)   in
                                  let chan = open_in tmp_file in
                                  close_in chan
                                  in
                                  string_of_command4c();  


                              (* To manteint the Originial LDA formula*)    
                              let string_of_command4x () =
                                let tmp_file = "planerdata/01_model.txt" in
                                let _ = Sys.command (Printf.sprintf "cat solvedatacp/formula_%i_%i.txt > solvedatacp/formula_lda_%i_%i.txt" !m !i !m !i)   in
                                let chan = open_in tmp_file in
                                close_in chan;

                              in
                              string_of_command4x(); 


                              (* Here we only replace the {2} by th_ to speed up the process... and that's all*)    
                              let string_of_command4b() =
                                let tmp_file = "planerdata/01_model.txt" in
                                let _ = Sys.command (Printf.sprintf "sed -i 's/{2} /th_/g' solvedatacp/formula_%i_%i.txt" !m !i)   in
                                let chan = open_in tmp_file in
                                close_in chan
                                in
                                string_of_command4b();  





                              (* Encapsulates the Process Plan procedure *)  
                              let process () =     
                                Printf.printf "Testing formula_%i_%i.txt \n" !m !i;
                                flush stdout; flush stderr;
                                let seq = ref [] in
                                seq := lines_from_files (Printf.sprintf "solvedatacp/pato_seq_%i_%i.txt"!m !i) ;
                                Printf.printf "%s \n" (List.hd !seq);

                                touistcode2 := (command (Printf.sprintf "./main.exe solvedatacp/formula_%i_%i.txt > solvedatacp/plan_%i_%i.txt" !m !i !m !i));
                                Printf.printf "touistcode ==>  %i \n\n" !touistcode2;
                                flush stdout; flush stderr;

                                if !touistcode2 = 8 then 
                                  begin
                                    Printf.printf "Plan of size %i found  : \n" !m;
                                    flush stdout; flush stderr;
                                    let string_of_command4w () =
                                                          
                                      let tmp_file = "planerdata/01_model.txt" in
                                      let _ = Sys.command (Printf.sprintf "cat 'solvedatacp/pato_seq_%i_%i.txt' > solvedatacp/plan_final.txt" !m !i)   in
                                      flush stdout; flush stderr;
                                      let chan = open_in tmp_file in
                                      close_in chan
                                    in
                                    string_of_command4w(); 


                                  let string_of_command4b() =
                                    let tmp_file = "planerdata/01_model.txt" in
                                    let _ = Sys.command (Printf.sprintf "cat solvedatacp/plan_final.txt > solvedatacp/plan_final_show.txt")   in
                                    let chan = open_in tmp_file in
                                    close_in chan
                                  in
                                  string_of_command4b();  


                                let string_of_command4b() =
                                  let tmp_file = "planerdata/01_model.txt" in
                                  let _ = Sys.command (Printf.sprintf "sed -i 's/),/\\n/g' solvedatacp/plan_final_show.txt")   in
                                  let chan = open_in tmp_file in
                                  close_in chan
                                  in
                                  string_of_command4b(); 


(*****)

                              (* Here we only replace the {2} by th_ to speed up the process... and that's all*)    
                              let string_of_command4b() =
                                let tmp_file = "planerdata/01_model.txt" in
                                let _ = Sys.command (Printf.sprintf "sed -i 's/ plusa({2}(val_te_ass_//g' solvedatacp/plan_final_show.txt")   in
                                let chan = open_in tmp_file in
                                close_in chan
                                in
                                string_of_command4b();  

                              let string_of_command4b() =
                                let tmp_file = "planerdata/01_model.txt" in
                                let _ = Sys.command (Printf.sprintf "sed -i 's/plusa({2}(val_te_ass_//g' solvedatacp/plan_final_show.txt")   in
                                let chan = open_in tmp_file in
                                close_in chan
                                in
                                string_of_command4b();  

                              let string_of_command4b() =
                                let tmp_file = "planerdata/01_model.txt" in
                                let _ = Sys.command (Printf.sprintf "sed -i 's/ plusa({2}(//g' solvedatacp/plan_final_show.txt")   in
                                let chan = open_in tmp_file in
                                close_in chan
                                in
                                string_of_command4b();                                 

                                let string_of_command4b() =
                                  let tmp_file = "planerdata/01_model.txt" in
                                  let _ = Sys.command (Printf.sprintf "sed -i 's/_h_/_/g' solvedatacp/plan_final_show.txt")   in
                                  let chan = open_in tmp_file in
                                  close_in chan
                                  in
                                  string_of_command4b();                                  

                                  let string_of_command4b() =
                                    let tmp_file = "planerdata/01_model.txt" in
                                    let _ = Sys.command (Printf.sprintf "sed -i '/^ *$/d' solvedatacp/plan_final_show.txt")   in
                                    let chan = open_in tmp_file in
                                    close_in chan
                                    in
                                    string_of_command4b();                                  

                                let string_of_command4b() =
                                  let tmp_file = "planerdata/01_model.txt" in
                                  let _ = Sys.command (Printf.sprintf "sed -i '$ s/.$//' solvedatacp/plan_final_show.txt")   in
                                  let chan = open_in tmp_file in
                                  close_in chan
                                  in
                                  string_of_command4b();     


(****)



                                    
                                    exit 0;
                                  end                                
                                else  
                                  begin
                                  let string_of_command4w () =
                                                          
                                    let tmp_file = "planerdata/01_model.txt" in
                                    let _ = Sys.command (Printf.sprintf "cat 'solvedatacp/pato_seq_%i_%i.txt' > solvedatacp/plan_final.txt" !m !i)   in
                                    flush stdout; flush stderr;
                                    let chan = open_in tmp_file in
                                    close_in chan
                                  in
                                  string_of_command4w(); 
                                  end    
                              in
                              process();     
                                

                            i := 1 + !i;
                            j := 0;
                            lines := "";
                        end

      done;

     with End_of_file -> close_in chan; 







     in
      parse3ac();
in


let pm = ref 0 in
  (*pm := List.length (lines_from_files "planerdata/02_op.txt");*)
  pm := List.length (lines_from_files fileactions);
  
let px = ref 0 in
for p = 1 to !pm do
  (*Printf.printf "EMPEZO ...\n";*)
  px := p;
  plan px;

done;

Printf.printf " Plan not found \n";

let string_of_command4b() =
  let tmp_file = "planerdata/01_model.txt" in
  let _ = Sys.command (Printf.sprintf "echo 'Sorry, it was not possible to find an ideal sport for you' > solvedatacp/plan_final_show.txt")   in
  let chan = open_in tmp_file in
  close_in chan
in
string_of_command4b();  



exit 0;


end;
end