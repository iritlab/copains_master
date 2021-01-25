open Printf

(*
class t  (encoding : int) =
object (self)
*)


class t  (score:string) (svol:string) (sinput:string) (encoding : int) =
object (self)


 
  method search = (* notimed_search *)

let command cmd =
  Sys.command cmd
in


(*
let filemodel = "../sdata/"^model in
let fileactions = "../sdata/"^actions in
let filegoal = "../sdata/"^goal in
*)


let filescore = "../sdata/"^score in
let filesvol = "../sdata/"^svol in
let filesinput = "../sdata/"^sinput in
let filetouist = "../sdata/main.exe" in

let filesvol_final = "../sdata/00_delta0.txt" in

(*let filesinput = "brevdata/"^sinput in*)


flush stdout; flush stderr;
Utils.print "Select TouIST\n";
flush stdout; flush stderr;
(*let returncode = (command "./main.exe --version") in*)
let returncode = (command (Printf.sprintf "%s --version" filetouist)) in




flush stdout; flush stderr;
if returncode == 0 then Utils.print ""
else begin Utils.eprint "[Error %d] please install TouIST with : brew install touist\n" returncode; exit returncode; end;


(** START SATPLAN-SOLVE - JFDX 20200408 **)
if (encoding == 100) then


begin
Printf.printf "ENTRO 100 \n";
Printf.printf "sc : %s \n" filescore;
Printf.printf "sv : %s \n" filesvol;
Printf.printf "si : %s \n" filesinput;
Printf.printf "touist : %s \n" filetouist;






let read_line i = try Some (input_line i) with End_of_file -> None  in

let lines_from_files filename = 
  let rec lines_from_files_aux i acc = match (read_line i) with 
    | None -> List.rev acc
    | Some s -> lines_from_files_aux i (s :: acc) in 
  lines_from_files_aux (open_in filename) [] in

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

Printf.printf "touistcode ==>  ACA before plan\n";

let plan m = 
    let touistcode2 = ref 99 in 

    (* Verify ZInput is not in contradiction with ZCore*)      
    (*touistcode2 := (command (Printf.sprintf "(cat filescore; cat brevdata/sinput.txt) | ./touist.exe --sat --solve - > brevsets/out.emodel_01.txt 2> brevsets/out.touisterr_01.txt"));*)

    (*touistcode2 := (command (Printf.sprintf "./main.exe brevsets/x_%i_%i.txt > brevsets/xs_%i_%i.txt" !m !i !m !i));*)
    (*Printf.printf "touistcode ==>  %i \n" !touistcode2;*)

    Printf.printf "touistcode ==> ACA after plan %i \n" !m;

    let string_of_command4 () =
      let tmp_file = filescore in
      (*let _ = Sys.command (Printf.sprintf "cat filescore brevdata/sinput.txt > brevsets/s_test.txt" )   in*)
      let _ = Sys.command (Printf.sprintf "cat '%s' '%s' > brevsets/s_test.txt" filescore filesinput)   in
      let chan = open_in tmp_file in
      close_in chan
    in
    string_of_command4(); 


    let string_of_command4 () =
      let tmp_file = filescore in
      let _ = Sys.command (Printf.sprintf "sed -i 's/$/ and/' brevsets/s_test.txt" )   in
      let chan = open_in tmp_file in
      close_in chan
    in
    string_of_command4();


    let string_of_command4 () =
      let tmp_file = filescore in
      let _ = Sys.command (Printf.sprintf "sed -i '$s/ and//' brevsets/s_test.txt" )   in
      let chan = open_in tmp_file in
      close_in chan
    in
    string_of_command4();    

    
    let string_of_command4 () =
      let tmp_file = filescore in
      let _ = Sys.command (Printf.sprintf "sed -i '1s/^/red (/' brevsets/s_test.txt" )   in
      let chan = open_in tmp_file in
      close_in chan
    in
    string_of_command4();      


    let string_of_command4 () =
      let tmp_file = filescore in
      let _ = Sys.command (Printf.sprintf "sed -i '$s/$/)/' brevsets/s_test.txt")   in
      let chan = open_in tmp_file in
      close_in chan
    in
    string_of_command4();      


    let string_of_command4 () =
                          let tmp_file = filescore in
                          let _ = Sys.command (Printf.sprintf "sed -i 's/\&/and/g' brevsets/s_test.txt" )   in

                          let chan = open_in tmp_file in
                          close_in chan
                        in
                        string_of_command4(); 




    (*touistcode2 := (command (Printf.sprintf "./main.exe brevsets/s_test.txt > brevsets/out.emodel_01.txt"));*)
    touistcode2 := (command (Printf.sprintf "%s brevsets/s_test.txt > brevsets/out.emodel_01.txt" filetouist));

    if !touistcode2 = 0 then 
      begin
       (* Printf.printf "Sigma Input is NOT in contradiction with Sigma Core (SAT) touistcode ==> %i \n\n " !touistcode2;*)
       Printf.printf "##################### Trying with combinations of size ==> %i \n\n" !m;
       (* Aca generar el file con las combinaciones : brevsets/svc2.txt -  starting with size = m *)

        let reslst = flatten (extract !m (lines_from_files filesvol)) in
        (*let reslst = (lines_from_files filesvol) in*)

          (*List.iter(printf "VOL :: %s\n") reslst;*)

          let a = ref 1 in
              let ll = ref [] in
              ll := reslst;
              (* Generate the combinations of m elements from ZVol*)
              (*List.iter(printf "VOL-ll :: %s\n") !ll;*)

              while (!a <= List.length reslst ) do
                  a := !a + 1;
                    let string_of_command4 () =
                    let tmp_file = filescore in
                    let _ = Sys.command (Printf.sprintf "echo '%s' >> brevsets/sv_%i_x.txt" (List.hd !ll) !m)   in
                    let chan = open_in tmp_file in
                    close_in chan
                  in
                  string_of_command4();  
                  ll := List.tl !ll;
              done ;

      end  
    else
      begin
        Printf.printf "\nSigma Input is in contradiction with Sigma Core touistcode ==> %i (UNSAT)\n" !touistcode2; 
        (*
        Printf.printf "\nRev(Zbase, Zinput) : \n";  
        *)

        let tmp_file =  filescore in
        let l1 = ref [] in
              l1 := lines_from_files tmp_file; 

        let tmp_file =  filesvol in
        let l2 = ref [] in
              l2 := lines_from_files tmp_file; 

        Printf.printf "\n";    
        Printf.printf "+++++++++++++++++++++++++++++++";    
        Printf.printf "\n";    
        Printf.printf "Rev(Zbase(t), Zinput(t+1)) :\n";    
        List.iter(printf "%s\n") !l1;
        List.iter(printf "%s\n") !l2;
        (*Printf.printf "\n";*)

        Printf.printf "\n";    
        Printf.printf "+++++++++++++++++++++++++++++++";    
        Printf.printf "\nZcore (t+1) : \n";
        List.iter(printf "%s\n") !l1;

        Printf.printf "\nZvol (t+1) : \n";
        List.iter(printf "%s\n") !l2;

        exit 0;
      end;


let intersection l1 l2 = List.filter (fun x -> List.mem x l2) l1 in

let parse3ac() = 
    
    let lines = ref "" in
    let x = ref "" in
     
    let i = ref 1 in
    (* Here we pass the name of the file which contains the combinations dinamically*)
    let tmp_file = (Printf.sprintf "brevsets/sv_%i_x.txt" !m) in 
    let chan = open_in tmp_file in
    let chan2 = open_in tmp_file in


    let sc = open_in filescore in
    (*let si = open_in "brevdata/sinput.txt" in*)
    let si = open_in filesinput in

    

    let mcs = ref [] in
    let mcs2 = ref [] in
    let line = ref "" in

    let l1 = ref [] in
    let l2 = ref [] in






      let tmp_file =  filescore in
      let lx = ref [] in
           lx := lines_from_files tmp_file; 

      let rec difference l arg = match l with
      | [] -> []
      | x :: xs -> 
        if (x = arg) then difference xs arg
        else x :: difference xs arg in 

      let rec list_diff l1 l2 = match l2 with
      | [] -> l1
      | x :: xs -> list_diff (difference l1 x) xs in








    
    let j = ref 0 in
    let n = ref 0 in
    j := 0;
    (*Printf.printf ("Entro al : parse3ac() \n");*)
    try
      (* While there exist combinations, then evaluate if they are SAT*)
      while true do
        j := !j + 1;
        (*if (!lines != "") then lines := input_line chan ^" and "^ !lines;*)
        lines := input_line chan ^" and "^ !lines;
        line := input_line chan2;
        (*Printf.printf "LINEA ==>> %s \n" !line;*)

        let string_of_command4 () =
          let tmp_file = filescore in (*Este archivo se debe de generar mas arriba*)
          let _ = Sys.command (Printf.sprintf "echo '%s' >> brevsets/svxx_size_%i_seq_%i.txt" !line !m !i)   in
          let chan = open_in tmp_file in
          close_in chan
        in
        string_of_command4();  

        (*Printf.printf "Entro al : WHILE *** : %s \n" !lines;*)
        if !j = !m then  begin 


                        (* Is NOT necessary to merge all in one single file, because we can send a sequence of files to TouIST - But we are going to use for demostration - just to show the contain*)
                        let string_of_command5 () =
                                let tmp_file = filescore  in
                                (*let _ = Sys.command (Printf.sprintf "cat filescore brevsets/svxx_size_%i_seq_%i.txt brevdata/sinput.txt > brevsets/x_%i_%i.txt" !m !i !m !i)   in *)
                                let _ = Sys.command (Printf.sprintf "cat '%s' brevsets/svxx_size_%i_seq_%i.txt '%s' > brevsets/x_%i_%i.txt" filescore !m !i filesinput !m !i)   in
                                let chan = open_in tmp_file in
                                close_in chan
                              in
                              string_of_command5();
                          
                        Printf.printf "> Testing x_%i_%i.txt\n" !m !i;
                        (*touistcode2 := (command (Printf.sprintf "./main.exe brevsets/x_%i_%i.txt > brevsets/xs_%i_%i.txt" !m !i !m !i));*)
                        (*touistcode2 := (command (Printf.sprintf "(cat filescore; cat brevsets/svxx_size_%i_seq_%i.txt; cat brevdata/sinput.txt) | ./touist.exe --sat --solve - > brevsets/out.emodel_02.txt 2> brevsets/out.touisterr_02.txt" !m !i));*)



                        let string_of_command4 () =
                          let tmp_file = filescore in
                          (*let _ = Sys.command (Printf.sprintf "cat filescore brevsets/svxx_size_%i_seq_%i.txt brevdata/sinput.txt > brevsets/s_test2.txt" !m !i)   in*)
                          let _ = Sys.command (Printf.sprintf "cat '%s' brevsets/svxx_size_%i_seq_%i.txt '%s' > brevsets/s_test2.txt" filescore !m !i filesinput)   in
                          let chan = open_in tmp_file in
                          close_in chan
                        in
                        string_of_command4(); 
                    
                    
                        let string_of_command4 () =
                          let tmp_file = filescore in
                          let _ = Sys.command (Printf.sprintf "sed -i 's/$/ and/' brevsets/s_test2.txt" )   in
                          let chan = open_in tmp_file in
                          close_in chan
                        in
                        string_of_command4();
                    
                    
                        let string_of_command4 () =
                          let tmp_file = filescore in
                          let _ = Sys.command (Printf.sprintf "sed -i '$s/) and/)/' brevsets/s_test2.txt" )   in

                          let chan = open_in tmp_file in
                          close_in chan
                        in
                        string_of_command4();    



                        let string_of_command4 () =
                          let tmp_file = filescore in
                          let _ = Sys.command (Printf.sprintf "sed -i 's/\&/and/g' brevsets/s_test2.txt" )   in

                          let chan = open_in tmp_file in
                          close_in chan
                        in
                        string_of_command4();    


                        let string_of_command4 () =
                          let tmp_file = filescore in
                          let _ = Sys.command (Printf.sprintf "sed -i '1s/^/red (/' brevsets/s_test2.txt" )   in
                          let chan = open_in tmp_file in
                          close_in chan
                        in
                        string_of_command4();      
                    
                    
                        let string_of_command4 () =
                          let tmp_file = filescore in
                          let _ = Sys.command (Printf.sprintf "sed -i '$s/$/)/' brevsets/s_test2.txt")   in
                          let chan = open_in tmp_file in
                          close_in chan
                        in
                        string_of_command4();     


                        
                        let string_of_command4 () =
                          let tmp_file = filescore in
                          let _ = Sys.command (Printf.sprintf "sed -i '$s/ and)/)/' brevsets/s_test2.txt")   in
                          let chan = open_in tmp_file in
                          close_in chan
                        in
                        string_of_command4();                             




                        (*touistcode2 := (command (Printf.sprintf "(cat filescore; cat brevsets/svxx_size_%i_seq_%i.txt; cat brevdata/sinput.txt) | ./main.exe > brevsets/out.emodel_02.txt" !m !i));*)
                        (*touistcode2 := (command (Printf.sprintf "./main.exe brevsets/s_test2.txt > brevsets/out.emodel_02.txt"));*)
                        touistcode2 := (command (Printf.sprintf "%s brevsets/s_test2.txt > brevsets/out.emodel_02.txt" filetouist));

                        if !touistcode2 = 0 then 
                          begin
                            n := !n + 1;
                            Printf.printf "touistcode :  %i ==> saving x_%i_%i.txt in MCS[] \n\n" !touistcode2 !m !i;
                            mcs := !mcs @ ["brevsets/x_"^string_of_int !m^"_"^string_of_int !i^".txt"];
                            mcs2 := !mcs2 @ ["brevsets/svxx_size_"^string_of_int !m^"_seq_"^string_of_int !i^".txt"];

                            (*touistcode2 := (command (Printf.sprintf "cat brevsets/x_%i_%i.txt > brevsets/xss_%i.txt" !m !i !n));*)
                            (*exit 0;*)
                          end  
                          else
                               Printf.printf "touistcode :  %i \n\n" !touistcode2;                         
                            
                          i := 1 + !i;
                          j := 0;
                          lines := ""

                        end

      done;
    with End_of_file -> close_in chan; (* End of Try - Aca se terminan de enviar al Touist las Combinaciones *)

    if List.length !mcs > 0 then 
    begin

      if List.length !mcs = 1 then
      begin 
        (*List.iter(printf " ACA ES MCS-MCS-MCS  ::::::::::: %s \n") !mcs;*)
        (*List.iter(printf " MCS : %s \n") !mcs;*)
        List.iteri(printf "MCS[%i] : %s \n") !mcs;
        Printf.printf "\n";    

        let tmp_file = List.hd !mcs in

        (*let l1 = ref [] in*)
              l1 := lines_from_files tmp_file; 
(*
        Printf.printf "\nRev(Zbase, Zinput) : \n";    
        List.iter(printf "%s\n") !l1;
        Printf.printf "\n";
*)        
        (*exit 0;*)
      end;

      if List.length !mcs > 1 then
      (* Extract from MCS[] the firts 1 element, then compare it with the rest elements of the list MCS[] until no elements in MCS[]*)
      begin
        (*List.iteri(printf "MCSx %i : %s \n") !mcs;*)
        List.iteri(printf "MCS[%i] : %s \n") !mcs;
        j := List.length !mcs;
        i := 1;

        let lines1 = ref "" in 
        let lines2 = ref "" in 
        let tmp_file =  List.hd !mcs in
        (*let l1 = ref [] in*)
              l1 := lines_from_files tmp_file; 

        (*x1 := flatten (lines_from_files tmp_file); *)
        let tmp_fileb =  List.hd !mcs2 in
        let chan =  open_in tmp_fileb in    
        let x1 = ref "" in
              try
              while true do              
                lines1 := input_line chan ^" "^ !lines1;
              done;
              with End_of_file -> close_in chan;              

        while  !i < !j do
              (*let mcsr = (List.tl !mcs) in      *)
              let tmp_file2 =  List.hd (List.tl !mcs) in
              (*let l2 = ref [] in*)
                  l2 := lines_from_files tmp_file2;

              (*x1 := flatten (lines_from_files tmp_file); *)
              let tmp_fileb =  List.hd (List.tl !mcs2) in
              let chan =  open_in tmp_fileb in    
              let x2 = ref "" in
                    try
                    while true do              
                      lines2 := input_line chan ^" "^ !lines2;
                    done;
                    with End_of_file -> close_in chan;      

              l1 := intersection !l1 !l2;

              i := 1 + !i;
        done;
(*
        Printf.printf "\nRev(Zbase(t), Zinput(t+1)) : \n";    
        List.iter(printf "%s\n") !l1;
        Printf.printf "\n";
*)        
      end;

      Printf.printf "+++++++++++++++++++++++++++++++";    
      Printf.printf "\n";    
      Printf.printf "Rev(Zbase(t), Zinput(t+1)) : \n";    
      List.iter(printf "%s\n") !l1;
      Printf.printf "\n";

      (*let oc = open_out "planerdata/01_model.txt" in    (* create or truncate file, return channel *)*)
      let oc = open_out "../sdata/01_model.txt" in
      List.iter(fprintf oc "%s\n") !l1;
      close_out oc; 


                        let string_of_command4 () =
                          let tmp_file = filescore in
                          (*let _ = Sys.command (Printf.sprintf "sed -i 's/$/ and/' planerdata/01_model.txt" )   in*)
                          let _ = Sys.command (Printf.sprintf "sed -i 's/$/ and/' ../sdata/01_model.txt" )   in
                          let chan = open_in tmp_file in
                          close_in chan
                        in
                        string_of_command4();
                    
                    
                        let string_of_command4 () =
                          let tmp_file = filescore in
                          (*let _ = Sys.command (Printf.sprintf "sed -i '$s/ and//' planerdata/01_model.txt" )   in*)
                          let _ = Sys.command (Printf.sprintf "sed -i '$s/ and//' ../sdata/01_model.txt" )   in
                          let chan = open_in tmp_file in
                          close_in chan
                        in
                        string_of_command4();  



      Printf.printf "+++++++++++++++++++++++++++++++";    
      Printf.printf "\n";    
      Printf.printf "Zcore (t+1) : \n";
      List.iter(printf "%s\n") !lx;

      Printf.printf "\nZvol (t+1) : \n";
      List.iter(printf "%s\n") (list_diff !l1 !lx);

      let oc = open_out filesvol in    (* create or truncate file, return channel *)
      List.iter(fprintf oc "%s\n") (list_diff !l1 !lx);
      close_out oc; 


      let string_of_command4 () =
      let tmp_file = filescore in
      let _ = Sys.command (Printf.sprintf "sed -i 's/\&/and/g' %s" filesvol)   in

      let chan = open_in tmp_file in
              close_in chan
              in
      string_of_command4();    


(*********@@@ ****)

let string_of_command4 () =
  let tmp_file = filescore in
  
  let _ = Sys.command (Printf.sprintf "cat '%s' > '%s'" filesvol filesvol_final)   in
  let chan = open_in tmp_file in
          close_in chan
          in
  string_of_command4(); 


  let string_of_command4 () =
    let tmp_file = filescore in
    let _ = Sys.command (Printf.sprintf "sed -i 's/x_/ass(/g' %s" filesvol_final)   in

    let chan = open_in tmp_file in
            close_in chan
            in
    string_of_command4();    

  let string_of_command4 () =
      let tmp_file = filescore in
      let _ = Sys.command (Printf.sprintf "sed -i 's/_x/)/g' %s" filesvol_final)   in
  
      let chan = open_in tmp_file in
              close_in chan
              in
      string_of_command4();    

  let string_of_command4 () =
      let tmp_file = filescore in
      let _ = Sys.command (Printf.sprintf "sed -i 's/_/,/g' %s" filesvol_final)   in
  
      let chan = open_in tmp_file in
              close_in chan
              in
      string_of_command4();    

     (* sed -i '1s/^/<added text> /' file *)

  let string_of_command4 () =
      let tmp_file = filescore in
      let _ = Sys.command (Printf.sprintf "sed -i 's/^/\"/' %s" filesvol_final)   in
    
      let chan = open_in tmp_file in
              close_in chan
              in
      string_of_command4(); 
      
  let string_of_command4 () =
      let tmp_file = filescore in
      let _ = Sys.command (Printf.sprintf "sed -i 's/$/\"/' %s" filesvol_final)   in
      
      let chan = open_in tmp_file in
              close_in chan
              in
      string_of_command4();       


(*************)







      let string_of_command4 () =
      let tmp_file = filescore in
      let _ = Sys.command (Printf.sprintf "sed -i 's/\&/and/g' ../sdata/01_model.txt" )   in

      let chan = open_in tmp_file in
              close_in chan
              in
      string_of_command4();   





(*      
let reslst = flatten (extract !m (lines_from_files filesvol)) in
*)


      exit 0;    
    end;
    in
    parse3ac();

in

let pm = ref 0 in
  pm := List.length (lines_from_files (Printf.sprintf "%s" filesvol));
let px = ref 0 in

if !pm = 0 then
  begin
    Printf.printf "Sigma Vol ==> VACIO SIZE ::: \n\n";

    let string_of_command4 () =
    let tmp_file = filescore in
    
    let _ = Sys.command (Printf.sprintf "cat '%s' > '%s'" filesinput  filesvol)   in
    let chan = open_in tmp_file in
            close_in chan
            in
    string_of_command4(); 



    let string_of_command4 () =
      let tmp_file = filescore in
      
      let _ = Sys.command (Printf.sprintf "cat '%s' > '%s'" filesinput  filesvol_final)   in
      let chan = open_in tmp_file in
              close_in chan
              in
      string_of_command4(); 



(*********@@@ ****)

let string_of_command4 () =
  let tmp_file = filescore in
  
  let _ = Sys.command (Printf.sprintf "cat '%s' > '%s'" filesvol filesvol_final)   in
  let chan = open_in tmp_file in
          close_in chan
          in
  string_of_command4(); 


  let string_of_command4 () =
    let tmp_file = filescore in
    let _ = Sys.command (Printf.sprintf "sed -i 's/x_/ass(/g' %s" filesvol_final)   in

    let chan = open_in tmp_file in
            close_in chan
            in
    string_of_command4();    

  let string_of_command4 () =
      let tmp_file = filescore in
      let _ = Sys.command (Printf.sprintf "sed -i 's/_x/)/g' %s" filesvol_final)   in
  
      let chan = open_in tmp_file in
              close_in chan
              in
      string_of_command4();    

  let string_of_command4 () =
      let tmp_file = filescore in
      let _ = Sys.command (Printf.sprintf "sed -i 's/_/,/g' %s" filesvol_final)   in
  
      let chan = open_in tmp_file in
              close_in chan
              in
      string_of_command4();    

     (* sed -i '1s/^/<added text> /' file *)

  let string_of_command4 () =
      let tmp_file = filescore in
      let _ = Sys.command (Printf.sprintf "sed -i 's/^/\"/' %s" filesvol_final)   in
    
      let chan = open_in tmp_file in
              close_in chan
              in
      string_of_command4(); 
      
  let string_of_command4 () =
      let tmp_file = filescore in
      let _ = Sys.command (Printf.sprintf "sed -i 's/$/\"/' %s" filesvol_final)   in
      
      let chan = open_in tmp_file in
              close_in chan
              in
      string_of_command4();       


(*************)











    let string_of_command4 () =
    let tmp_file = filescore in
    let _ = Sys.command (Printf.sprintf "sed -i 's/\&/and/g' %s" filesvol)   in

    let chan = open_in tmp_file in
            close_in chan
             in
    string_of_command4();    




  end
else
  begin
    for p = 0 to (!pm - 1) do
      px := (!pm - p);
      plan px;
    done;
  end;



Printf.printf "Process completed ... \n";
exit 0;


end;

end