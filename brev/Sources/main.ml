let encoding_list = ["s";"m";"qbf-efa";"qbf-noop";"qbf-efa-nfla";"qbf-open";"sat";"sat-efa";"sat-open";"sat-efa-select";"smt-open";"smt-link"]
and solvers_list = ["depqbf"; "rareqs"; "caqe"; "qute"; "minisat"; "glucose"; "glucose-syrup"; "picosat"; "lingeling"]
and mode = ref "touistplan"
and encoding = ref "s"
and inputencoding = ref ""
and domain = ref ""
and problem = ref ""

and model = ref ""
and score = ref ""
and svol = ref ""
and sinput = ref ""
and actions = ref ""
and goal = ref ""



and constraints = ref ""
and gpminlevel = ref 0
and options = ref ""
and solver = ref "depqbf"
and incrmode = ref 1
and incmin = ref 1
and timeout = ref 0
and verbose = ref 0 (* 0 = not verbose *)
and count = ref None
and lint = ref false
and nodewidth = ref 1



let () =

  let returncode = (Sys.command "rm brevsets/*.*") in
  
  let usage = "\
Usage: "^Sys.argv.(0)^" [-e ENC] [-insat ENC_FILE] [-s SOLVER] SCORE SVOL SINPUT [-c CONSTR] [--timeout SECONDS]

This is the TouISTPLAN planner.

Note that the constraints file '-c CONSTR' is only usable with '-e sat-efa'.

MODEL: strips planning domain expressed in (typed) PDDL
ACTIONS: strips planning problem expressed in (typed) PDDL
GOAL: strips planning problem expressed in (typed) PDDL
"

  and argspecs = [ (* This list enumerates the different flags (-x,-f...)*)
    ("-e", Arg.Symbol (encoding_list, fun s -> encoding:=s), (
        ": TouISTPLAN encoding [default: "^ !encoding ^"]\n"^
        "\t- qbf-efa : QBFPLAN with Explanatory Frame-Axioms (default)\n"^
        "\t- qbf-noop : QBFPLAN with No-op Actions\n"^
        "\t- sat-efa : SATPLAN with Explanatory Frame-Axioms"));
    ("-insat", Arg.Set_string inputencoding,
     "ENC_FILE: SAT encoding text file (expressed in TouIST language)");
    ("-s", Arg.Symbol (solvers_list, fun s -> solver:=s), ": solver choice");
    ("-c", Arg.Set_string constraints,
     "CONSTR: strips planning constraints expressed in (typed) Extended-PDDL");
     
    ("-nodewidth", Arg.Set_int nodewidth,
     "m: select node width in QBF mode (m plan steps in each tree node)");

    ("-incr", Arg.Set_int incrmode,
     "2: double plan length at each increment with SAT/SMT encodings");

    ("-incmin", Arg.Set_int incmin,
     "2: double plan length at each increment with SAT/SMT encodings");

    ("--gpminlevel", Arg.Set_int gpminlevel,
     ("N: set the gpminlevel (mode (2) only) [default: "^ string_of_int !gpminlevel ^"]"));

    ("--timeout", Arg.Set_int timeout,
     ("SECONDS: search & extract are both given SECONDS to complete. 0=infinite \
      [default: "^ string_of_int !timeout ^"]"));

    ("-v", Arg.Unit (fun _ -> verbose := 1), ("a bit verbose"));
    ("-vv", Arg.Unit (fun _ -> verbose := 2), ("more verbose"));

    ("--options", Arg.Set_string options,
     ("OPTIONS: ??? [default: "^ !options ^"]"));

    ("--count", Arg.Int (fun i -> count := Some i),
     ("count the branch and node constraints"));
    ("--lint", Arg.Set lint,
     ("check that the syntax of the given domain/problem is correct"));
  ]

  (* The 'alone' arguments (not preceeded by a '--something') are going to be
     processed by this function in the order they appear. The first alone
     argument is interpreted as DOMAIN, the second is PROBLEM. *)
  and process_arg_alone arg =
    match !score, !svol, !sinput with
    | "", _, _  -> score := arg (*  *)
    | _, "", _ -> svol := arg
    | _, _, "" -> sinput := arg    
    | _, _, _  -> (Printf.eprintf "Usage: %s [opts] SCORE SVOL SINPUT (see --help).\n"
                  Sys.argv.(0); exit 1)



  in
  Arg.parse argspecs (process_arg_alone) usage; (* Parse command-line args *)
  Utils.begin_time := Utils.get_time ();

  (* Check that the user entered DOMAIN and PROBLEM *)
(*
  if !sinput = "" then
    (Printf.eprintf "Usage: %s [opts] INPUT (see --help).\n" Sys.argv.(0); exit 1);
*)    
  (* If -c CONSTR has been given, check that it is using 'sat-efa' *)

  if !score = "" || !svol = "" || !sinput = "" then
    (Printf.eprintf "Usage: %s [opts] SCORE SVOL SINPUT (see --help).\n" Sys.argv.(0); exit 1);  
  if !constraints <> "" && !encoding <> "sat-efa" then
    (Printf.eprintf "Usage: -c must be used with -e sat-efa (see --help).\n"; exit 1);

  if !incmin < 1 then
    (Printf.eprintf "Usage: -incmin N\n N must be greater than 0 (see --help).\n"; exit 1)
  else incmin := !incmin - 1;


  begin
  (*Printf.printf "ACA 002 \n";*)
  let solver_code = match !solver with
    | "depqbf" -> 0
    | "rareqs" -> 1
    | "caqe" -> 2
    | "qute" -> 3
    | "minisat" -> 0
    | "glucose" -> 101
    | "glucose-syrup" -> 102
    | "picosat" -> 103
    | "lingeling" -> 104
    | v -> failwith "solver unknown (tell the dev)"
  in
   
    match !encoding with
    | "s" ->  (*Printf.printf "ACA 005 \n";*)
                (*(new Planer.t !problem !domain !options 100 !inputencoding solver_code !nodewidth !incrmode !incmin !timeout !verbose)#search*)
                (new Brev.t !score !svol !sinput 100)#search

    | "m" ->  (*Printf.printf "ACA 005 \n";*)
                (*(new Planer.t !problem !domain !options 100 !inputencoding solver_code !nodewidth !incrmode !incmin !timeout !verbose)#search*)
                (new Brev.t !score !svol !sinput 200)#search                
    | _ -> failwith "encoding impossible (tell the dev)"
  end


