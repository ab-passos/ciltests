open Arg
open Printf
open Cil

module F = Frontc
module C = Cil
module CK = Check
module E = Errormsg

(** Vrs is a reference or a global var flag to show version *)
let vrs = ref false
(** Prt is a reference flag of console print mode activated *)
let prt = ref false
(** Fn is a reference var of source filename (include directory) *)
let fn = ref ""
(** Fd is a reference var of destination filename (include directory) *)
let fd = ref "a.out"
(** Fe is a reference var for a destination filename that will be recorded as CIL**)
let fe = ref "a.cil.c"
 

(** This is a function to set source filename on global var Fn *)
let setsource f = fn:=f;;
(** This is a function to set destination filename on reference var Fd *)
let setdestination f = fd:=f;;
(** Set the destination file for the CIL saving, the default one is a.cil.c **)
let setCILdestinationFile f = fe:=f;;


(** A list of arguments (comand line menu) *)
let arglist = [
	("-v", Arg.Set vrs, "Version of FormalTest - Automatic test generator");
	("-p", Arg.Set prt, "Print Mode");
	("-s", Arg.String(setsource), "Source");
	("-d", Arg.String(setdestination), "Target Filename");
	("-c", Arg.String(setCILdestinationFile), "Target Filename to save as CIL")
	];;

(** This is a usage mensage of aacomp *)
let usage_msg = "usage of FormalTest: [-vct] -s <source> -d <destination>";;

(*

	let file = Sys.argv.(1)
	let cin = open_in file
*)

let parseOneFile (fname: string) : C.file =
  (* PARSE and convert to CIL *)
  if !Cilutil.printStages then ignore (E.log "Parsing %s\n" fname);
  let cil = F.parse fname () in
  
  if (not !Epicenter.doEpicenter) then (
    (* sm: remove unused temps to cut down on gcc warnings  *)
    (* (Stats.time "usedVar" Rmtmps.removeUnusedTemps cil);  *)
    (* (trace "sm" (dprintf "removing unused temporaries\n")); *)
    (Rmtmps.removeUnusedTemps cil)
  );
  cil


(*The main of the automatic test generator*)
(*Example of usage:
	/main -s a.txt -d b.txt
*)
let _ =
	Arg.parse arglist (fun s -> ()) usage_msg;
	if !vrs then print_endline "Version 0.1 Alpha";
	if !prt then print_endline "Print Mode Activated ...";
	if !fn<>"" then print_endline ("Source file: "^(!fn)) else print_endline "No source File.";
	if !fd<>"a.out" then print_endline ("Destination file: "^(!fd));
	if !fe<>"a.cil.c" then print_endline ("Destination file: "^(!fe)); 

	try
	let lexbuf = (open_in (!fn)) in
	begin
		Cil.initCIL();
		let resParsed = parseOneFile !fn in 
		let oc = open_out !fe in
		(*let lwVisitor = new logWriteVisitor in 
			C.visitCilFileSameGlobals lwVisitor oc;*)
		(*C.saveBinaryFileChannel resParsed oc;*)
		C.dumpFile (!C.printerForMaincil) oc !fe resParsed;
		print_endline "Opened with sucess";
                Test1.functionality1 resParsed;
		(*C.visitCilFileSameGlobals (vvdec vname) resParsed;*)
		(*print_endline resParsed.fileName;*)
	end
	with Sys_error (e) -> (print_endline("Could not open the file - " ^ (!fn)))
;;
