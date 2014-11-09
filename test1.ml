open Arg
open Printf
open Cil

module F = Frontc
module C = Cil
module CK = Check
module E = Errormsg

let functionality1FixInstr (i : instr) : bool =
  match i with
  | Set((Var vi, NoOffset), _, loc)
      when vi.vname = "deleted" && vi.vglob ->
    E.log "%a: Deleted assignment: %a\n" d_loc loc d_instr i;
    false
  | _ -> true


let rec functionality1FixStmt (s : stmt) : unit =
  match s.skind with
  | Instr il ->
    s.skind <- Instr(List.filter functionality1FixInstr il)
  | If(_,tb,fb,_) ->
    functionality1FixBlock tb;
    functionality1FixBlock fb
  | Switch(_,b,_,_) ->
    functionality1FixBlock b
  | Loop(b,_,_,_) ->
    functionality1FixBlock b
  | Block b ->
    functionality1FixBlock b
  | TryFinally(b1, b2, _) ->
    functionality1FixBlock b1;
    functionality1FixBlock b2
  | TryExcept(b1,_,b2,_) ->
    functionality1FixBlock b1;
    functionality1FixBlock b2

  | _ -> ()

and functionality1FixBlock (b : block) : unit = List.iter functionality1FixStmt b.bstmts

let functionality1FixFunction (fd : fundec) : unit = functionality1FixBlock fd.sbody

(*
Looks for a function called target;
If finds the function that filter out assignments to a global variable called deleted
*)
let functionality1 (f : file) : unit =
  List.iter (fun g ->
    match g with
    | GFun (fd, loc) when fd.svar.vname = "target" ->
      functionality1FixFunction fd
    | _ -> ())
  f.globals

