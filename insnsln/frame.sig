signature FRAME =
sig
  type frame
  type access
  val newFrame: {name: Temp.label, formals: bool list} -> frame
  val name: frame -> Temp.label
  val formals: frame -> access list
  val allocLocal: frame -> bool -> access
  val FP: Temp.temp
  val SP: Temp.temp
  val RV : Temp.temp (* as seen by callee *)
  val wordSize: int
  val exp: access -> Tree.exp -> Tree.exp
  datatype frag = PROC of {body: Tree.stm, frame: frame}
                | STRING of Temp.label * string
  val externalCall: string * Tree.exp list -> Tree.exp
  val procEntryExit1: frame * Tree.stm -> Tree.stm
  val procEntryExit2: frame * Assem.instr list -> Assem.instr list
  val procEntryExit3: frame * Assem.instr list -> {prolog:string, 
                                                   body: Assem.instr list,
                                                   epilog:string}
  
  val string: Temp.label * string -> string
                                            
  (* The following two functions are to facilitate debugging *)
  val printAccess: access -> unit
  val printFrame: frame -> unit

  type register = string
  val tempMap:register Temp.Table.table
                       
end

