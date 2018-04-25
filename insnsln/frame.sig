signature FRAME =
sig
  type frame
  type register = string
  type access

  val RV: Temp.temp
  val FP: Temp.temp
  val SP: Temp.temp
  val zero: Temp.temp

  val wordSize: int

  val newFrame: {name: Temp.label, formals: bool list} -> frame
  val formals: frame -> access list
  val name: frame -> Temp.label
  val setOutgoingArgs: frame * int -> unit
  val allocLocal: frame -> bool -> access

  val string: Temp.label * string -> string
  val externalCall: string * Tree.exp list -> Tree.exp
  val exp: access -> Tree.exp -> Tree.exp

  val tempMap: register Temp.Table.table
  val findTemp: string -> Temp.temp
  val trashedByCall: Temp.temp list

  val procEntryExit1: frame * Tree.stm -> Tree.stm
  val procEntryExit2: frame * Assem.instr list -> Assem.instr list
  val procEntryExit3: frame * Assem.instr list -> {prolog:string,
                                                   body: Assem.instr list,
                                                   epilog:string}

  datatype frag = PROC of {body: Tree.stm, frame: frame}
                | STRING of Temp.label * string

  (* The following function is to facilitate debugging *)
  val printFrame: frame -> unit
end

