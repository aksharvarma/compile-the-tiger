signature FRAME =
sig
  type frame
  type register = string
  type access

  val RV: Temp.temp
  val FP: Temp.temp
  val SP: Temp.temp

  val wordSize: int

  val newFrame: {name: Temp.label, formals: bool list} -> frame
  val formals: frame -> access list
  val name: frame -> Temp.label
  val allocLocal: frame -> bool -> access

  val string: Temp.label * string -> string
  val externalCall: string * Tree.exp list -> Tree.exp
  val exp: access -> Tree.exp -> Tree.exp
  val getOffset: access -> int

  val tempMap: register Temp.Table.table
  val findTemp: string -> Temp.temp
  val tempToString: register Temp.Table.table -> Temp.temp -> string
  val physicalRegsT: Temp.temp list
  val registers: register list
  val trashedByCall: Temp.temp list
  val K: int

  val procEntryExit1: frame * Tree.stm -> Tree.stm
  val procEntryExit2: frame * Assem.instr list -> Assem.instr list
  val procEntryExit3: frame * Assem.instr list -> {prolog:string,
                                                   body: Assem.instr list,
                                                   epilog:string}

  datatype frag = PROC of {body: Tree.stm, frame: frame}
                | STRING of Temp.label * string

  (* The following two functions are to facilitate debugging *)
  val printAccess: access -> unit
  val printFrame: frame -> unit
end

