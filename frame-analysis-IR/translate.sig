signature TRANSLATE =
sig
  type exp
  eqtype level
  type access

  val dummy : exp
  val outermost: level
  val newLevel: {parent: level, name: Temp.label,
                 formals: bool list} -> level
  val formals: level -> access list
  val allocLocal: level -> bool -> access

  (* Helpers for translating to IR tree *)
  val simpleVar : access * level -> exp
  val subscriptVar : exp  * exp -> exp
  val fieldVar : exp * int -> exp
  val arithOp : Absyn.oper * exp * exp -> exp
  val relOp: Absyn.oper * exp * exp -> exp
  val ifThenElse: exp * exp * exp option -> exp
  val ifThen: exp * exp -> exp
  val funCall: Temp.label * exp list * level * level -> exp
  val whileExp: exp * exp -> exp
  val intExp: int -> exp
  val assignExp: exp * exp -> exp
  val emptySeq: unit -> exp
  val singleSeq: exp -> exp
  val seqExp: exp * exp -> exp
  val brkExp: Temp.label -> exp
  val stringExp: string -> exp
  val stringEquality: Absyn.oper * exp * exp -> exp
  val createArray: exp * exp -> exp
  val recordExp: exp list -> exp
  val insertDecs: exp list * exp -> exp
  val varDec: access * level * exp -> exp
  val forExp: level * access * Temp.label * exp * exp * exp -> exp

  val procEntryExit : {level: level, body: exp} -> unit
  structure Frame : FRAME
  val getResult : unit -> Frame.frag list
end
