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
  val singleSeq: exp -> exp
  val seqExp: exp * exp -> exp
  val brkExp: Temp.label -> exp
  val stringExp: string -> exp
end
