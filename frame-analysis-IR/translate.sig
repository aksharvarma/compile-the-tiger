signature TRANSLATE =
sig

  structure Frame : FRAME

  type frag                     (* is Frame.frag *)
  type exp                      (* Ex, Nx, Cx *)
  type level
  type access                   (* Different from Frame.access *)

  val nop : exp                 (* placeholder expression *)
  val outermost: level

  (* Called when a new function called *)
  val newLevel: {parent: level,
                 name: Temp.label,
                 formals: bool list} -> level
  val formals: level -> access list
  (* Allocates local variables. Uses Frame.allocLocal *)
  val allocLocal: level -> bool -> access

  (* Functions called by Semant for translating to IR tree *)
  val simpleVar : access * level -> exp
  val subscriptVar : exp  * exp -> exp
  val fieldVar : exp * int -> exp
  val arithOp : Absyn.oper * exp * exp -> exp
  val relOp: Absyn.oper * exp * exp * bool -> exp
  val ifThen: exp * exp -> exp
  val ifThenElse: exp * exp * exp -> exp
  val funCall: Temp.label * exp list * level * level * bool -> exp
  val libCall: Symbol.symbol * exp list * bool -> exp
  val whileExp: exp * exp * Temp.label -> exp
  val intExp: int -> exp
  val assignExp: exp * exp -> exp
  val emptySeq: unit -> exp
  val singleSeq: exp -> exp
  val seqExp: exp * exp * bool -> exp
  val brkExp: Temp.label -> exp
  val stringExp: string -> exp
  val stringEquality: Absyn.oper * exp * exp -> exp
  val createArray: exp * exp -> exp
  val recordExp: exp list -> exp
  val insertDecs: exp list * exp * bool -> exp
  val varDec: access * level * exp -> exp
  val forExp: level * access * Temp.label * exp * exp * exp -> exp
  val procEntryExit : {level: level,
                       body: exp,
                       isProcedure: bool,
                       isMain: bool} -> unit

  (* This is the final result that is used. *)
  val getResult : unit -> frag list

  (* Used for debugging *)
  val printInfo: unit -> unit

  (* To ensure that frag list and stringTable are reset *)
  val reset: unit -> unit
end
