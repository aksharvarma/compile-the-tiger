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
  val arrayVar : access * level * exp -> exp
end
