signature ENV =
sig
  type access
  (* type ty *)
  datatype enventry = VarEntry of {access: Translate.access, ty: Types.ty}
		   | FunEntry of {level: Translate.level,
                                  label: Temp.label,
                                  formals: Types.ty list,
                                  result: Types.ty}
  val base_tenv: Types.ty Symbol.table
  val base_venv: enventry Symbol.table
end

structure Env :> ENV =
struct

type access = unit
(* type ty = Types.ty *)
datatype enventry = VarEntry of {access: Translate.access, ty:Types.ty}
		  | FunEntry of {level: Translate.level,
                                  label: Temp.label,
                                  formals: Types.ty list,
                                  result: Types.ty}

val base_tenv = Symbol.enter(Symbol.enter(Symbol.empty,
					  Symbol.symbolize("int"),
					  Types.INT),
			     Symbol.symbolize("string"), Types.STRING)
local
val base_venv = Symbol.enter(Symbol.empty,
			     Symbol.symbolize("print"),
			     FunEntry({level= Translate.outermost,
                                       label= Temp.newLabel(),
                                       formals=[Types.STRING],
                                       result=Types.UNIT}))
val base_venv = Symbol.enter(base_venv,
			     Symbol.symbolize("flush"),
			     FunEntry({level= Translate.outermost,
                                       label= Temp.newLabel(),
                                       formals=[],
				       result=Types.UNIT}))
val base_venv = Symbol.enter(base_venv,
			     Symbol.symbolize("getchar"),
			     FunEntry({level= Translate.outermost,
                                       label= Temp.newLabel(),
                                       formals=[],
				       result=Types.STRING}))
val base_venv = Symbol.enter(base_venv,
			     Symbol.symbolize("ord"),
			     FunEntry({level=Translate.outermost,
                                       label=Temp.newLabel(),
                                       formals=[Types.STRING],
				       result=Types.INT}))
val base_venv = Symbol.enter(base_venv,
			     Symbol.symbolize("chr"),
			     FunEntry({level= Translate.outermost,
                                       label= Temp.newLabel(),
                                       formals=[Types.INT],
				       result=Types.STRING}))
val base_venv = Symbol.enter(base_venv,
			     Symbol.symbolize("size"),
			     FunEntry({level= Translate.outermost,
                                       label= Temp.newLabel(),
                                       formals=[Types.STRING],
				       result=Types.INT}))
val base_venv = Symbol.enter(base_venv,
			     Symbol.symbolize("substring"),
			     FunEntry({level= Translate.outermost,
                                       label= Temp.newLabel(),
                                       formals=[Types.STRING, Types.INT, Types.INT],
				       result=Types.STRING}))
val base_venv = Symbol.enter(base_venv,
			     Symbol.symbolize("concat"),
			     FunEntry({level= Translate.outermost,
                                       label= Temp.newLabel(),
                                       formals=[Types.STRING, Types.STRING],
				       result=Types.STRING}))
val base_venv = Symbol.enter(base_venv,
			     Symbol.symbolize("not"),
			     FunEntry({level= Translate.outermost,
                                       label= Temp.newLabel(),
                                       formals=[Types.INT],
				       result=Types.INT}))
in
    val base_venv = Symbol.enter(base_venv,
                    	     Symbol.symbolize("exit"),
    			     FunEntry({level= Translate.outermost,
                                       label= Temp.newLabel(),
                                       formals=[Types.INT],
				       result=Types.UNIT}))
end
end
