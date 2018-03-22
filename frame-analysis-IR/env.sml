(* This is the environment that has the variable and type environments 
 *
 * These were modified for the frame analysis assignment to include
 * - The variable's access (frame/temp register) in VarEntry
 * - The function's level (information about it's frame) in FunEntry
 *
 * A helper function was added for the translation to IR phase
 *
 * inBaseVenv: Symbol.symbol -> bool
 * 
 * This is used to determine if a function was predefined or was later
 * defined by the user (shadowed). This information is needed to know
 * if translate should make an externalCall to the runtime or call the
 * user defined version
 *)
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
  val inBaseVenv: Symbol.symbol -> bool
end

structure Env :> ENV =
struct

type access = unit
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

fun inBaseVenv(s) =
        (case Symbol.look(base_venv, s)
           of SOME(FunEntry(_)) => true
            | _ => false)
end
