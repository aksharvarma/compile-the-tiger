structure Translate =
struct

type exp = unit
type level = int
type access = level * Frame.access

val outermost = 0
(* TODO: reset to outermost at some point *)
val levelCount = ref outermost

(* table mapping level to frames *)
structure Table = IntMapTable(type key = level
                              fun getInt n = n)

val frameTable: Frame.frame Table.table ref = ref Table.empty

exception UnknownLevel
exception alloc

fun newLevel({parent:level, name: Temp.label, formals:bool list}) =
    (* add code here to set up static given parent info *)
    (levelCount := !levelCount + 1;
    frameTable := Table.enter(!frameTable, !levelCount,
                              Frame.newFrame({name=name, formals=true::formals}));
    !levelCount)

fun formals(level) =
    case Table.look(!frameTable, level)
        of NONE => raise UnknownLevel
        | SOME(f) => map (fn (a) => (level, a)) (tl(Frame.formals(f)))

fun allocLocal(level) =
    case Table.look(!frameTable, level)
        of NONE => raise alloc
        (* We return a bool->Translate.access function, taking in a bool
           and passing that to the function returned by Frame.allocLocal,
           yielding a Frame.access, which we can then return in the tuple *)
        | SOME(f) => (fn (b) => (level, Frame.allocLocal(f)(b)))

end
