structure Translate =
struct

structure T = Tree

(* TODO: this is going to be changed? *)
(* type exp = unit  *)


(* Basic changes to Translate based on IR chapter *)
datatype exp = Ex of Tree.exp
	     | Nx of Tree.stm
	     | Cx of Temp.label * Temp.label -> Tree.stm

val dummy = Ex(T.CONST(0))
val error = Ex(T.CONST(0))
(* Signatures for un_x funcstions, and empty definitions *)
(*val unEx: Translate.exp -> Tree.exp
val unNx: Translate.exp -> Tree.stm
val unCx: Translate.exp -> (Temp.label * Temp.label -> Tree.stm) *)

fun unEx(Ex e) = e
  | unEx(Cx genstm) =
        let
            val r = Temp.newTemp()
            val t = Temp.newLabel() and f = Temp.newLabel()
        in
            T.ESEQ(T.SEQ(T.MOVE(T.TEMP r, T.CONST 1),
                        T.SEQ(genstm(t,f),
                        T.SEQ(T.LABEL f,
                        T.SEQ(T.MOVE(T.TEMP r, T.CONST 0),
                              T.LABEL t)))),
                    T.TEMP r)
        end
   | unEx(Nx s) = T.ESEQ(s, T.CONST 0)

fun unNx(Ex e) = T.EXP(e)
  | unNx(Nx s) = s
  | unNx(Cx genstm) =
       (* TODO: ??? *)
        let
            val t = Temp.newLabel() and f = Temp.newLabel()
        in
            genstm(t, f)
        end

fun unCx(Ex e) = (fn (t, f) => T.EXP(e))
  | unCx(Cx genstm) = genstm
  | unCx (Nx _) =
        let
            exception UnCxException
        in
            raise UnCxException
        end

(* thrown if allocLocal is called on the outermost level *)
exception OutermostException

(* a level contains a frame and a parent level.
   the outermost level is a special level (OUTERMOST) with no frame or parent *)
datatype level = OUTERMOST
               | Lev of {frame: Frame.frame, parent: level, unique: unit ref}


fun getParent(OUTERMOST) = raise OutermostException
  | getParent(Lev({frame, parent, unique})) = parent

fun getFrame(OUTERMOST) = raise OutermostException
  | getFrame(Lev({frame, parent, unique})) = frame

fun levelEq(OUTERMOST, OUTERMOST) = true
  | levelEq(Lev({frame=frame1, parent=parent1, unique=unique1}),
            Lev({frame=frame2, parent=parent2, unique=unique2})) = (unique1 = unique2)
  | levelEq(_, _) = false
(* Note this is different than Frame.access, also includes a level *)
type access = level * Frame.access


(* the outermost level *)
val outermost = OUTERMOST

(* Creates a new level with the given parent level, a name, and a list of booleans
   corresponding to the escapes for the formal parameters *)
fun newLevel({parent:level, name: Temp.label, formals:bool list}) =
    (* add on true to the formals list to represent the static link in the frame *)
    Lev({frame=Frame.newFrame({name=name, formals=true::formals}), parent=parent,
         unique=ref ()})

(* Return a list of Translate.accesses associated with the formal parameters of the given level *)
(* Removes the first formal parameter returned from Frame since this will correspond to the
   static link *)
fun formals(Lev({frame, parent, unique})) =
        map (fn (a) => (Lev({frame=frame, parent=parent, unique=unique}), a))
            (tl(Frame.formals(frame)))
  | formals(OUTERMOST) = []

(* Return a function that takes a boolean and returns the appropriate access for
   the new local variable *)
fun allocLocal(Lev({frame, parent, unique})) =
        (fn (b) => (Lev({frame=frame, parent=parent, unique=unique}),
                    Frame.allocLocal(frame)(b)))
  (* this should never occur, as the only things defined in the outermost level
     are the library functions *)
  | allocLocal(OUTERMOST) = raise OutermostException

(* TODO: do static links *)
fun followSL(curLev: level, targetLev: level) =
        if (levelEq(curLev, targetLev))
        then (T.TEMP Frame.FP)
        else T.MEM(followSL(getParent(curLev), targetLev))

fun simpleVar((l, access), level) = Ex(Frame.exp(access)(followSL(level, l)))

fun arrayVar((l, access), level, ex) =
    T.MEM(T.BINOP(T.PLUS,
                  (* This gives us the base address for the array var *)
                  Frame.exp(access)(followSL(level, l)),
                  T.BINOP(T.MUL, T.CONST Frame.wordSize, ex)))

end
