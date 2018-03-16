structure Translate =
struct

structure T = Tree
structure A = Absyn

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

(* TODO: test static links *)
fun followSL(curLev: level, targetLev: level) =
        if (levelEq(curLev, targetLev))
        then (T.TEMP Frame.FP)
        else T.MEM(followSL(getParent(curLev), targetLev))

fun simpleVar((l, access), level) = Ex(Frame.exp(access)(followSL(level, l)))

(* TODO: check array out of bounds *)
fun subscriptVar(a, i) =
    Ex(T.MEM(T.BINOP(T.PLUS,
                  (* This gives us the base address for the array var *)
                  unEx(a),
                  (* Frame.exp(access)(followSL(level, l)), *)
                  T.BINOP(T.MUL, T.CONST Frame.wordSize, unEx(i)))))

(* TODO fix naming in these functions
   i is a number here but an exp above *)
fun fieldVar(a, i) = subscriptVar(a, Ex(T.CONST i))

fun arithOp(oper, left, right) =
    let
        exception ArithOpException
        val binop =
                (case oper
                    of A.PlusOp => T.PLUS
                     | A.MinusOp => T.MINUS
                     | A.TimesOp => T.MUL
                     | A.DivideOp => T.DIV
                     | _ => raise ArithOpException)
    in
        Ex(T.BINOP(binop, unEx(left), unEx(right)))
    end

fun relOp(oper, left, right) =
    let
        exception RelOpException
        val relop =
                (case oper
                    of A.EqOp => T.EQ
                     | A.NeqOp => T.NE
                     | A.LtOp => T.LT
                     | A.LeOp => T.LE
                     | A.GtOp => T.GT
                     | A.GeOp => T.GE
                     | _ => raise RelOpException)
    in
        Cx(fn(t,f) => T.CJUMP(relop, unEx(left), unEx(right), t, f))
    end

fun ifThenElse(test, Nx(thenStm), Nx(elseStm)) =
        let
            val z = Temp.newLabel()
            val t = Temp.newLabel() and f = Temp.newLabel()
        in
            Nx(T.SEQ(unCx(test)(t,f),
                     T.SEQ(T.LABEL t,
                     T.SEQ(thenStm,
                     T.SEQ(T.JUMP(T.NAME z, [z]),
                     T.SEQ(T.LABEL f,
                    T.SEQ(elseStm, T.LABEL z)))))))
        end
  | ifThenElse(test, Cx(thenFun), Cx(elseFun)) =
        let
            val r = Temp.newTemp()
            val z = Temp.newLabel() and join = Temp.newLabel()
            val t = Temp.newLabel() and f = Temp.newLabel()
        in
            Ex(T.ESEQ(T.SEQ(T.MOVE(T.TEMP r, T.CONST 0),
                            T.SEQ(unCx(test)(t,f),
                            T.SEQ(T.LABEL t,
                            T.SEQ(thenFun(z, join),
                            T.SEQ(T.LABEL z,
                            T.SEQ(T.MOVE(T.TEMP r, T.CONST 1),
                            T.SEQ(T.JUMP(T.NAME join, [join]),
                            T.SEQ(T.LABEL f,
                            T.SEQ(elseFun(z, join),
                            T.LABEL join))))))))), T.TEMP r))
        end
  | ifThenElse(test, Cx(thenFun), Ex(elseExp)) =
        let
            val r = Temp.newTemp()
            val z = Temp.newLabel() and join = Temp.newLabel()
            val t = Temp.newLabel() and f = Temp.newLabel()
        in
            Ex(T.ESEQ(T.SEQ(T.MOVE(T.TEMP r, T.CONST 0),
                            T.SEQ(unCx(test)(t,f),
                            T.SEQ(T.LABEL t,
                            T.SEQ(thenFun(z, join),
                            T.SEQ(T.LABEL z,
                            T.SEQ(T.MOVE(T.TEMP r, T.CONST 1),
                            T.SEQ(T.JUMP(T.NAME join, [join]),
                            T.SEQ(T.LABEL f,
                            T.SEQ(T.MOVE(T.TEMP r, elseExp),
                            T.LABEL join))))))))), T.TEMP r))

        end
  | ifThenElse(test, Ex(thenExp), Cx(elseFun)) =
        let
            val r = Temp.newTemp()
            val z = Temp.newLabel() and join = Temp.newLabel()
            val t = Temp.newLabel() and f = Temp.newLabel()
        in
            Ex(T.ESEQ(T.SEQ(T.MOVE(T.TEMP r, T.CONST 0),
                            T.SEQ(unCx(test)(t,f),
                            T.SEQ(T.LABEL t,
                            T.SEQ(T.MOVE(T.TEMP r, thenExp),
                            T.SEQ(T.JUMP(T.NAME join, [join]),
                            T.SEQ(T.LABEL f,
                            T.SEQ(elseFun(z, join),
                            T.SEQ(T.LABEL z,
                            T.SEQ(T.MOVE(T.TEMP r, T.CONST 1),
                            T.LABEL join))))))))), T.TEMP r))
        end
  | ifThenElse(test, Ex(thenExp), Ex(elseExp)) =
        let
            val r = Temp.newTemp()
            val z = Temp.newLabel()
            val t = Temp.newLabel() and f = Temp.newLabel()
        in
            Ex(T.ESEQ(T.SEQ(unCx(test)(t,f),
                                      T.SEQ(T.LABEL t,
                                      T.SEQ(T.MOVE(T.TEMP r, thenExp),
                                      T.SEQ(T.JUMP(T.NAME z, [z]),
                                      T.SEQ(T.LABEL f,
                                      T.SEQ(T.MOVE(T.TEMP r, elseExp),
                                            T.LABEL z)))))), T.TEMP r))
        end
  | ifThenElse(_) =
        let exception IfThenElseNxMismatch in raise IfThenElseNxMismatch end

fun ifThen(test, Nx(thenStm)) =
        let
            val t = Temp.newLabel() and f = Temp.newLabel()
        in
            Nx(T.SEQ(unCx(test)(t,f),
                     T.SEQ(T.LABEL t,
                     T.SEQ(thenStm,
                     T.LABEL f))))
        end
  | ifThen(test, Cx(thenFun)) =
        let exception CxInIfThenException in raise CxInIfThenException end
  | ifThen(test, Ex(thenExp)) =
        let exception ExInIfThenException in raise ExInIfThenException end

fun funCall(label, argExps, curLev, targetLev, isProcedure) =
    let
        val sl = followSL(curLev, targetLev)
        val argList = map unEx argExps
        val result = T.CALL(T.NAME label, sl::argList)
    in
        if(isProcedure)
        then Nx(T.EXP result)
        else Ex(result)
    end

fun whileExp(test, body) =
    let
        val testLabel = Temp.newLabel() and done = Temp.newLabel()
        and fallThrough = Temp.newLabel()
    in
        Nx(T.SEQ(T.LABEL testLabel,
                 T.SEQ(unCx(test)(fallThrough, done),
                 T.SEQ(T.LABEL fallThrough,
                 T.SEQ(T.EXP(unEx(body)),
                 T.SEQ(T.JUMP(T.NAME testLabel, [testLabel]),
                 T.LABEL done))))))
    end
end
