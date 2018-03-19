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

val dummy = Ex(T.NAME(Temp.namedLabel("dummy")))
val error = Ex(T.NAME(Temp.namedLabel("error")))

val fragList: Frame.frag list ref = ref []
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
        let
            val t = Temp.newLabel() and f = Temp.newLabel()
        in
            T.SEQ(genstm(t,f), T.SEQ(T.LABEL f, T.LABEL t))
        end

fun unCx(Ex(T.CONST 0)) = (fn (t, f) => T.JUMP(T.NAME f, [f]))
  | unCx(Ex(T.CONST 1)) = (fn (t, f) => T.JUMP(T.NAME t, [t]))
  | unCx(Ex e) = (fn (t, f) => T.CJUMP(T.NE, e, T.CONST 0, t, f))
  | unCx(Cx genstm) = genstm
  | unCx (Nx _) =
        let
            exception UnCxException
        in
            raise UnCxException
        end

(* thrown if allocLocal is called on the outermost level *)
exception OutermostException


fun printExp(e, msg) = (print(msg ^ "\n"); Printtree.printtree(TextIO.stdOut, unNx(e)); print("-------------\n"))

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
fun subscriptVar(a, indexExp) =
    Ex(T.MEM(T.BINOP(T.PLUS,
                  (* This gives us the base address for the array var *)
                  unEx(a),
                  (* Frame.exp(access)(followSL(level, l)), *)
                  T.BINOP(T.MUL, T.CONST Frame.wordSize, unEx(indexExp)))))

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
(* final parameter is a boolean that is true if comparing strings *)
fun relOp(oper, left, right, false) =
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
  | relOp(oper, left, right, true) =
    let
        exception RelOpException
        val fc =
                (case oper
                     of A.LtOp => {func="stringLt", res=1}
                     | A.LeOp => {func="stringLte", res=1}
                     | A.GtOp => {func="stringLte", res=0}
                     | A.GeOp => {func="stringLt", res=0}
                     | _ => raise RelOpException)
    in
       (* might need to pass static link to external call ? *)
        Cx(fn(t,f) => T.CJUMP(T.EQ, Frame.externalCall(#func fc,
                                 [unEx(left), unEx(right)]), T.CONST (#res fc), t, f))
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

fun funCall(inVenv, name, label, argExps, curLev, targetLev, isProcedure) =
    let
        val sl = followSL(curLev, targetLev)
        val argList = map unEx argExps
        val result =
                if inVenv
                then Frame.externalCall(Symbol.name(name), argList)
                else T.CALL(T.NAME label, sl::argList)
    in
        if(isProcedure)
        then Nx(T.EXP result)
        else Ex(result)
    end

fun whileExp(test, body, done) =
    let
        val testLabel = Temp.newLabel() and bodyLabel = Temp.newLabel()
    in
        Nx(T.SEQ(T.JUMP(T.NAME testLabel, [testLabel]),
                 T.SEQ(T.LABEL bodyLabel,
                 T.SEQ(unNx(body),
                 T.SEQ(T.LABEL testLabel,
                 T.SEQ(unCx(test)(bodyLabel, done), T.LABEL done))))))
    end

(* also used for nil exp *)
fun intExp(i) = Ex(T.CONST i)

fun assignExp(left, right) = Nx(T.MOVE(unEx(left), unEx(right)))

fun emptySeq() = Nx(T.EXP(T.CONST 0))

fun singleSeq(fst) = fst

(* last parameter is true if the result type of the sequence is unit *)
fun seqExp(fst, rest, false) = Ex(T.ESEQ(unNx(fst), unEx(rest)))
  | seqExp(fst, rest, true) = Nx(T.SEQ(unNx(fst), unNx(rest)))

fun brkExp(brkLabel) = Nx(T.JUMP(T.NAME brkLabel, [brkLabel]))

fun stringExp(s, pos) =
    let
      val lab = Temp.newLabel()
    in
      (fragList := Frame.STRING(lab, s) :: !fragList;
       Ex(T.NAME lab))
    end

fun stringEquality(oper, left, right) =
        let
            exception StringEquality
            val compRes = (case oper
                          of A.EqOp => T.CONST 1
                           | A.NeqOp => T.CONST 0
                           | _ => raise StringEquality)
        in
            Cx(fn(t,f) => T.CJUMP(T.EQ, Frame.externalCall("stringEqual",
                                     [unEx(left), unEx(right)]), compRes, t, f))
        end

fun createArray(size, init) = Ex(Frame.externalCall("initArray",
                                 [unEx(size), unEx(init)]))

fun recordExp(fieldList:exp list) =
        let
            val r = Temp.newTemp()
            fun initFields([], r, i) = T.EXP (T.CONST 0)
              | initFields(ex::[], r, i) =
                    T.MOVE(T.MEM(T.BINOP(T.PLUS, T.TEMP r,
                                         T.CONST(i * Frame.wordSize))), unEx(ex))
              | initFields(ex::exs, r, i) =
                    T.SEQ(T.MOVE(T.MEM(T.BINOP(T.PLUS, T.TEMP r,
                                         T.CONST(i * Frame.wordSize))), unEx(ex)),
                          initFields(exs, r, i + 1))
        in
            Ex(T.ESEQ(T.SEQ(T.MOVE(T.TEMP r, Frame.externalCall("malloc",
                                 [T.CONST (List.length(fieldList) * Frame.wordSize)])),
                            initFields(fieldList, r, 0)),
                      T.TEMP r))
        end

fun insertHelper([]) = let exception NeverHappens in raise NeverHappens end
  | insertHelper(d::[]) = unNx(d)
  | insertHelper(d::ds) = T.SEQ(unNx(d), insertHelper(ds))

fun insertDecs([], body, _) = body
  | insertDecs(dec::decs, body, false) =
        Ex(T.ESEQ(insertHelper(dec::decs), unEx(body)))
  | insertDecs(dec::decs, body, true) =
        Nx(T.SEQ(insertHelper(dec::decs), unNx(body)))

fun varDec(access:access, level, init:exp) =
        assignExp(simpleVar(access, level), init)

fun forExp(level, iAccess:access, brkLabel, lo:exp, hi:exp, body:exp) =
    let
        val limitAccess = allocLocal(level)(false)
        val i = simpleVar(iAccess, level)
        val iPlus1 = Ex(T.BINOP(T.PLUS, unEx(i), T.CONST 1))
        val newBody = Nx(T.SEQ(unNx(body), unNx(varDec(iAccess, level, iPlus1))))
        val test = relOp(A.LeOp, i, simpleVar(limitAccess, level), false)
    in
        Nx(T.SEQ(unNx(varDec(iAccess, level, lo)),
           T.SEQ(unNx(varDec(limitAccess, level, hi)),
                 unNx(whileExp(test, newBody, brkLabel)))))
    end

fun procEntryExit({level, body}) =
    let
        val frame = getFrame(level)
        (* items 6-7 *)
        val bodyWithRV = T.MOVE(T.TEMP Frame.RV, unEx(body))
        (* procEntryExit1 gives us items 4-8, items 1-3 and 9-11 will be done
           later by procEntryExit3 (or 2?) *)
        val modifiedBody = Frame.procEntryExit1(frame, bodyWithRV)
    in
        fragList := Frame.PROC({body=modifiedBody, frame=frame}):: !fragList
    end

fun getResult() = !fragList

fun printFrag(Frame.PROC({body, frame})) = (Frame.printFrame(frame);
                                            printExp(Nx(body), "frag body"))
  | printFrag(Frame.STRING(label, str)) = (printExp(Nx(T.LABEL label),
                                           "string label: " ^ str))

fun printInfo() = (print("-----------Info-------------\n");
                   printExp(Ex(T.TEMP Frame.FP), "Frame pointer");
                   printExp(Ex(T.TEMP Frame.RV), "RV");
                   print("---------frags------\n");
                   (app printFrag (getResult())))

fun reset() = fragList := []

end
