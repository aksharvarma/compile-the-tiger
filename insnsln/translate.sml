(* The Translate module does all the heavylifting and interfacing,
 * allowing Semant to interact with both the MipsFrame module
 * (for frame analysis) and Tree (for AST to IR translation) module.
 *)

structure Translate:>TRANSLATE =
struct

(* Shortening of common modules *)
structure T = Tree
structure A = Absyn

(* These are wrappers around Tree stms and exps. *)
datatype exp = Ex of Tree.exp
             | Nx of Tree.stm
             | Cx of Temp.label * Temp.label -> Tree.stm

(* A level contains a frame and a parent level and a unit ref for comparison.
 * The outermost level is a special level (OUTERMOST) with no frame or parent
 *)
datatype level = OUTERMOST
               | Lev of {frame: Frame.frame,
                         parent: level, unique: unit ref}

(* Note this is different than Frame.access. It includes a level *)
type access = level * Frame.access

(* Thrown if allocLocal is called on the outermost level
 * Shouldn't happen.
 *)
exception OutermostException

(* Placeholder expression to continue type-checking *)
val nop = Ex(T.CONST 0)

(* the outermost level *)
val outermost = OUTERMOST

(* newLevel: {parent: level, name: Temp.label, formals: bool list} -> level
 *
 * Create a new level with the given parent level, a name, and booleans
 * corresponding to the escapes for the formal parameters *)
fun newLevel({parent:level, name: Temp.label, formals:bool list}) =
    (* SL is added as an escaping parameter to the formals list *)
    Lev({frame=Frame.newFrame({name=name, formals=true::formals}),
         parent=parent,
         unique=ref ()})

(* formals: level -> access list
 *
 * Return list of Translate.access for the formals of the given level
 *)
fun formals(Lev({frame, parent, unique})) =
    map (fn (a) => (Lev({frame=frame, parent=parent, unique=unique}), a))
        (tl(Frame.formals(frame))) (* Drop SL (first param) *)
  | formals(OUTERMOST) = []

(* allocLocal: level -> bool -> access
 *
 * Return a function that takes a boolean and returns the Translate.access
 * for the new local variable; Wraps over the namesake Frame.allocLocal
 *)
fun allocLocal(Lev({frame, parent, unique})) =
    (fn (b) => (Lev({frame=frame, parent=parent, unique=unique}),
                Frame.allocLocal(frame)(b)))
  (* This should never occur, because the only things in the outermost
   * level are the library functions.
   *)
  | allocLocal(OUTERMOST) = raise OutermostException

(* A few global (yet internal) variables follow *)

(* Two named labels for runtime errors *)
val derefNil = Temp.namedLabel("dereference_NIL")
val outOfBounds = Temp.namedLabel("outOfBounds")

(* The list of Frame.frag (=Translate.frag) *)
val fragList: Frame.frag list ref = ref []

(* Keeps a hash table of strings encountered until now
 * This is used to ensure that the same string doesn't get two labels
 *)
val stringTable:Temp.label Symbol.table ref = ref Symbol.empty

(* These functions help extract inner IR representation in the form we want *)

(* unEx: Translate.exp -> Tree.exp
 *
 * Standard implementation from the book
 *)
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

(* unNx: Translate.exp -> Tree.stm  *)
fun unNx(Ex e) = T.EXP(e)
  | unNx(Nx s) = s
  | unNx(Cx genstm) =
    let
      val t = Temp.newLabel() and f = Temp.newLabel()
    in
      T.SEQ(genstm(t,f), T.SEQ(T.LABEL t, T.LABEL f))
    end

(* unCx: Translate.exp -> (Temp.label * Temp.label -> Tree.stm)
 *
 * The unCx for 0 and 1 help to make AND/OR translation cleaner
 *)
fun unCx(Ex(T.CONST 0)) = (fn (t, f) => T.JUMP(T.NAME f, [f]))
  | unCx(Ex(T.CONST 1)) = (fn (t, f) => T.JUMP(T.NAME t, [t]))
  | unCx(Ex e) = (fn (t, f) => T.CJUMP(T.NE, e, T.CONST 0, t, f))
  | unCx(Cx genstm) = genstm
  | unCx (Nx _) =
    let                         (* As mentioned in the book *)
      exception UnCxException
    in
      raise UnCxException
    end

(* followSl: level * level -> Tree.exp
 *
 * This is the function that follows the SL to reach required level.
 * curLev is current level and targetLev is the one whose FP you want
 *
 * Since we store SL at FP+0, we can simply recurse with T.MEM()
 * We keep recursing until we reach the correct level at which point
 * we use the FP to finish our expression to find the correct address
 *
 * Note: Our way of doing things (and result) differs from Appel's way
 * Our result is his with all inner SL-offsets to be 0.
 *)
fun followSL(curLev: level, targetLev: level) =
    let
      (* Returns the parent level *)
      fun getParent(OUTERMOST) = raise OutermostException
        | getParent(Lev({frame, parent, unique})) = parent

      (* Compares two level for equality using the unit ref in them *)
      fun levelEq(OUTERMOST, OUTERMOST) = true
        | levelEq(Lev({frame=frame1, parent=parent1, unique=unique1}),
                  Lev({frame=frame2, parent=parent2, unique=unique2})) = (unique1 = unique2)
        | levelEq(_, _) = false

    in
      (* Recursively follow levels up until you hit the right one *)
      if (levelEq(curLev, targetLev))
      then (T.TEMP Frame.FP)
      else T.MEM(followSL(getParent(curLev), targetLev))
    end

(* simpleVar : access * level -> exp
 *
 * Uses the followSL function to get the correct address for the var
 * Note: Translate.access = (level, Frame.acesss) is the 1st arg.
 *)
fun simpleVar((l, access), level) = Ex(Frame.exp(access)(followSL(level, l)))

(* subscriptVar : exp  * exp -> exp
 *
 * We perform array bound checking by storing the size of the array in
 * the base memory address. This means we offset our index by 1.
 * Using this size, we jump to an error label if the index is out of bounds
 *)
fun subscriptVar(a, indexExp) =
    let
      val r = Temp.newTemp() val t = Temp.newTemp()
      val z = Temp.newLabel() and x = Temp.newLabel()
      val varTree = unEx(a)
      val indexTree = unEx(indexExp)
    in
      Ex(T.ESEQ(T.SEQ(T.MOVE(T.TEMP r, varTree),
                T.SEQ(T.MOVE(T.TEMP t, indexTree),
                T.SEQ(T.CJUMP(T.GE, T.TEMP t, T.MEM(T.TEMP r),
                              outOfBounds, z),
                T.SEQ(T.LABEL z,
                T.SEQ(T.CJUMP(T.LT, T.TEMP t, T.CONST 0,
                              outOfBounds, x),
                      T.LABEL x))))),
                T.MEM(T.BINOP(T.PLUS,
                              (* This is the base *)
                              T.TEMP r,
                              (* offset index by 1 *)
                              T.BINOP(T.MUL,
                                      T.CONST Frame.wordSize,
                                      T.BINOP(T.PLUS, T.TEMP t,
                                              T.CONST 1))))))
    end

(* fieldVar : exp * int -> exp
 *
 * We check for nil deferences here. Since nil is set to 0, we simply
 * add a check to ensure that we are not dereferencing 0.
 * If we are, we jump to the derefNil error label.
 *)
fun fieldVar(a, i) =
    let
      val r = Temp.newTemp()
      val z = Temp.newLabel()
    in
      Ex(T.ESEQ(T.SEQ(T.MOVE(T.TEMP r, unEx(a)),
                T.SEQ(T.CJUMP(T.EQ, T.TEMP r, T.CONST 0, derefNil, z),
                T.LABEL z)),
                T.MEM(T.BINOP(T.PLUS,
                              T.TEMP r,
                              T.BINOP(T.MUL,
                                      T.CONST Frame.wordSize,
                                      T.CONST i)))))
    end

(* arithOp : Absyn.oper * exp * exp -> exp *)
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

(* relOp: Absyn.oper * exp * exp * bool -> exp
 *
 * The relOp is different for strings and for other types. The final
 * parameter is a boolean denoting if we are comparing strings
 *)
fun relOp(oper, left, right, false) = (* Not comparing strings *)
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
  (* Comparing strings is done using functions defined in the runtime
   * environment. Since lexicographical comparison is not available
   * otherwise, we assume that we will add 2 functions to help do that
   * to the runtime: stringLt and stringLte.
   * Then we simply make externalCalls to the right function.
   *)
  | relOp(oper, left, right, true) = (* Compare strings *)
    let
      exception RelOpException
      val fc =
          (case oper            (* We use externalCall *)
            of A.LtOp => {func="stringLt", res=1}
             | A.LeOp => {func="stringLte", res=1}
             | A.GtOp => {func="stringLte", res=0}
             | A.GeOp => {func="stringLt", res=0}
             | _ => raise RelOpException)
    in
      Cx(fn(t,f) => T.CJUMP(T.EQ,
                            Frame.externalCall(#func fc,
                                               [unEx(left),
                                                unEx(right)]),
                            T.CONST (#res fc), t, f))
    end

(* ifThen: exp * exp -> exp
 *
 * The cases without the else is easy. It'll contain Nx and return Nx.
 * Raise exceptions in other cases (won't happen because we type-check)
 *)
fun ifThen(test, Nx(thenStm)) =
    let
      val t = Temp.newLabel() and f = Temp.newLabel()
    in
      Nx(T.SEQ(unCx(test)(t,f),
         T.SEQ(T.LABEL t,
         T.SEQ(thenStm,
         T.LABEL f))))
    end
  (* The following two should never happen *)
  | ifThen(test, Cx(thenFun)) =
    let exception CxInIfThenException in raise CxInIfThenException end
  | ifThen(test, Ex(thenExp)) =
    let exception ExInIfThenException in raise ExInIfThenException end


(* ifThenElse: exp * exp * exp -> exp
 *
 * To make the IR code efficient the cases are handled differently.
 * Depending on the kinds of exp's in the branches, we produce the
 * most appropriate kind of exp for the if/then/else as a whole.
 * We explain each case separately as well.
 *)

(* This is the simplest case. Do the test and jump to the right place, ending
 * at the newly created label z.
 * Both branches are statements, so the whole if/then/else should also be
 * statements (Nx)
 *)
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
  (* Both branches are Cx's and should be evaluated for control, not value,
   * and thus the whole if/then/else should also be evaluated for control (Cx).
   * If the test statement is true, then we want to use the then branch as the
   * real test. Similarly, if false, we want to use the else branch as the real
   * test.
   *)
  | ifThenElse(test, Cx(thenFun), Cx(elseFun)) =
    let
      val x = Temp.newLabel() and y = Temp.newLabel()
    in
      Cx(fn (t,f) => T.SEQ(unCx(test)(x,y),
                     T.SEQ(T.LABEL x,
                     T.SEQ(thenFun(t, f),
                     T.SEQ(T.LABEL y,
                           elseFun(t, f))))))
    end
  (* When the then branch contains a Cx and the else branch contains 0,
   * this is equivalent to an AND. We want to specifically optimize this
   * case to evaluate for control, not value. This produces optimal jumps
   * for the AND case, and also still will produce the correct result if
   * this expression later needed to be evaluate for value (by unExing
   * this Cx). Also note that unCx of 0 is treated specially.
   *)
  | ifThenElse(test, Cx(thenFun), Ex(T.CONST 0)) =
    let
      val x = Temp.newLabel() and y = Temp.newLabel()
    in
      Cx(fn (t,f) =>  T.SEQ(unCx(test)(x,y),
                            T.SEQ(T.LABEL x,
                            T.SEQ(thenFun(t, f),
                            T.SEQ(T.LABEL y,
                            unCx(Ex(T.CONST 0))(t,f))))))
    end
  (* When the then branch contains a Cx and the else branch contains
   * an arbitrary expression, the if/then/else is assumed to be used
   * for value rather than control. Producing an Ex is necessary here
   * to preserve the value in the else expression.
   * To do this, while still evaluating the Cx in then expression for
   * control, we perform the test, and if it is true, we evaluate the
   * then test and move the correct value (0 or 1) into a temp r and
   * jump to the end. Else, if the original test was false, we move
   * the result of the else expression into the temp r. At the end,
   * r is then the result of the expression.
   *)
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
  (* When the then branch contains 1 and the else branch contains a Cx,
   * this is equivalent to an OR. We want to specifically optimize this
   * case to evaluate for control, not value. This produces optimal jumps
   * for the OR case, and also still will produce the correct result if
   * this expression later needed to be evaluate for value (by unExing
   * this Cx). Also note that unCx of 1 is treated specially.
   *)
  | ifThenElse(test, Ex(T.CONST 1), Cx(elseFun)) =
    let
      val x = Temp.newLabel() and y = Temp.newLabel()
    in
      Cx(fn (t,f) => T.SEQ(unCx(test)(x,y),
                     T.SEQ(T.LABEL x,
                     T.SEQ(unCx(Ex(T.CONST 1))(t,f),
                     T.SEQ(T.LABEL y,
                     elseFun(t, f))))))
    end
  (* When the then branch contains an arbitrary expression and the else
   * branch contains a Cx, the if/then/else is assumed to be used
   * for value rather than control. Producing an Ex is necessary here
   * to preserve the value in the then expression.
   * To do this, while still evaluating the Cx in else expression for
   * control, we perform the test, and if it is true, we just move
   * the result of the then expression into a temp r. If false, we
   * evaluate the else test and move the correct value (0 or 1) into a temp r.
   * At the end, r is then the result of the expression.
   *)
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
  (* This is a simple case in which both branches are expressions,
   * and the if/then/else is assumed to be used for value.
   * Simply moving the results of the appropriate expression
   * into a new temp r and making that the result is all that is
   * needed here
   *)
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

  (* The remaining cases are type-errors and will be caught by the
   * type checker, raise exception.
   *)
  | ifThenElse(_) =
    let exception IfThenElseNxMismatch in raise IfThenElseNxMismatch end

(* funCall: Temp.label * exp list * level * level * bool -> exp
 *
 * The levels are the current level and the level used to find SL to pass
 * The isProcedure bool is used to determine whether a result should
 * be expected or not and behave differently.
 *)
fun funCall(label, argExps, curLev, targetLev, isProcedure) =
    let
      val sl = followSL(curLev, targetLev)
      val argList = map unEx argExps
      val result = T.CALL(T.NAME label, sl::argList)
    in
      if(isProcedure)
      then Nx(T.EXP result)     (* Produce Nx if return type UNIT *)
      else Ex(result)
    end

(* libCall: Symbol.symbol * exp list * bool -> exp
 *
 * This is to make calls to predefined/libarary functions.
 * They need to use externalCall as Tiger's predefined functions are
 * all written in the C runtime.
 *
 * This also means that we don't need to find SL. However, we still
 * check if it is a procedure or not.
 *)
fun libCall(name, argExps, isProcedure) =
    let
      val argList = map unEx argExps
      val result = Frame.externalCall(Symbol.name(name), argList)
    in
      if(isProcedure)
      then Nx(T.EXP result)
      else Ex(result)
    end

(* whileExp: exp * exp * Temp.label -> exp
 *
 * We use the version with the test at the end of the body.
 *)
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

(* intExp: int -> exp
 *
 * This is also used by nilExp because we assume that nil is just 0.
 *)
fun intExp(i) = Ex(T.CONST i)

(* assignExp: exp * exp -> exp
 *
 * This is also used in Vardec and gets called there.
 *)
fun assignExp(left, right) = Nx(T.MOVE(unEx(left), unEx(right)))

(* emptySeq: unit -> exp
 *
 * Essentially nop, but as an Nx, not an Ex.
 *)
fun emptySeq() = Nx(T.EXP(T.CONST 0))

(* singleSeq: exp -> exp *)
fun singleSeq(fst) = fst

(* seqExp: exp * exp * bool -> exp
 *
 * The bool tells us whether result is UNIT or not.
 * We use that to decide between Nx and Ex.
 *)
fun seqExp(fst, rest, false) = Ex(T.ESEQ(unNx(fst), unEx(rest)))
  | seqExp(fst, rest, true) = Nx(T.SEQ(unNx(fst), unNx(rest)))

(* brkExp: Temp.label -> exp *)
fun brkExp(brkLabel) = Nx(T.JUMP(T.NAME brkLabel, [brkLabel]))

(* stringExp: string -> exp
 *
 * Most of the work with strings will be done later on, so we only need
 * to add them to the frag list for now.
 *
 * However, we do test if we've seen the string before.
 * If so, we don't make a new entry in the frag list.
 * We simply find its existing label and use that.
 *)
fun stringExp(s) =
    let
      val lab =
          (* check if already seen, if so use the same label,
           if not generate a newLabel and add to the string table *)
          (case Symbol.look(!stringTable, Symbol.symbolize(s))
            of SOME(l) => l
             | NONE => let
               val newLab = Temp.newLabel()
             in
               (stringTable := Symbol.enter(!stringTable,
                                            Symbol.symbolize(s),
                                            newLab);
                fragList := Frame.STRING(newLab, s) :: !fragList;
                newLab)
             end)
    in
      Ex(T.NAME lab)
    end

(* stringEquality: Absyn.oper * exp * exp -> exp
 *
 * Simply make an externalCall to the stringEqual function.
 *)
fun stringEquality(oper, left, right) =
    let
      exception StringEquality
      val compRes = (case oper
                      of A.EqOp => T.CONST 1
                       | A.NeqOp => T.CONST 0
                       | _ => raise StringEquality)
    in
      Cx(fn(t,f) => T.CJUMP(T.EQ, Frame.externalCall("stringEqual",
                                                     [unEx(left),
                                                      unEx(right)]),
                            compRes, t, f))
    end

(* createArray: exp * exp -> exp
 *
 * Simple external call to initArray
 *)
fun createArray(size, init) =
    Ex(Frame.externalCall("initArray", [unEx(size), unEx(init)]))

(* recordExp: exp list -> exp
 *
 * This needs a call to malloc to allocate space.
 * Then each of the fields' addresses is added to the allocated space
 *)
fun recordExp(fieldList:exp list) =
    let
      val r = Temp.newTemp()

      (* Initialize fields *)
      fun initFields([], r, i) = T.EXP (T.CONST 0)
        | initFields(ex::[], r, i) =
          T.MOVE(T.MEM(T.BINOP(T.PLUS, T.TEMP r,
                               T.CONST(i * Frame.wordSize))), unEx(ex))
        | initFields(ex::exs, r, i) =
          T.SEQ(T.MOVE(T.MEM(T.BINOP(T.PLUS, T.TEMP r,
                                     T.CONST(i * Frame.wordSize))), unEx(ex)),
                initFields(exs, r, i + 1))
    in
      Ex(T.ESEQ(T.SEQ(T.MOVE(T.TEMP r,
                             Frame.externalCall("malloc", [T.CONST (List.length(fieldList) * Frame.wordSize)])),
                      initFields(fieldList, r, 0)),
                T.TEMP r))
    end

(* insertHelper: exp list -> Tree.stm
 *
 * Internal helper function for insertDecs
 *)
fun insertHelper([]) = let exception NeverHappens in raise NeverHappens end
  | insertHelper(d::[]) = unNx(d)
  | insertHelper(d::ds) = T.SEQ(unNx(d), insertHelper(ds))

(* insertDecs: exp list * exp * bool -> exp
 *
 * The bool is whether the body unit or not. Use Nx if it is.
 *)
fun insertDecs([], body, _) = body
  | insertDecs(dec::decs, body, false) =
    Ex(T.ESEQ(insertHelper(dec::decs), unEx(body)))
  | insertDecs(dec::decs, body, true) =
    Nx(T.SEQ(insertHelper(dec::decs), unNx(body)))

(* varDec: access * level * exp -> exp
 *
 * A varDec is simply an assignExp with appropriate expressions.
 *)
fun varDec(access:access, level, init:exp) =
    assignExp(simpleVar(access, level), init)

(* forExp: level * access * Temp.label * exp * exp * exp -> exp
 *
 * We incorporate the suggestion by Appel in the book so as to allow
 * hi to be maxint. This needs multiple tests and jumps but ensures
 * that the resultant code is correct.
 *)
fun forExp(level, iAccess:access, brkLabel, lo:exp, hi:exp, body:exp) =
    let
      val limitAccess = allocLocal(level)(false)
      val i = simpleVar(iAccess, level)
      val iPlus1 = Ex(T.BINOP(T.PLUS, unEx(i), T.CONST 1))
      val testLe = relOp(A.LeOp, i, simpleVar(limitAccess, level), false)
      val testLt = relOp(A.LtOp, i, simpleVar(limitAccess, level), false)
      val bodyLabel = Temp.newLabel() and incLabel = Temp.newLabel()
    in
      Nx(T.SEQ(unNx(varDec(iAccess, level, lo)),
         T.SEQ(unNx(varDec(limitAccess, level, hi)),
         T.SEQ(unCx(testLe)(bodyLabel, brkLabel),
         T.SEQ((T.LABEL incLabel),
         T.SEQ(unNx(varDec(iAccess, level, iPlus1)),
         T.SEQ((T.LABEL bodyLabel),
         T.SEQ(unNx(body),
         T.SEQ(unCx(testLt)(incLabel, brkLabel),
               T.LABEL brkLabel)))))))))
    end


(* procEntryExit: {level:level, body:exp, isProcedure:bool, isMain:bool} -> unit
 *
 * This is the function that calls the Frame.procEntryExit1 function
 * Based on the list of items from the book, we don't do these yet:
 * 1-3, 9-11,
 * This functions combines 6 and 7 (move result to RV)
 * 4, 5, 8 are machine dependent and are done by the Frame module.
 *)
fun procEntryExit({level, body, isProcedure, isMain}) =
    let

      (* If Main function, need to append these labels to frag *)
      fun appendErrorLabels(e) =
          let
            val done = Temp.namedLabel("DONE")
          in T.SEQ(e,
             T.SEQ(T.JUMP(T.NAME done, [done]),
             T.SEQ(T.LABEL derefNil,
             T.SEQ(unNx(libCall(Symbol.symbolize("print"),
                   [stringExp("Attempted to deref a nil record")], true)),
             T.SEQ(T.JUMP(T.NAME done, [done]),
             T.SEQ(T.LABEL outOfBounds,
             T.SEQ(unNx(libCall(Symbol.symbolize("print"),
                   [stringExp("Index out of bounds exception")], true)),
             T.LABEL done)))))))
          end

      (* Helper function to get the frame from level *)
      fun getFrame(OUTERMOST) = raise OutermostException
        | getFrame(Lev({frame, parent, unique})) = frame

      val frame = getFrame(level)
      (* items 6-7: Move result of exp into RV
       * But do that only if it is not a procedure
       *)
      val bodyWithRV = if isProcedure
                       then unNx(body)
                       else T.MOVE(T.TEMP Frame.RV, unEx(body))

      (* procEntryExit1 modifies the body to do items 4-8. *)
      val modifiedBody = Frame.procEntryExit1(frame, bodyWithRV)

      (* If isMain, then append runtime error labels to the end *)
      val finalBody = if isMain
                      then appendErrorLabels(modifiedBody)
                      else modifiedBody
    in
      (* Each function becomes a fragment *)
      fragList := Frame.PROC({body=finalBody, frame=frame}):: !fragList
    end

(* getResult : unit -> frag list *)
fun getResult() = !fragList


(* printInfo: unit -> unit
 *
 * Just a debugging function that prints FP, RV, and the fraglist.
 *)
fun printInfo() =
    let
      (* Helper for printFrag *)
      fun printExp(e, msg) =
          (print(msg ^ "\n");
           Printtree.printtree(TextIO.stdOut, unNx(e));
           print("-------------\n"))

      (* Actual helper for printInfo *)
      fun printFrag(Frame.PROC({body, frame})) =
          (Frame.printFrame(frame);
           printExp(Nx(body), "frag body"))
        | printFrag(Frame.STRING(label, str)) =
          (printExp(Nx(T.LABEL label),
                    "string label: " ^ str))
    in
      (print("-----------Info-------------\n");
       printExp(Ex(T.TEMP Frame.FP), "Frame pointer");
       printExp(Ex(T.TEMP Frame.RV), "RV");
       print("---------frags------\n");
       (app printFrag (getResult())))
    end

(* reset: unit -> unit *)
fun reset() = (fragList := []; stringTable := Symbol.empty)

end
