(* The Semant module
 *
 * - uses FindEscape to perform escape analysis
 * - Does the type checking
 * - Simultaneously makes calls to Translate functions to translate the
 *   AST into the IR tree.
 *
 * The actual code here is only for static semantic analysis including
 * type checking and verifying positions of breaks.
 * Everything else is done in its own module and details/comments are
 * found in their respective files.
 *)

(* Shorten commonly used structures *)
structure A = Absyn
structure Ty = Types
structure S = Symbol
structure E = Env

(* Main semantic analysis module *)
signature SEMANT =
sig
  (* static variable environment, table maps symbols to Env.enventry *)
  type venv = E.enventry S.table
  (* static type environment, table maps symbols to Ty.ty *)
  type tenv = Ty.ty S.table

  (* expty: a translated expression and its type
   * this is what transExp essentially computes
   *)
  type expty = {exp: Translate.exp, ty: Ty.ty}

  (* main semantic analysis entry point for a tiger program *)
  val transProg : A.exp -> unit
end

structure Semant :> SEMANT =
struct
type venv = E.enventry S.table
type tenv = Ty.ty S.table
type expty = {exp: Translate.exp, ty: Ty.ty}

(* Convenience: shorten subtype checking *)
val istype = Ty.isSubtype

(* Tracks whether at least one error has occurred *)
val errorExists = ref false

(* Tracks whether we are currently in a scope that allows breaks *)
val brNesting = ref 0

(* Helper methods *)

(* throwUp: int * string -> unit
 *
 * Sets error flag and outputs error message
 * Note since only the starting positions are stored in the AST
 * the error messages will have the same pos twice rather than
 * an interval. The initial pos should still be accurate.
 *)
fun throwUp(pos, msg) = (errorExists := true;
                         ErrorMsg.error(pos, pos, msg))

(* Takes in a list of record fields (from Ty.RECORD) and finds the
 * field with the given name if it exists
 *)
and findField([], id:S.symbol) = NONE
  | findField((x:S.symbol, ty:Ty.ty)::xs, id:S.symbol) =
    if (id=x) then SOME(actualTy(ty)) else findField(xs, id)

and getIndex([], id:S.symbol, counter) = ~1
  | getIndex((x:S.symbol, ty:Ty.ty)::xs, id:S.symbol, counter) =
    if (id=x) then counter else getIndex(xs, id, counter + 1)

and checkFieldAndRemove([], (id:S.symbol, typ:Ty.ty, pos)) =
    (throwUp(pos, "Field "^S.name(id)^" doesn't exit in record");
     [])
  | checkFieldAndRemove((x:S.symbol, ty:Ty.ty)::xs,
                        (id:S.symbol, typ:Ty.ty, pos)) =
    if (id = x)
    then if (istype(actualTy(typ), actualTy(ty)))
         then xs
         else (throwUp(pos, "Field "^S.name(x)^" has incorrect type");
               xs)
    else checkFieldAndRemove(xs, (id, typ, pos))

(* Finds the actual/concrete type of a given type.
   If the given type is a NAME continue looking past all NAMES
   until a concrete type is found.

   Note: Due to the way NAME types are constructed,
         there should be no NAME types remaining with NONE in them. *)
and actualTy(Ty.NAME(sym, ty)) =
    (case !ty
      of SOME(t) => actualTy(t)
       (* Should never occur, but if it does,
          print an error and return BOTTOM to try to continue *)
       | NONE => (throwUp(0,"Undefined type:"^S.name(sym)^".\n");
                  Ty.BOTTOM))
  | actualTy(a) = a

(* Helper for adding headers to the tenv for tydecs.
   Essentially accomplishes the first pass over the tydecs. *)
and addTypeHeads(tenv, []) = tenv
  | addTypeHeads(tenv, {name, ty, pos}::tydecs) =
    addTypeHeads(S.enter(tenv, name, Ty.NAME(name, ref NONE)), tydecs)

(* Accumulates the fields of a RecordTy into a (name, ty) list *)
and accRecord([], tenv) = []
  | accRecord({name, escape, typ, pos}::fields, tenv) =
    (* Look up the typ of the field in the tenv *)
    case (S.look(tenv, typ))
           (* If not found, print an error and continue recurring *)
     of NONE => (throwUp(pos, "Type of field: "^S.name(name)^" is not defined");
                 accRecord(fields, tenv))
      (* If found, add (name, t) to the list and recur *)
      | SOME(t) => (name, t)::accRecord(fields, tenv)

(* The main semantic analysis happens here.
 * transExp translates/type checks an expression given venv and tenv.
 * transExp is curried to take in the venv and tenv first and return
 * a function taking in the expression to analyze since many recursive
 * calls should happen in the same environments as were given initially *)
fun transExp(venv:venv, tenv:tenv, level:Translate.level, brkLabel) : (A.exp -> expty) =
    let
      (* Many more helper methods *)

      (* Checks whether the given expression is of the target type.
       * If not, print the given error message.
       *)
      fun check(ty:Ty.ty, targetType:Ty.ty, pos, msg) =
          if (not (istype(ty, actualTy(targetType))))
          then throwUp(pos, msg)
          else ()

      (* Takes a list of expressions and a list of types and
       * checks whether the expressions in the first list
       * have the same types as the actual types of the
       * types in the second list (order matters).
       * This helper is useful for checking whether the
       * arguments used to call a function have the correct types
       *)
      and checkTypeList([], []) = true (* we're good if both empty *)
        (* if exactly one is empty, something's wrong *)
        | checkTypeList([], _) = false
        | checkTypeList(_, []) = false
        (* check 1st entry and then recur *)
        | checkTypeList(e::es, t::ts) =
          #ty(e) = actualTy(t) andalso checkTypeList(es, ts)

      (* Verifies whether the given list of record field declarations
       * match the types of the record fields looked up from the tenv
       *)
      and verifyFields([], [], pos) = () (* both empty => we're good *)
        | verifyFields([], _, pos) =
          throwUp(pos,"Given fields don't match fields for record type: too few fields")
        | verifyFields(_, [], pos) =
          throwUp(pos,"Given fields don't match fields for record type: too many fields")
        | verifyFields((sym:S.symbol, typ:Ty.ty, pos)::fields,
                       recordFields, pos2) =
          verifyFields(fields,
                       checkFieldAndRemove(recordFields,
                                           (sym, typ, pos)),
                       pos2)

      (* trvar: Translate/type-check variables *)
      and trvar(A.SimpleVar(id, pos)) =
          (* look for the id in the venv *)
          (case S.look(venv, id)
                      (* if a var entry is found, return the actualTy of that entry *)
            of SOME(E.VarEntry({access, ty})) => {exp=Translate.simpleVar(access, level), ty=actualTy(ty)}
             (* if not, print an error and return BOTTOM to keep going *)
             | _ => (throwUp(pos, "Undefined variable:" ^ S.name(id));
                     {exp=Translate.nop, ty=Types.BOTTOM}))

        (* Field vars for accessing records *)
        | trvar(A.FieldVar(var, id, pos)) =
          let
            val varRes = trvar(var)
          in
            (* determine the type of the LHS variable *)
            (case (#ty varRes)
              of Ty.RECORD(sym, ty) =>
                 (* if it's a record, then search for the field with
                  * the given name *)
                 (case findField(sym, id)
                   (* if we didn't find it,
                    * print an error and return BOTTOM to continue *)
                   of NONE => (throwUp(pos, "Invalid field id:" ^ S.name(id));
                               {exp=Translate.nop, ty=Ty.BOTTOM})
                    (* if we did find it,
                     * then return the type of that field *)
                    | SOME(t) =>
                      {exp=Translate.fieldVar(#exp varRes,
                                              getIndex(sym, id, 0)),
                       ty=t})
               (* if the LHS wasn't a record, then print error
                * and return BOTTOM to continue *)
               | _ => (throwUp(pos, "accessing field of non-record:"
                                    ^S.name(id));
                       {exp=Translate.nop, ty=Ty.BOTTOM}))
          end

        (* Subscript vars for accessing arrays *)
        | trvar(A.SubscriptVar(var, e, pos)) =
          (* determine the type of the LHS variable *)
          (let
            val varRes = trvar(var)
          in
            case (#ty varRes)
             (* if it's an array, check that the expression is an int
              * and return the type of that array *)
             of Ty.ARRAY(typ, _) =>
                let
                  val subRes = trexp(e)
                in
                  (check(#ty subRes, Ty.INT, pos,
                         "Non integer subscript.");
                   {exp=Translate.subscriptVar(#exp varRes, #exp subRes),
                    ty=actualTy(typ)})
                end
              (* if it's not an array, print error, return BOTTOM *)
              | _ => (throwUp(pos, "Subscripting non-array variable.");
                      {exp=Translate.nop, ty=Ty.BOTTOM})
          end)
      (* trvar ENDS *)

      (* trTy: Translate/type-check type declarations *)
      (* essentially accomplishes the second pass over the mutually
       * recursive type definitions *)
      and trTy({name, ty, pos}, tenv) =
          ((case S.look(tenv, name)
             (* If it's there and a NAME type, case on ty
              * and mutate the type in the NAME accordingly *)
             of SOME(Ty.NAME(s, typ))  =>
                (case ty
                  (* Look up the type of the ref type in the NAME
                   * and set the ref to be that looked up type *)
                  of A.NameTy(sym, pos) => typ := S.look(tenv, sym)
                   (* Set the type of the NAME to be a new record type *)
                   | A.RecordTy(fields) =>
                     typ := SOME(Ty.RECORD(accRecord(fields, tenv), ref ()))
                   (* Look up the type of the ref type in the NAME
                    * and set the ref to be a new array type of
                    * that looked up type *)
                   | A.ArrayTy(sym, pos) =>
                     (case S.look(tenv, sym)
                       of SOME(t) => (typ := SOME(Ty.ARRAY(t, ref ())))
                        | NONE => throwUp(pos, "Type for array not found.")))
              (* This shouldn't happen since we add all the headers
               * as NAMEs in the first pass, but if it does,
               * just return the current tenv *)
              | _ => ());
           (* Return the updated tenv *)
           tenv)

      (* trDecs: Translate/type-check all declarations.
       * Returns an updated venv and tenv *)
      and trDecs(venv:venv, tenv:tenv, [], level:Translate.level,
                 brkLabel, varDecList) =
          (venv, tenv, level, brkLabel, varDecList)
        | trDecs(venv:venv, tenv:tenv, dec::decs, level,
                 brkLabel, varDecList) =
          (case dec of
               (* normal variables *)
               A.VarDec({name, escape, typ, init, pos}) =>
               let
                 val initRes = transExp(venv, tenv, level, brkLabel) init
                 (* Determine the type of the init expr *)
                 val initType = #ty(initRes)
                 (* If type explicitly specified, use that.
                  * Handles things like var b:rectype := nil
                  * where we want RECORD and not NIL as the type *)
                 val typeToUse:Ty.ty ref = ref initType
                 val newAccess = Translate.allocLocal(level)(!escape)
               in
                 ((case typ
                    (* If the type of the variable was specified,
                     * look it up in the tenv *)
                    of SOME(ty, pos2) =>
                       (case S.look(tenv, ty)
                         (* If it was found in the tenv, print an
                          * error if it is not equal to the type of
                          * the init expr *)
                         of SOME(t) =>
                            (if (not(istype(initType, actualTy(t))))
                             then throwUp(pos2, "Initial expression doesn't match provided type:"^S.name(ty))
                             else ();
                             (* If type is provided, use that. *)
                             typeToUse:=actualTy(t))
                          (* If it was specified but not found,
                           * print an error *)
                          | NONE => throwUp(pos2, "Unknown type chosen for variable:"^S.name(name)))
                     (* If the type wasn't specified then cannot
                      * have NIL as initType *)
                     | NONE => (if (initType = Ty.NIL)
                                then throwUp(pos, "NIL cannot be RHS unless RECORD type explicitly specified.")
                                else ()));
                  (* Recur with the new variable added to the venv *)
                  trDecs(S.enter(venv, name,
                                 E.VarEntry({access=newAccess,
                                             ty= !typeToUse})),
                         tenv, decs, level, brkLabel,
                         Translate.varDec(newAccess, level, #exp initRes)
                         ::varDecList))
               end

             (* Type declarations *)
             | A.TypeDec(tyList) =>
               let
                 (* first pass through the type declarations.
                    adds blank headers to the tenv for each tydec *)
                 val tenv' = addTypeHeads(tenv, tyList)

                 (* Returns true if names repeated. *)
                 fun duplication([]) = false
                   | duplication({name:S.symbol, ty, pos}::xs) =
                     (List.exists (fn y => name = (#name(y)))
                                  xs)
                     orelse (duplication xs)

                 (* Determines whether there is a cycle when we
                  * follow the types through the given a, given
                  * that we started searching at start *)
                 fun cyclic(a, start) =
                     (* Look up a in the tenv *)
                     (case S.look(tenv', a)
                       (* Shouldn't happen, this means a is undefined,
                        * if so, return true to indicate a problem *)
                       of NONE => true
                        (* If a is a NAME, examine the type ref inside *)
                        | SOME(Ty.NAME(sym, typref)) =>
                          (* Note we need to do two layers here
                           * to get to the new symbol of the
                           * type referred to by the first NAME *)
                          (case !typref
                            (* If it is NONE, then we have found
                             * an unresolved type, return true
                             * to indicate a problem *)
                            of NONE => true
                             (* If there is another name inside,
                              * check to see if it is the start
                              * symbol, if so, then we have found
                              * a cycle. If not, keep searching *)
                             | SOME(Ty.NAME(sym2, typref2)) =>
                               if (sym2=start)
                               then (true)
                               else cyclic(sym2, start)
                             (* If it is anything else, we have
                              * reached a concrete type, and know
                              * that there is no cycle here *)
                             | _ => false)
                        (* If it wasn't a name to begin with, then
                         * there is no cycle *)
                        | _ => false)
                 (* Determines whether there is a cycle starting
                  * from any of the symbols in the given list *)
                 fun cyclicList([]) = false
                   | cyclicList({name=sym, pos=pos, ty=ty}::xs) =
                     cyclic(sym, sym) orelse cyclicList(xs)
               in
                 (* Error is there are duplicates in the tyList *)
                 (if duplication(tyList)
                  then throwUp((#pos(hd(tyList))), "Duplicate names in a mutually recursive type declaration.")
                  else ();
                  (* Apply trTy (the second pass) over all of the
                   * decs in the tyList, recursively mutating the
                   * environment, starting with the base of tenv' *)
                  (foldl trTy tenv' tyList;
                   (* Check for cycles in the list of tydecs we just
                    * processed. If there is a cycle, print an error
                    * and recur in the environment without these decs.
                    * If not, recur with the new environment with these
                    * decs added *)
                   if cyclicList(tyList)
                   then (throwUp(0, "Cyclic type definition!");
                         trDecs(venv, tenv, decs, level, brkLabel, varDecList))
                   else trDecs(venv, tenv', decs, level, brkLabel, varDecList)))
               end

             (* Function declarations *)
             | A.FunctionDec(funlist) =>
               let
                 (* Returns true if names repeated. *)
                 fun duplication([]) = false
                   | duplication({name:S.symbol, params, body,
                                  result, pos}::xs) =
                     (List.exists (fn y => name = (#name(y)))
                                  xs)
                     orelse (duplication xs)

                 (* Translate a function param to the name paired
                  * with its type *)
                 fun transparam({name, escape, typ, pos}) =
                     case S.look(tenv, typ)
                      of SOME(t) => {name=name, ty=actualTy(t), esc=escape}
                       | NONE =>
                         (throwUp(pos, "Type of function parameter not found.");
                          {name=name, ty=Ty.BOTTOM, esc=escape})
                 (* First pass through the function decs, add
                  * the headers into the venv with the types of
                  * the params and the return type of the function *)
                 fun addFunHeads(venv, [], _) = venv
                   | addFunHeads(venv, _, []) = venv
                   | addFunHeads(venv, {name:S.symbol,
                                        params, result,
                                        body, pos}::xs, funLabel::fls) =
                     let
                       (* Translate all params into a useful form *)
                       val params' = map transparam params
                       (* Determine the return type *)
                       val retTy =
                           case result
                            (* If no type is specified, then
                               this function is assumed to be a
                               procedure and returns type UNIT *)
                            of NONE => Ty.UNIT
                             (* If a return type was specified,
                                look it up in the tenv *)
                             | SOME(rt, pos2) =>
                               (case S.look(tenv, rt)
                                 of NONE =>
                                    (throwUp(pos2,
                                             "Return type not found.");
                                     Ty.BOTTOM)
                                  | SOME(t) => actualTy(t))
                     in
                       (* Error is there are duplicates in the tyList *)
                       if duplication(funlist)
                       then throwUp((#pos(hd(funlist))), "Duplicate names in a mutually recursive function declaration.")
                       else ();
                       (* Recur with the new venv in which the
                        * headers have been entered *)
                       addFunHeads(S.enter(venv, name,
                                           E.FunEntry({level=level,
                                                       label=funLabel,
                                                       formals= map #ty params',
                                                       result=retTy})),
                                   xs, fls)
                     end
                 (* creates one label for every function *)
                 val labelList = map (fn (f) => Temp.newLabel()) funlist
                 (* Perform the first pass through the fun decs.
                  * venv' is then the venv augmented with blank headers *)
                 val venv' = addFunHeads(venv, funlist, labelList)
                 (* Second pass through the function decs, process
                  * the bodies, taking advantage of the headers that
                  * have been entered into venv' by the first pass *)
                 fun secondPass ([], _) = venv'
                   | secondPass(_, []) = venv' (* never happens *)
                   | secondPass ({name:S.symbol, params: A.field list, result,
                                  body, pos}::funs, funLabel::fls) =
                     let
                       (* Extracts the escape field from a param *)
                       fun getEscape({name, escape, typ, pos}) = !escape
                       val newLevel = Translate.newLevel({parent=level,
                                                          name=funLabel,
                                                          formals=(map getEscape params)})
                       (* Translate the params into a useful format *)
                       val params' = map transparam params
                       (* Helper function to add a var entry for a
                        * param into the given venv *)
                       fun enterparam(({name,ty,esc}, access), venv) =
                           S.enter(venv, name,
                                   E.VarEntry({access=access,
                                               ty=ty}))
                       (* Enter all of the params as var entries into
                        * the new environment venv' *)
                       val venv'' = foldr enterparam venv' (ListPair.zip(params', Translate.formals(newLevel)))
                       (* Determine what the result type should be
                        * by looking up the fun entry from the venv
                        * which should have been entered in the first
                        * pass *)
                       val resultType =
                           (case S.look(venv', name)
                             of SOME(E.FunEntry({level, label,
                                                 formals, result})) =>
                                result
                              | _ => (throwUp(pos,
                                              "Function header not found!!");
                                      Ty.BOTTOM))
                       val bodyRes = transExp(venv'', tenv,
                                              newLevel, brkLabel) body
                     in
                       (* Process the body in the new env with the
                        * function decs and the params added.
                        * If the body does not have the correct
                        * return type, print an error *)
                       (if ((#ty(bodyRes) <> resultType))
                        then throwUp(pos,
                                     "Function body does not match result type.")
                        else ();
                        Translate.procEntryExit({level=newLevel,
                                                 body=(#exp bodyRes),
                                                 isProcedure=istype(actualTy(resultType), Ty.UNIT),
                                                 isMain=false});
                        (* Process the rest of the fundecs for the
                         * second pass *)
                        secondPass(funs, fls))
                     end
               in
                 (* Recursively parse the rest of the decs
                  * with the new environment after performing the
                  * second pass on the funlist *)
                 trDecs(secondPass(funlist, labelList), tenv, decs,
                        level, brkLabel, varDecList)
               end)
      (* trDecs ENDS *)

      (* trexp: Translates/type-checks expressions *)
      and trexp(A.OpExp{left, oper=oper, right, pos}) =
          let
            datatype operTy = Arith | Compare | Equality
            (* Find if oper is arithmetic, comparison or equality *)
            fun typeOf(oper) =
                if oper = A.PlusOp orelse oper = A.MinusOp orelse
                   oper = A.TimesOp orelse oper = A.DivideOp
                then Arith
                else if oper=A.LtOp orelse oper=A.LeOp orelse
                        oper=A.GtOp orelse oper=A.GeOp
                then Compare
                else Equality
          in
            case typeOf(oper) of
                Arith => (* Handle Arithmetic ops: +,-,*,/ *)
                let
                  val leftRes = trexp(left)
                  val rightRes = trexp(right)
                in
                  (check(#ty leftRes, Ty.INT, pos,
                         "Integer required for arithmetic operations.");
                   check(#ty rightRes, Ty.INT, pos,
                         "Integer required for arithmetic operations.");
                   {exp=Translate.arithOp(oper, #exp leftRes,
                                          #exp rightRes),
                    ty=Ty.INT})
                end
              | Compare => (* comparison ops: <, <=, >=, > *)
                (* Comparison allowed only between strings or ints *)
                let
                  val leftRes = trexp(left)
                  val rightRes = trexp(right)
                  val ty = #ty(leftRes)
                  (* Check if left expression is string or int *)
                  val leftType =
                      if (istype(ty, Ty.INT) orelse
                          istype(ty, Ty.STRING))
                      then SOME(actualTy(ty))
                      else NONE
                in
                  (case leftType
                          (* If not int or string print error *)
                    of NONE => throwUp(pos, "Comparison needs int or string.")
                     (* Right expression must be of the same type *)
                     | SOME(t) => check(#ty rightRes, t, pos,
                                        "Type of both expressions should match for comparisons.");
                   {exp=Translate.relOp(oper, #exp leftRes,
                                        #exp rightRes,
                                        istype(valOf(leftType),
                                               Ty.STRING)),
                    ty=Ty.INT})
                end
              (* Handle equality checks: =, <> *)
              | Equality =>
                let
                  (* EqOps can used with strings, ints, records, or arrays *)
                  fun findEqType(typ:Ty.ty) =
                      case typ
                       of Ty.INT => SOME(Ty.INT)
                        (* when comparing unassignables are ints *)
                        | Ty.UNASSIGNABLE => SOME(Ty.INT)
                        | Ty.STRING => SOME(Ty.STRING)
                        | Ty.RECORD(t) => SOME(Ty.RECORD(t))
                        | Ty.ARRAY(t) => SOME(Ty.ARRAY(t))
                        | Ty.NIL => SOME(Ty.NIL)
                        | Ty.BOTTOM => SOME(Ty.BOTTOM)
                        | Ty.UNIT => SOME(Ty.UNIT)       (* Extension *)
                        | t => NONE
                  val leftRes = trexp(left)
                  val rightRes = trexp(right)
                  val leftType = findEqType(#ty leftRes)
                  val rightType = findEqType(#ty rightRes)
                  val leftNONE = leftType=NONE
                  val rightNONE = rightType=NONE
                in
                  (if leftNONE orelse rightNONE
                   then (* Type cannot be compared for equality *)
                     throwUp(pos, "Equality tests only valid with types: INT, STRING, RECORD, or ARRAY.")
                   else if (not(istype(valOf(leftType),
                                       valOf(rightType)) orelse
                                istype(valOf(rightType),
                                       valOf(leftType))))
                             (* Types don't match *)
                   then throwUp(pos, "Types need to match for equality test.")
                   else ();
                   if (leftType=SOME(Ty.NIL) andalso rightType=SOME(Ty.NIL))
                        (* Can't compare two NILs *)
                   then throwUp(pos, "Cannot compare two NIL expressions.")
                   else ();
                   {exp=(if(leftType=SOME(Ty.STRING))
                         then Translate.stringEquality(oper, #exp leftRes, #exp rightRes)
                         else Translate.relOp(oper, #exp leftRes, #exp rightRes, false)),
                    ty=Ty.INT})
                end
          end

        (* Primitives: ints and strings, and nil *)
        | trexp(A.IntExp(number)) = {exp=Translate.intExp(number), ty=Ty.INT}
        | trexp(A.StringExp((str, pos))) = {exp=Translate.stringExp(str), ty=Ty.STRING}
        | trexp(A.NilExp) = {exp=Translate.intExp(0), ty=Ty.NIL}
        (* Variables, outsourced to trvar *)
        | trexp(A.VarExp(var)) = (trvar(var))

        (* Function calls *)
        | trexp(A.CallExp({func, args, pos})) =
          (case S.look(venv, func)
            of SOME(E.FunEntry({level=lev, label, formals, result})) =>
               let
                 val argList = map trexp args
                 val inBaseVenv =
                     (case S.look(E.base_venv, func)
                       of SOME(E.FunEntry({level=baseLev,
                                           label=baseLab,
                                           formals=baseForm,
                                           result=baseRes})) =>
                          if (label = baseLab)
                          then true
                          else false
                        | _ => false)
                 val expRes =
                     if inBaseVenv
                     then Translate.libCall(func,
                                            map #exp argList,
                                            istype(actualTy(result),
                                                   Ty.UNIT))
                     else Translate.funCall(label, map #exp argList,
                                            level, lev,
                                            istype(actualTy(result),
                                                   Ty.UNIT))
               in
                 (if(not(checkTypeList(argList, formals)))
                  then throwUp(pos, "Function args don't match type")
                  else ();
                  {exp=expRes, ty=actualTy(result)})
               end
             | _ => (throwUp(pos, "Undefined function:" ^ S.name(func));
                     {exp=Translate.nop, ty=Types.BOTTOM}))

        (* Assignments *)
        | trexp(A.AssignExp({var, exp, pos})) =
          let
            val varRes = trvar(var)
            val expRes = trexp(exp)
            (* Determine the type of the variable on the LHS *)
            val varType= #ty(varRes)
            (* Determine the type of the expression on the RHS *)
            val expType= #ty(expRes)
          in
            (* Print an error if LHS variable is UNASSIGNABLE
             * (handles implicitly declared loop variable in for) *)
            (if (varType=Ty.UNASSIGNABLE)
             then throwUp(pos, "Cannot assign to the local variable of for")
             (* Else, check that RHS is a subtype of the LHS type.
              * If it is then we're good, else print an error *)
             else if (istype(expType, varType))
             then ()
             else throwUp(pos,"Assigning expression of type " ^
                              Ty.toString(expType)
                              ^" to variable of type "^
                              Ty.toString(varType));
             {exp=Translate.assignExp(#exp varRes, #exp expRes),
              ty=Ty.UNIT})
          end

        (* 2 kinds of sequences. *)
        | trexp(A.SeqExp([])) = ({exp=Translate.emptySeq(), ty=Ty.UNIT})
        | trexp(A.SeqExp((x,pos)::[])) =
          let
            val res = trexp(x)
          in
            {exp=Translate.singleSeq(#exp res), ty=(#ty res)}
          end
        | trexp(A.SeqExp((x, pos)::xs)) =
          let
            val fstRes = trexp(x)
            val restRes = trexp(A.SeqExp(xs))
          in
            {exp=Translate.seqExp(#exp fstRes, #exp restRes,
                                  istype(#ty restRes, Ty.UNIT)),
             ty=(#ty restRes)}
          end

        (* Let expressions. Processing outsourced to trDecs *)
        | trexp(A.LetExp({decs, body, pos})) =
          let
            val (venv, tenv,level, brkLabel, varDecList) =
                trDecs(venv, tenv, decs, level, brkLabel, [])
            val transExpArg = (venv, tenv, level, brkLabel)
            val bodyRes = (transExp(transExpArg) body)
          in
            (* trDec recursion makes varDecList backwards *)
            {exp=Translate.insertDecs(rev varDecList, #exp bodyRes,
                                      istype(#ty bodyRes, Ty.UNIT)),
             ty=(#ty bodyRes)}
          end

        (* If-then expression *)
        | trexp(A.IfExp({test=exp1,
                         then'=exp2, else'=NONE, pos=pos})) =
          (* Condition must be of type int and result of type UNIT
           * Check these and return UNIT. *)
          let
            val testRes = trexp(exp1)
            val thenRes = trexp(exp2)
          in
            (check(#ty testRes, Ty.INT, pos,
                   "Condition in an if expression must be of type INT.");
             check(#ty thenRes, Ty.UNIT, pos,
                   "Body if if-then expression must be of type UNIT.");
             (* if-then is a valueless expr *)
             {exp=Translate.ifThen(#exp testRes, #exp thenRes), ty=Ty.UNIT})
          end
        (* If-then-else expression *)
        | trexp(A.IfExp({test=exp1, then'=exp2,
                         else'=SOME(exp3), pos=pos})) =
          let
            val testRes = trexp(exp1)
            val thenRes = trexp(exp2)
            val elseRes = trexp(exp3)
            (* Find out what type the then expression has *)
            val bodyType= (#ty thenRes)
          in
            (* Condition must be an int, and the type of
             * the then and else expression must match *)
            (check(#ty testRes,Ty.INT, pos,
                   "Condition in an if expression must be of type INT.");
             check(#ty elseRes, bodyType, pos,
                   "Branches of if-then-else need to have same type.");
             {exp=Translate.ifThenElse(#exp testRes,
                                       #exp thenRes,
                                       #exp elseRes),
              ty=bodyType})
          end

        (* Loops. Body of while and for loops must be of type unit *)
        | trexp(A.WhileExp({test=exp1, body=exp2, pos=pos})) =
          let
            val testRes = trexp(exp1)
            val newBreak = Temp.newLabel()
            val bodyRes = (brNesting := !brNesting + 1;
                           transExp(venv, tenv, level, newBreak) (exp2))
            val decrement = (brNesting := !brNesting - 1)
          in
            (check(#ty testRes, Ty.INT, pos,
                   "Non-Int condition check in an while expression.");
             check(#ty bodyRes, Ty.UNIT, pos,
                   "Body of while must be of type UNIT.");
             {exp=Translate.whileExp(#exp testRes, #exp bodyRes, newBreak),
              ty=Ty.UNIT}) (* while is a valueless expression *)
          end
        (* For loops *)
        | trexp(A.ForExp({var=sym, escape=esc, lo=exp1, hi=exp2,
                          body=exp3, pos=pos})) =
          let
            (* Add loop variable to the venv with type UNASSIGNABLE.
             * UNASSIGNABLE is a subtype of int and indicates that this
             * variable should not be assigned to in the for loop *)
            val iAccess = Translate.allocLocal(level)(!esc)
            val venv' = S.enter(venv, sym,
                                E.VarEntry({access=iAccess,
                                            ty=Ty.UNASSIGNABLE}))
            val loRes = trexp(exp1)
            val hiRes = trexp(exp2)
            val newBreak = Temp.newLabel()
            val bodyRes = transExp(venv', tenv, level, newBreak) exp3
          in
            (check(#ty loRes, Ty.INT, pos,
                   "Low value in `for` must be of type INT.");
             check(#ty hiRes, Ty.INT, pos,
                   "High value in `for` must be of type INT.");
             (* Check that the body is of type UNIT
              * using with the new environment *)
             brNesting := !brNesting + 1;
             if (istype(#ty bodyRes, Ty.UNIT))
             then ()
             else throwUp(pos, "Body of for must be of type UNIT.");
             brNesting := !brNesting - 1;
             {exp=Translate.forExp(level, iAccess, newBreak,
                                   #exp loRes, #exp hiRes, #exp bodyRes),
              ty=Ty.UNIT}) (* For is a valueless expression *)
          end

        (* Break is of type BOTTOM for greatest flexibility *)
        | trexp(A.BreakExp(pos)) =
          (if (!brNesting > 0)
           then ()
           else throwUp(pos, "Break occurs out of scope.");
           {exp=Translate.brkExp(brkLabel), ty=Ty.BOTTOM})

        (* Records *)
        | trexp(A.RecordExp({fields=fields, typ=typ, pos=pos})) =
          (* Look up the type of the record in the tenv *)
          ((case S.look(tenv, typ)
             of SOME(t) =>
                (case actualTy(t)
                  (* if it was in the tenv and it's a record type,
                   * then we need to verify all of the field types
                   * are correct *)
                  of Ty.RECORD((fieldList, uniq)) =>
                     let
                       val expList = map trexp (map #2 fields)
                       val tyList = map #ty expList
                       val verifyList = ListPair.map
                                          (fn ((sym, exp, pos), ty) =>
                                              (sym, ty, pos))
                                          (fields, tyList)
                     in
                       (verifyFields(verifyList, fieldList, pos);
                        {exp=Translate.recordExp(map #exp expList),
                         ty=Ty.RECORD((fieldList, uniq))})
                     end
                   (* if it is not a record, print an error
                    * and use an empty record type to continue *)
                   | _ => (throwUp(pos, "Type given is not a record:"^S.name(typ));
                           {exp=Translate.nop, ty=Ty.RECORD([], ref ())}))
             (* if it is not in the tenv, print an error
              * and use an empty record type to continue *)
             |  NONE => (throwUp(pos, "Record type not found:"^S.name(typ));
                         {exp=Translate.nop, ty=Ty.RECORD([], ref ())})))

        (* Arrays *)
        | trexp(A.ArrayExp({typ=typ, size=size, init=init, pos=pos})) =
          let
            val sizeRes = trexp(size)
            val initRes = trexp(init)
          in
            (* Check that the size of the array is an int *)
            (check(#ty sizeRes, Ty.INT, pos,
                   "Size of array must be of type INT.");
             (* Look up the type of the array in the tenv *)
             (case S.look(tenv, typ)
               (* If not found, print error, use array[BOTTOM] to continue *)
               of NONE => (throwUp(pos, "Array type not found:"^S.name(typ));
                           {exp=Translate.nop, ty=Ty.ARRAY(Ty.BOTTOM, ref ())})
                (* If found, check if it is an array type *)
                | SOME(t) =>
                  (case actualTy(t)
                    (* If it is an array type, check if the type of the
                     * init expression is of that array's type *)
                    of Ty.ARRAY(ty, uniq) =>
                       (check(#ty initRes, ty, pos,
                              "Type of initial value does not match array type.");
                        {exp=Translate.createArray(#exp sizeRes, #exp initRes),
                         ty=actualTy(t)})

                     (* If not array, print error, use array[BOTTOM], continue *)
                     | _ => (throwUp(pos, "Type given is not an array.");
                             {exp=Translate.nop,
                              ty=Ty.ARRAY(Ty.BOTTOM, ref ())}))))
          end
            (* trexp ENDS *)
    in
      (* the body of the curried transExp function is just trexp *)
      trexp
    end

(* The main entry point for a tiger program.
 * - Call FindEscape to perform escape analysis
 * - Translates/type-checks a given program (expression), beginning
 * with the base environments
 * - Call Translate.getResult() to get list of frags at the end.
*)
fun transProg(e) =
    (Translate.reset();         (* resets certain refs in Translate *)
     errorExists := false;      (* No errors at the start *)
     FindEscape.findEscape(e);  (* Escape analysis *)
     let
       (* Used to create a frag for the whole program *)
       val mainLevel =
           Translate.newLevel {parent=Translate.outermost,
                               name=Temp.namedLabel("main"),
                               formals=[]}

       (* The brklabel for outermost doesn't matter.
        * It's a type error to break here, so this label will disappear
        *
        * This variable will contain the tree of the whole program
        *)
       val result = transExp(E.base_venv, E.base_tenv,
                             mainLevel, Temp.newLabel()) e
     in
       (* If you don't type check, then crash and burn *)
       (if !errorExists
        then (throwUp(0,"Type-checking failed.");
              raise ErrorMsg.Error)
        else ();

        (* Create a big frag for the whole program *)
        Translate.procEntryExit({level=mainLevel, body=(#exp result),
                                 isProcedure=istype(#ty result,
                                                    Ty.UNIT),
                                 isMain=true}))
     end)
end

signature RUNNER =
sig
  val run: string -> unit
  val printAST: string -> unit
  val printFrags: string -> unit
end

structure Runner :> RUNNER =
struct
(* Runs the type-checker *)
fun run(filename:string) =
    (Semant.transProg(Parse.parse(filename)))

(* Prints the AST. Mainly debugging *)
fun printAST(filename:string) = PrintAbsyn.print(TextIO.stdOut,
                                                 Parse.parse(filename))
(* Prints the frags computed by Translate *)
fun printFrags(filename:string) = (run(filename);
                                   Translate.printInfo())

end
