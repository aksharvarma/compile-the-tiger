(* Shorten commonly used structures *)
structure A = Absyn
structure Ty = Types
structure S = Symbol
structure E = Env

(* Dummy Translate module to be fleshed out in later assignment *)
structure Translate = struct type exp = unit end

(* Main semantic analysis module *)
signature SEMANT =
sig
  (* static variable environment, table maps symbols to Env.enventry *)
  type venv = E.enventry S.table
  (* static type environment, table maps symbols to Ty.ty *)
  type tenv = Ty.ty S.table

  (* expty: a translated expression and its type *)
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

(* Sets error flag and outputs error message *)
(* Note since only the starting positions are stored in the AST
   the error messages will have the same pos twice rather than
   an interval. The initial pos should still be accurate *)
fun throwUp(pos, msg) = (errorExists := true;
			 ErrorMsg.error(pos, pos, msg))


(* Takes in a list of record fields (from Ty.RECORD) and finds the
   field with the given name if it exists *)
and findField([], id:S.symbol) = NONE
  | findField((x:S.symbol, ty:Ty.ty)::xs, id:S.symbol) =
    if (id=x) then SOME(actualTy(ty)) else findField(xs, id)

and checkFieldAndRemove([], (id:S.symbol, typ:Ty.ty, pos)) =
        (throwUp(pos, "Field " ^ S.name(id) ^ " doesn't exit in record");
        [])
  | checkFieldAndRemove((x:S.symbol, ty:Ty.ty)::xs, (id:S.symbol, typ:Ty.ty, pos)) =
        if (id = x)
        then if (istype(actualTy(typ), actualTy(ty)))
        then xs
        else (throwUp(pos, "Field " ^ S.name(x) ^ " has incorrect type"); xs)
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
     of NONE => (throwUp(pos, "Type of field: "^S.name(name)^
			      " is not defined");
		 accRecord(fields, tenv))
      (* If found, add (name, t) to the list and recur *)
      | SOME(t) => (name, t)::accRecord(fields, tenv)

(* The main semantic analysis happens here.
   transExp translates/type checks an expression given venv and tenv.
   transExp is curried to take in the venv and tenv first and return
   a function taking in the expression to analyze since many recursive
   calls should happen in the same environments as were given initially *)
fun transExp(venv:venv, tenv:tenv) =
    let
      (* More helper methods *)

      (* Checks whether the given expression is of the target type.
           If not, print the given error message. *)
      fun check(expr:A.exp, targetType:Ty.ty, pos, msg) =
	  if (not (istype(#ty(trexp(expr)), actualTy(targetType))))
	  then throwUp(pos, msg)
          else ()

      (* Takes a list of expressions and a list of types and
         checks whether the expressions in the first list
	 have the same types as the actual types of the
	 types in the second list (order matters).
         This helper is useful for checking whether the
	 arguments used to call a function have the correct types *)
      and checkTypeList([], []) = true (* we're good if both empty *)
	(* if exactly one is empty, something's wrong *)
	| checkTypeList([], _) = false
	| checkTypeList(_, []) = false
	(* check 1st entry and then recur *)
	| checkTypeList(e::es, t::ts) =
          #ty(trexp(e)) = actualTy(t) andalso checkTypeList(es, ts)

      (* Verifies whether the given list of record field declarations
	 match the types of the record fields looked up from the tenv
         TODO: Doesn't actually verify that all of them
	 are there exactly once *)
      and verifyFields([], [], pos) = () (* both empty then we're good *)
        | verifyFields([], _, pos) = throwUp(pos,
            "Given fields don't match fields for record type: too few fields")
        | verifyFields(_, [], pos) = throwUp(pos,
            "Given fields don't match fields for record type: too many fields")
        | verifyFields((sym:S.symbol, exp:A.exp, pos)::fields, recordFields, pos2) =
            verifyFields(fields, checkFieldAndRemove(recordFields,
                (sym, #ty(trexp(exp)), pos)), pos2)

      (* trvar: Translate/type-check variables *)
      and trvar(A.SimpleVar(id, pos)) =
          (* look for the id in the venv *)
	  (case S.look(venv, id)
            (* if a var entry is found,
	       return the actual type of that entry *)
	    of SOME(E.VarEntry({ty})) => {exp=(), ty=actualTy(ty)}
             (* if not, print an error
		and return BOTTOM to keep going *)
	     | _ => (throwUp(pos, "Undefined variable:" ^ S.name(id));
		     {exp=(), ty=Types.BOTTOM}))

        (* Field vars for accessing records *)
	| trvar(A.FieldVar(var, id, pos)) =
          (* determine the type of the LHS variable *)
	  (case (#ty(trvar(var)))
	    of Ty.RECORD(sym, ty) =>
               (* if it's a record, then search for the field with
                  the given name *)
	       (case findField(sym, id)
                 (* if we didn't find it,
		    print an error and return BOTTOM to continue *)
                 of NONE => (throwUp(pos, "Invalid field id:" ^ S.name(id));
			     {exp=(), ty=Ty.BOTTOM})
                  (* if we did find it,
		     then return the type of that field *)
		  | SOME(t) => {exp=(), ty=t})
             (* if the LHS wasn't a record, then print error
		and return BOTTOM to continue *)
	     | _ => (throwUp(pos, "accessing field of non-record:" ^ S.name(id));
                     {exp=(), ty=Ty.BOTTOM}))

        (* Subscript vars for accessing arrays *)
	| trvar(A.SubscriptVar(var, e, pos)) =
          (* determine the type of the LHS variable *)
	  (case (#ty(trvar(var)))
            (* if it's an array, check that the expression is an int
               and return the type of that array *)
	    of Ty.ARRAY(typ, _) =>
               (check(e, Ty.INT, pos, "Non integer subscript.");
		{exp=(), ty=actualTy(typ)})
             (* if it's not an array, print an error and return BOTTOM *)
	     | _ => (throwUp(pos, "Subscripting non-array variable.");
                     {exp=(), ty=Ty.BOTTOM}))
      (* trvar ENDS *)

      (* trTy: Translate/type-check type declarations *)
      (* essentially accomplishes the second pass over the mutually
         recursive type definitions *)
      and trTy({name, ty, pos}, tenv) =
          ((case S.look(tenv, name)
             (* If it's there and a NAME type, case on ty
                and mutate the type in the NAME accordingly *)
	     of SOME(Ty.NAME(s, typ))  =>
                (case ty
                  (* Look up the type of the ref type in the NAME
                     and set the ref to be that looked up type *)
                  of A.NameTy(sym, pos) => typ := S.look(tenv, sym)
                   (* Set the type of the NAME to be a new
                      record type *)
                   | A.RecordTy(fields) =>
                     typ := SOME(Ty.RECORD(accRecord(fields, tenv), ref ()))
                   (* Look up the type of the ref type in the NAME
                      and set the ref to be a new array type of
                      that looked up type *)
	           | A.ArrayTy(sym, pos) =>
                     (case S.look(tenv, sym)
                       of SOME(t) => (typ := SOME(Ty.ARRAY(t, ref ())))
                        | NONE => throwUp(pos, "Type for array not found.")))
              (* This shouldn't happen since we add all the headers
                 as NAMEs in the first pass, but if it does,
                 just return the current tenv *)
	      | _ => ());
           (* Return the updated tenv *)
           tenv)

      (* trDecs: Translate/type-check all declarations.
         Returns an updated venv and tenv *)
      and trDecs(venv:venv, tenv:tenv, []) = (venv, tenv)
	| trDecs(venv:venv, tenv:tenv, dec::decs) =
	  (case dec of
	       (* normal variables *)
	       A.VarDec({name, escape, typ, init, pos}) =>
	       let
                 (* Determine the type of the init expr *)
		 val initType = #ty(transExp(venv, tenv) init)
		 (* If type explicitly specified, use that.
		    Handles things like var b:rectype := nil
		    where we want RECORD and not NIL as the type *)
		 val typeToUse:Ty.ty ref = ref initType
	       in
		 ((case typ
                    (* If the type of the variable was specified,
                       look it up in the tenv *)
                    of SOME(ty, pos2) =>
		       (case S.look(tenv, ty)
                         (* If it was found in the tenv, print an
                            error if it is not equal to the type of
                            the init expr *)
                         of SOME(t) =>
			    (if (not(istype(initType, actualTy(t))))
			    then throwUp(pos2, "Initial expression doesn't match provided type:"^S.name(ty))
			     else ();
			     (* If type is provided, use that. *)
			     typeToUse:=actualTy(t))
                          (* If it was specified but not found,
                             print an error *)
		    	  | NONE => throwUp(pos2, "Unknown type chosen for variable:"^S.name(name)))
                     (* If the type wasn't specified then cannot
			have NIL as initType *)
		     | NONE => (if (initType = Ty.NIL)
				then throwUp(pos, "NIL cannot be RHS unless RECORD type explicitly specified.")
				else ()));
                  (* Recur with the new variable added to the venv *)
		  trDecs(S.enter(venv, name,
				 E.VarEntry({ty= !typeToUse})),
			 tenv, decs))
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
                    follow the types through the given a, given
                    that we started searching at start *)
		 fun cyclic(a, start) =
                     (* Look up a in the tenv *)
		     (case S.look(tenv', a)
                       (* Shouldn't happen, this means a is undefined,
                          if so, return true to indicate a problem *)
		       of NONE => true
                        (* If a is a NAME, examine the type ref inside *)
			| SOME(Ty.NAME(sym, typref)) =>
                          (* Note we need to do two layers here
                             to get to the new symbol of the
                             type referred to by the first NAME *)
			  (case !typref
                            (* If it is NONE, then we have found
                               an unresolved type, return true
                               to indicate a problem *)
			    of NONE => true
                             (* If there is another name inside,
                                check to see if it is the start
                                symbol, if so, then we have found
                                a cycle. If not, keep searching *)
			     | SOME(Ty.NAME(sym2, typref2)) =>
                               if (sym2=start)
			       then (true)
			       else cyclic(sym2, start)
                             (* If it is anything else, we have
                                reached a concrete type, and know
                                that there is no cycle here *)
			     | _ => false)
                        (* If it wasn't a name to begin with, then
                           there is no cycle *)
			| _ => false)
                 (* Determines whether there is a cycle starting
                    from any of the symbols in the given list *)
		 fun cyclicList([]) = false
		   | cyclicList({name=sym, pos=pos, ty=ty}::xs) =
		     cyclic(sym, sym) orelse cyclicList(xs)
	       in
		 (* Error is there are duplicates in the tyList *)
		 if duplication(tyList)
		 then throwUp((#pos(hd(tyList))), "Duplicate names in a mutually recursive type declaration.")
		 else ();
                 (* Apply trTy (the second pass) over all of the
                    decs in the tyList, recursively mutating the
                    environment, starting with the base of tenv' *)
	       	 (foldl trTy tenv' tyList;
                  (* Check for cycles in the list of tydecs we just
                     processed. If there is a cycle, print an error
                     and recur in the environment without these decs.
                     If not, recur with the new environment with these
                     decs added *)
		  if cyclicList(tyList)
		  then (throwUp(0, "Cyclic type definition!");
			trDecs(venv, tenv, decs))
		  else trDecs(venv, tenv', decs))
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
                    with its type *)
                 fun transparam({name, escape, typ, pos}) =
                     case S.look(tenv, typ)
                      of SOME(t) => {name=name, ty=actualTy(t)}
                       | NONE =>
                         (throwUp(pos, "Type of function parameter not found.");
                          {name=name, ty=Ty.BOTTOM})
                 (* First pass through the function decs, add
                    the headers into the venv with the types of
                    the params and the return type of the function *)
	         fun addFunHeads(venv, []) = venv
	           | addFunHeads(venv, {name:S.symbol,
					params, result,
					body, pos}::xs) =
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
                          headers have been entered *)
	               addFunHeads(S.enter(venv, name,
	                                   E.FunEntry({formals= map #ty params', result=retTy})), xs)
                     end
                 (* Perform the first pass through the fun decs.
                    venv' is then the venv augmented with blank headers *)
                 val venv' = addFunHeads(venv, funlist)
                 (* Second pass through the function decs, process
                    the bodies, taking advantage of the headers that
                    have been entered into venv' by the first pass *)
                 fun secondPass ([]) = venv'
                   | secondPass ({name:S.symbol, params, result,
                                  body, pos}::funs) =
                     let
                       (* Translate the params into a useful format *)
                       val params' = map transparam params
                       (* Helper function to add a var entry for a
                          param into the given venv *)
                       fun enterparam({name,ty}, venv) =
                           S.enter(venv, name, E.VarEntry({ty=ty}))
                       (* Enter all of the params as var entries into
                          the new environment venv' *)
                       val venv'' = foldr enterparam venv' params'
                       (* Determine what the result type should be
                          by looking up the fun entry from the venv
                          which should have been entered in the first
                          pass *)
                       val resultType =
                           (case S.look(venv', name)
                             of SOME(E.FunEntry({formals, result})) =>
                                result
                              | _ => (throwUp(pos,
                                              "Function header not found!!");
                                      Ty.BOTTOM))
                     in
                       (* Process the body in the new env with the
                          function decs and the params added.
                          If the body does not have the correct
                          return type, print an error *)
                       (if ((#ty(transExp(venv'', tenv) body))
                            <>resultType)
                        then throwUp(pos,
                                     "Function body does not match result type.")
                        else ();
                        (* Process the rest of the fundecs for the i
                           second pass *)
                        secondPass(funs))
                     end
               in
                 (* Recursively parse the rest of the decs
                    with the new environment after performing the
                    second pass on the funlist *)
                 trDecs(secondPass(funlist), tenv, decs)
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
		(check(left, Ty.INT, pos, "Integer required for arithmetic operations.");
	         check(right, Ty.INT, pos, "Integer required for arithmetic operations.");
		 {exp=(), ty=Ty.INT})
	     | Compare => (* comparison ops: <, <=, >=, > *)
               (* Comparison allowed only between strings or ints *)
	       let
                 val ty = #ty(trexp(left))
		 (* Check if left expression is string or int *)
		 val leftType =
		     if (istype(ty, Ty.INT) orelse istype(ty, Ty.STRING))
		     then SOME(actualTy(ty))
		     else NONE
	       in
		 (case leftType
		    (* If not int or string print error *)
		    of NONE => throwUp(pos, "Comparison needs int or string.")
                    (* Right expression must be of the same type *)
		    | SOME(t) => check(right, t, pos,
				    "Type of both expressions should match for comparisons.");
		  {exp=(), ty=Ty.INT})
	       end
	      (* Handle equality checks: =, <> *)
	     | Equality =>
	       let
		 (* Equality ops can used with strings, ints, records, or arrays *)
		 fun findEqType(typ:Ty.ty) =
		     case typ
		      of Ty.INT => SOME(Ty.INT)
                       | Ty.UNASSIGNABLE => SOME(Ty.INT) (* when comparing unassignables are ints *)
		       | Ty.STRING => SOME(Ty.STRING)
		       | Ty.RECORD(t) => SOME(Ty.RECORD(t))
		       | Ty.ARRAY(t) => SOME(Ty.ARRAY(t))
		       | Ty.NIL => SOME(Ty.NIL)
                       | Ty.BOTTOM => SOME(Ty.BOTTOM)
		       | Ty.UNIT => SOME(Ty.UNIT) 	(* Extension *)
		       | t => NONE
		 val leftType = findEqType(#ty(trexp(left)))
		 val rightType = findEqType(#ty(trexp(right)))
		 val leftNONE = leftType=NONE
		 val rightNONE = rightType=NONE
	       in
		 (if leftNONE orelse rightNONE
		  then (* Type cannot be compared for equality *)
		    throwUp(pos, "Equality tests only valid with types: INT, STRING, RECORD, or ARRAY.")
		  else if (not(istype(valOf(leftType), valOf(rightType)) orelse istype(valOf(rightType), valOf(leftType))))
		  (* Types don't match *)
		  then throwUp(pos, "Types need to match for equality test.")
                  else ();
		  if (leftType=SOME(Ty.NIL) andalso rightType=SOME(Ty.NIL))
		  (* Can't compare two NILs *)
		  then throwUp(pos, "Cannot compare two NIL expressions.")
		  else ();
		  {exp=(), ty=Ty.INT})
	       end
	  end

        (* Primitives: ints and strings, and nil *)
	| trexp(A.IntExp(number)) = {exp=(), ty=Ty.INT}
	| trexp(A.StringExp(str)) = {exp=(), ty=Ty.STRING}
	| trexp(A.NilExp) = {exp=(), ty=Ty.NIL}
	(* Variables, outsourced to trvar *)
	| trexp(A.VarExp(var)) = (trvar(var))

	(* Function calls *)
	| trexp(A.CallExp({func, args, pos})) =
	  (case S.look(venv, func)
	    of SOME(E.FunEntry({formals, result})) =>
	       (if(not(checkTypeList(args, formals)))
		then throwUp(pos, "Function args don't match type")
		else ();
		{exp=(), ty=actualTy(result)})
	     | _ => (throwUp(pos, "Undefined function:" ^ S.name(func));
		     {exp=(), ty=Types.BOTTOM}))

	(* Assignments *)
	| trexp(A.AssignExp({var, exp, pos})) =
	  let
            (* Determine the type of the variable on the LHS *)
	    val varType= #ty(trvar(var))
            (* Determine the type of the expression on the RHS *)
	    val expType= #ty(trexp(exp))
	  in
            (* Print an error if LHS variable is UNASSIGNABLE
	       (handles implicitly declared loop variable in for) *)
	    (if (varType=Ty.UNASSIGNABLE)
             then throwUp(pos, "Cannot assign to the local variable of for")
             (* Else, check that RHS is a subtype of the LHS type.
                If it is then we're good, else print an error *)
             else if (istype(expType, varType))
             then ()
	     else throwUp(pos,"Assigning expression of type " ^
			      Ty.toString(expType)
			      ^" to variable of type "^
			      Ty.toString(varType));
	     {exp=(), ty=Ty.UNIT})
	  end

	(* 3 kinds of sequences. non-last exp is enforced as unit  *)
	| trexp(A.SeqExp([])) = ({exp=(), ty=Ty.UNIT})
	| trexp(A.SeqExp((x, pos)::[])) = (trexp(x))
	| trexp(A.SeqExp((x, pos)::xs)) = (trexp(x); trexp(A.SeqExp(xs)))

	(* Let expressions. Processing outsourced to trDecs *)
	| trexp(A.LetExp({decs, body, pos})) =
	  transExp(trDecs(venv, tenv, decs)) body

        (* If-then expression *)
	| trexp(A.IfExp({test=exp1,
			 then'=exp2, else'=NONE, pos=pos})) =
          (* Condition must be of type int and result of type UNIT
             Check these and return UNIT. *)
	  (check(exp1, Ty.INT, pos,
		 "Condition in an if expression must be of type INT.");
	   check(exp2, Ty.UNIT, pos,
		 "Body if if-then expression must be of type UNIT.");
	   {exp=(), ty=Ty.UNIT}) (* if-then is a valueless expr *)

	(* If-then-else expression *)
	| trexp(A.IfExp({test=exp1, then'=exp2,
			 else'=SOME(exp3), pos=pos})) =
	  let
            (* Find out what type the then expression has *)
	    val bodyType= (#ty(trexp(exp2)))
	  in
            (* Condition must be an int, and the type of
	       the then and else expression must match *)
	    (check(exp1,Ty.INT, pos,
		   "Condition in an if expression must be of type INT.");
	     check(exp3, bodyType, pos,
		   "Branches of if-then-else need to have same type.");
	     {exp=(), ty=bodyType})
	  end

        (* Loops. Body of while and for loops must be of type unit *)
	| trexp(A.WhileExp({test=exp1, body=exp2, pos=pos})) =
	  (check(exp1, Ty.INT, pos,
		 "Non-Int condition check in an while expression.");
	   brNesting := !brNesting + 1;
           check(exp2, Ty.UNIT, pos,
		 "Body of while must be of type UNIT.");
           brNesting := !brNesting - 1;
	   {exp=(), ty=Ty.UNIT}) (* while is a valueless expression *)

	(* For loops *)
	| trexp(A.ForExp({var=sym, escape=esc, lo=exp1, hi=exp2,
			  body=exp3, pos=pos})) =
	  let
            (* Add loop variable to the venv with type UNASSIGNABLE.
               UNASSIGNABLE is a subtype of int and indicates that this
               variable should not be assigned to in the for loop *)
            val venv' = S.enter(venv, sym, E.VarEntry({ty=Ty.UNASSIGNABLE}))
	  in
	    (check(exp1, Ty.INT, pos,
		   "Low value in `for` must be of type INT.");
	     check(exp2, Ty.INT, pos,
		   "High value in `for` must be of type INT.");
             (* Check that the body is of type UNIT
		using with the new environment *)
	     brNesting := !brNesting + 1;
             if (istype(#ty(transExp(venv', tenv) exp3), Ty.UNIT))
	     then ()
	     else throwUp(pos, "Body of for must be of type UNIT.");
             brNesting := !brNesting - 1;
	     {exp=(), ty=Ty.UNIT}) (* For is a valueless expression *)
	  end

        (* Break is of type BOTTOM for greatest flexibility *)
	| trexp(A.BreakExp(pos)) =
          (if (!brNesting > 0)
           then ()
           else throwUp(pos, "Break occurs out of scope.");
           {exp=(), ty=Ty.BOTTOM})

	(* Records *)
        | trexp(A.RecordExp({fields=fields, typ=typ, pos=pos})) =
          (* Look up the type of the record in the tenv *)
          ((case S.look(tenv, typ)
             of SOME(t) =>
                (case actualTy(t)
                  (* if it was in the tenv and it's a record type,
                     then we need to verify all of the field types
                     are correct *)
                  of Ty.RECORD((fieldList, uniq)) =>
                     (verifyFields(fields, fieldList, pos);
                      {exp=(), ty=Ty.RECORD((fieldList, uniq))})
                   (* if it is not a record, print an error
                      and use an empty record type to continue *)
                   | _ => (throwUp(pos, "Type given is not a record:"^S.name(typ));
                           {exp=(), ty=Ty.RECORD([], ref ())}))
             (* if it is not in the tenv, print an error
                and use an empty record type to continue *)
             |  NONE => (throwUp(pos, "Record type not found:"^S.name(typ));
                         {exp=(), ty=Ty.RECORD([], ref ())})))

        (* Arrays *)
        | trexp(A.ArrayExp({typ=typ, size=size, init=init, pos=pos})) =
          (* Check that the size of the array is an int *)
          (check(size, Ty.INT, pos, "Size of array must be of type INT.");
           (* Look up the type of the array in the tenv *)
           (case S.look(tenv, typ)
             (* If not found, print error and use array of BOTTOM to
                continue *)
             of NONE => (throwUp(pos, "Array type not found:"^S.name(typ));
                         {exp=(), ty=Ty.ARRAY(Ty.BOTTOM, ref ())})
              (* If found, check if it is an array type *)
              | SOME(t) =>
                (case actualTy(t)
                  (* If it is an array type, check if the type of the
                     init expression is of that array's type *)
                  of Ty.ARRAY(ty, uniq) =>
                     (check(init, ty, pos,
                            "Type of initial value does not match array type.");
                      {exp=(), ty=actualTy(t)})
                   (* If not array, print error and use array of BOTTOM
                      to continue *)
                   | _ => (throwUp(pos, "Type given is not an array.");
                           {exp=(), ty=Ty.ARRAY(Ty.BOTTOM, ref ())}))))
	    (* trexp ENDS *)

    in
      (* the body of the curried transExp function is just trexp *)
      trexp
    end

(* The main entry point for a tiger program.
   Translates/type-checks a given program (expression), beginning
   with the base environments *)
fun transProg(e) = (errorExists := false;
                    transExp(E.base_venv, E.base_tenv) e;
		    if !errorExists
		    then (throwUp(0,"Type-checking failed.");
			  raise ErrorMsg.Error)

		    else ())

end

signature MAIN =
sig
  val run: string -> unit
  val printAST: string -> unit
end

structure Main :> MAIN =
struct
(* Runs the type-checker *)
fun run(filename:string) = Semant.transProg(Parse.parse(filename))

(* Prints the AST *)
fun printAST(filename:string) = PrintAbsyn.print(TextIO.stdOut,
						 Parse.parse(filename))
end
