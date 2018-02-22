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
  (* static variable environment, maps symbols to Env.enventry's in a table *)
  type venv = E.enventry S.table
  (* static type environment, maps symbols to Ty.ty's in a table *)
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

  (* Helper methods *)

  (* Sets error flag and outputs error message *)
  (* Note since only the starting positions are stored in the AST
     the error messages will have the same pos twice rather than
     an interval. The initial pos should still be accurate *)
  fun throwUp(pos, msg) = (errorExists := true;
			   ErrorMsg.error(pos, pos, msg))

  (* Takes in a single type and a list of types.
     If the given type is of one of the types in the list, return it.
     Otherwise return NONE.
     This helper is useful when operations can occur on one of several
     types, and we need to know which it is acting on *)
  and findType(typ:Ty.ty, []) = NONE
    | findType(typ:Ty.ty, ty::tyList) =
          if (istype(typ, ty))
          then SOME(actualTy(ty))
          else findType(typ, tyList)

  (* Takes in a list of record fields (from Ty.RECORD) and finds the
     field with the given name if it exists *)
  and findField([], id:S.symbol) = NONE
    | findField((x:S.symbol, ty:Ty.ty)::xs, id:S.symbol) =
        if (id=x) then SOME(actualTy(ty)) else findField(xs, id)

  (* Finds the actual type of a given type. If the given type is a NAME
     continue looking past all of the names until we find a concrete type.
     Note: there is an invariant in the way we construct NAME types such
     that by the time actualTy is called, there should be no NAME types
     remaining with NONE in them. *)
  and actualTy(Ty.NAME(sym, ty)) =
        (case !ty
            of SOME(t) => actualTy(t)
            (* This case should never occur, but if it does for some
               reason, print an error and return BOTTOM to try to continue *)
  	    | NONE => (throwUp(0,"Undefined type.\n"); Ty.BOTTOM))
    | actualTy(a) = a

  (* Helper for adding headers to the tenv for tydecs.
     Essentially accomplishes the first pass over the tydecs. *)
  and addTypeHeads(tenv, []) = tenv
    | addTypeHeads(tenv, {name, ty, pos}::tydecs) =
        addTypeHeads(S.enter(tenv, name, Ty.NAME(name, ref NONE)), tydecs)

  (* The main semantic analysis happens here.
     transExp translates/type checks an expression in the given venv and tenv.
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
           checks whether the expressions in the first list have the same
           types as the actual types of the types in the second list (order matters).
           This helper is useful for checking whether the arguments used
           to call a function have the correct types *)
        and checkTypeList([], []) = true (* both empty means we're done *)
	  | checkTypeList([], _) = false (* if exactly one is empty, something's wrong *)
	  | checkTypeList(_, []) = false
	  | checkTypeList(e::es, t::ts) = (* check the firsts are the same and recur *)
                #ty(trexp(e)) = actualTy(t) andalso checkTypeList(es, ts)

        (* Verifies whether the given list of record field declarations match the
           types of the record fields looked up from the tenv
           TODO: Doesn't actually verify that all of them are there exactly once *)
        and verifyFields([], recordFields) = ()
          | verifyFields((sym, exp, pos)::fields, recordFields) =
                (case findField(recordFields, sym)
                    of NONE => throwUp(pos, "Invalid field for record")
                    | SOME(ty) => check(exp, actualTy(ty), pos,
                        "Type doesn't match for field"))

	(* trvar: Translate/type-check variables *)
	and trvar(A.SimpleVar(id, pos)) =
            (* look for the id in the venv *)
	    (case S.look(venv, id)
                (* if a var entry is found, return the actual type of that entry *)
	        of SOME(E.VarEntry({ty})) => {exp=(), ty=actualTy(ty)}
                (* if not, print an error and return BOTTOM to keep going *)
	        | _ => (throwUp(pos, "undefined variable:" ^ S.name(id));
		    {exp=(), ty=Types.BOTTOM}))

          (* Field vars for accessing records *)
	  | trvar(A.FieldVar(var, id, pos)) =
            (* determine the type of the LHS variable *)
	    (case (#ty(trvar(var)))
	        of Ty.RECORD(sym, ty) =>
                    (* if it's a record, then search for the field with
                       the given name *)
	            (case findField(sym, id)
                        (* if we didn't find it, print an error and return BOTTOM
                           to continue *)
                        of NONE => (throwUp(pos, "invalid field id:" ^ S.name(id));
			    {exp=(), ty=Ty.BOTTOM})
                        (* if we did find it, then return the type of that field *)
		        | SOME(t) => {exp=(), ty=t})
                (* if the LHS wasn't a record, then print an error and return
                   BOTTOM to continue *)
	        | _ => (throwUp(pos, "accessing field of non-record:" ^ S.name(id));
                    {exp=(), ty=Ty.BOTTOM}))

          (* Subscript vars for accessing arrays *)
	  | trvar(A.SubscriptVar(var, e, pos)) =
                (* determine the type of the LHS variable *)
	        (case (#ty(trvar(var)))
                    (* if it's an array, check that the expression is an int
                       and return the type of that array *)
	            of Ty.ARRAY(typ, _) =>
                        (check(e, Ty.INT, pos, "non-int subscript for array");
		        {exp=(), ty=actualTy(typ)})
                    (* if it's not an array, print an error and return BOTTOM *)
	            | _ => (throwUp(pos, "subscripting non-array variable");
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
                                | NONE => throwUp(pos, "Type for array not found")))
                (* This shouldn't happen since we add all the headers
                   as NAMEs in the first pass, but if it does,
                   just return the current tenv *)
		| _ => ());
            (* Return the updated tenv *)
            tenv)

        and accRecord([], tenv) = []
          | accRecord(field::fields, tenv) =
            case (S.look(tenv, (#typ(field))))
                of NONE => (throwUp(#pos(field), "Type of field is not defined"); accRecord(fields, tenv))
                | SOME(t) => (#name(field), t)::accRecord(fields, tenv)

	(* trDecs: type check declarations *)
	and trDecs(venv:venv, tenv:tenv, []) = {venv=venv,tenv=tenv}
	  | trDecs(venv:venv, tenv:tenv, d::ds) =
	    (case d
		    (* normal variables *)
	      of A.VarDec({name, escape, typ, init, pos}) =>
		 let
		   val initType = #ty(transExp(venv, tenv) init)
		 in
		   ((* print("found init: "^Ty.toString(initType)^"\n"); *)
		    (case typ of
		    	 SOME(ty, pos2) =>
		    	 (case S.look(tenv, ty) of
		    	      SOME(t) =>
			      (print("initType: "^Ty.toString(initType)^
				     ", actualTy: "^Ty.toString(actualTy(t))
				     ^"\n");
			       if (not(istype(initType, actualTy(t))))
			       then throwUp(pos2,
		    			    "init value doesn't match provided type")
			       else ())
		    	    | NONE =>
		    	      throwUp(pos2, "unknown type chosen for variable:"^S.name(name)))
		       | NONE => ());
		     trDecs(S.enter(venv, name,
				    E.VarEntry({ty=initType})), tenv, ds))
		 end
	       (* Types *)
	       | A.TypeDec(tyList) =>
	       	 let
		   val tenv' = addTypeHeads(tenv, tyList)
		   fun cyclic(a, start) =
		       (case S.look(tenv', a)
			of NONE => true (* a undefined *)
			 | SOME(Ty.NAME(sym, typref)) =>
			   (case !typref
			     of NONE => true (* a=NAME(_,NONE) *)
			      | SOME(Ty.NAME(sym2, typref2))
				=> (if (sym2=start)
				   then (true)
				   else cyclic(sym2, start))
			      | _ => false)
			 | _ => false)

		   fun cyclicList([]) = false
		     | cyclicList({name=sym, pos=pos, ty=ty}::xs) =
		       cyclic(sym, sym) orelse cyclicList(xs)
		     (* | cyclicList(_::xs) = true andalso cyclicList(xs) *)
		 in
	       	   (foldr trTy tenv' tyList;
		    if cyclicList(tyList)
		    then (throwUp(0, "Cyclic type-def\n");
			  trDecs(venv, tenv, ds))
		    else trDecs(venv, tenv', ds)
		  )
		 end
	       (* Catch all *)
              | A.FunctionDec(funlist) =>
                let
                    fun transparam({name, escape, typ, pos}) =
                        case S.look(tenv, typ)
                            of SOME(t) => {name=name, ty=actualTy(t)}
                            | NONE => (throwUp(pos, "type of param not found");
                                        {name=name, ty=Ty.BOTTOM})
	            fun addFunHeads(venv, []) = venv
	              | addFunHeads(venv, {name:S.symbol, params, result, body, pos}::xs) =
                        let
                            val params' = map transparam params
                            val resultTy =
                                case result
                            (* Not guaranteed to not be NONE for actualTy *)
                                    of NONE => Ty.UNIT
                                    | SOME(rt, pos2) =>
                                        (case S.look(tenv, rt)
                                            of NONE => (throwUp(pos2, "Return type not found"); Ty.BOTTOM)
                                            | SOME(t) => actualTy(t))
                         in
	                    addFunHeads(S.enter(venv, name,
	                        E.FunEntry({formals= map #ty params',
                                result=resultTy})), xs)
                         end
                    val venv' = addFunHeads(venv, funlist)
                    fun secondPass ([]) = venv'
                      | secondPass ({name:S.symbol, params, result, body, pos}::funs) =
                        let
                            val params' = map transparam params
                            fun enterparam({name,ty}, venv) =
                                S.enter(venv, name, E.VarEntry({ty=ty}))
                            val venv'' = foldr enterparam venv' params'
                            val resultType = (case S.look(venv', name)
                                of SOME(E.FunEntry({formals, result})) => result
                                | _ => (throwUp(pos, "function header not there");
                                    Ty.BOTTOM))
                        in
                            (if ((#ty (transExp(venv'', tenv) body))<>resultType)
                            then throwUp(pos, "Function body does not match result type")
                            else ();
                            secondPass(funs))
                        end
                in
                    trDecs(secondPass(funlist), tenv, ds)
                end)

	(* trDecs ENDS *)


	(* The actual workhorse *)
	and trexp(A.OpExp{left, oper=oper, right, pos}) =
	    (* Handle Arithmetic ops: +,-,*,/ *)
            if oper = A.PlusOp orelse oper = A.MinusOp orelse
                oper = A.TimesOp orelse oper = A.DivideOp
            then (check(left, Ty.INT, pos, "integer required");
	        check(right, Ty.INT, pos, "integer required");
		{exp=(), ty=Ty.INT})
	    (* comparison ops: <, <=, >=, > *)
            else if oper = A.LtOp orelse oper = A.LeOp orelse
                oper = A.GtOp orelse oper = A.GeOp
            then
		 let val leftType = findType(#ty(trexp(left)), [Ty.STRING, Ty.INT])
		 in
		   ((case leftType of
			NONE => throwUp(pos,
					"int or string required")
		      | SOME(t) => check(right, t, pos,
					 "both expressions should be int or string"));
		    {exp=(), ty=Ty.INT})
		 end
	       (* Handle equality checks: =, <> *)
            else
		 let
		   val leftType = findType(#ty(trexp(left)), [Ty.INT,
						  Ty.STRING])
		 in
		   ((case leftType of
			NONE => throwUp(pos,
					"can't check equality with this type")
		     | SOME(t) => check(right, t, pos,
					"both expressions should have same type"));
		    {exp=(), ty=Ty.INT})
		 end
	  (* Integers, strings, nils *)
	  | trexp(A.IntExp(number)) = ({exp=(), ty=Ty.INT})
	  | trexp(A.StringExp(string)) = (print("String:"^(#1(string))^"\n");
					  {exp=(), ty=Ty.STRING})
	  | trexp(A.NilExp) = {exp=(), ty=Ty.NIL}
	  (* variables, outsourced to trvar *)
	  | trexp(A.VarExp(var)) = (trvar(var))
	  (* Function calls *)
	  | trexp(A.CallExp({func, args, pos})) =
	    (print("calling:"^S.name(func)^"\n");
	     case S.look(venv, func)
	      of SOME(E.FunEntry({formals, result})) =>
		 (if(not(checkTypeList(args, formals)))
		  then throwUp(pos,
			       "function args don't match type")
		  else ();
		   {exp=(), ty=actualTy(result)})
	       | _ => (throwUp(pos,
			      "undefined function:"^S.name(func));
		       {exp=(), ty=Types.BOTTOM}))
	  (* Assignments, not too difficult *)
	  | trexp(A.AssignExp({var, exp, pos})) =
	    let
	      val varType= #ty(trvar(var))
	      val expType= #ty(trexp(exp))
	    in
	      ((if (varType=Ty.UNASSIGNABLE)
              then throwUp(pos, "Cannot assign to the local variable of for")
              else
              (if(varType=expType) then ()
	       else throwUp(pos,"Assigning expression of type "
				    ^Ty.toString(expType)^
				    " to variable of type "^
				    Ty.toString(varType))));
	       {exp=(), ty=Ty.UNIT})
	    end
	  (* Three kinds of sequences. non-last exp forced to unit  *)
	  | trexp(A.SeqExp([])) = ({exp=(), ty=Ty.UNIT})
	  | trexp(A.SeqExp((x, pos)::[])) = (trexp(x))
	  | trexp(A.SeqExp((x, pos)::xs)) =
	    if(#ty(trexp(x))=Ty.UNIT)
	    then (trexp(A.SeqExp(xs)))
	    else (throwUp(pos, "Need Unit type for non-final SeqExp expressions."); {exp=(), ty=Ty.BOTTOM})
	  (* Let expressions. Decs outsourced to trDecs *)
	  | trexp(A.LetExp({decs, body, pos})) =
	    let
	      val {venv=venv', tenv=tenv'}=trDecs(venv, tenv, decs)
	    in (transExp(venv', tenv') body)
	    end
	  | trexp(A.IfExp({test=exp1,
			   then'=exp2,
			   else'=NONE, pos=pos})) =
	    (print("else-less if\n");
	     check(exp1, Ty.INT, pos,
		   "Non-Int condition check in an if expression.");
	     {exp=(), ty=(#ty(trexp(exp2)))})
	  | trexp(A.IfExp({test=exp1,
			   then'=exp2,
			   else'=SOME(exp3), pos=pos})) =
	    let
	      val bodyType= (#ty(trexp(exp2)))
	    in
	      (print("if with else\n");
	       check(exp1,Ty.INT, pos,
		   "Non-Int condition check in an if expression.");
	       check(exp3, bodyType, pos,
		   "if branches need to have same type.");
	       {exp=(), ty=bodyType})
	    end
	  | trexp(A.WhileExp({test=exp1, body=exp2, pos=pos})) =
	    (print("while\n");
	     check(exp1, Ty.INT, pos,
		   "Non-Int condition check in an while expression.");
	     check(exp2, Ty.UNIT, pos,
		   "Body of while must be of type UNIT.");
	     {exp=(), ty=Ty.UNIT}
	    )
	  | trexp(A.ForExp({var=sym, escape=esc, lo=exp1, hi=exp2,
			    body=exp3, pos=pos})) =
	    let val venv'=S.enter(venv, sym, E.VarEntry({ty=Ty.UNASSIGNABLE}))
	    in
	      (check(exp1, Ty.INT, pos,
		     "Low value in `for` must be of type INT.");
	       check(exp2, Ty.INT, pos,
		     "High value in `for` must be of type INT.");
	       (* sym cannot be assigned to. DO THAT! *)
	       (* This is essentially check which takes in extra
		venv param. Consider writing another func for this. *)
	       if (#ty(transExp(venv', tenv) exp3)<> Ty.UNIT)
	       then throwUp(pos,
			    "Body of for must be of type UNIT.")
	       else ();
	       {exp=(), ty=Ty.UNIT})
	    end
	  | trexp(A.BreakExp(pos)) = (print("break\n");
				      {exp=(), ty=Ty.BOTTOM})
          | trexp(A.RecordExp({fields=fields, typ=typ, pos=pos})) =
                (print("record: "^S.name(typ)^"\n");
                (case S.look(tenv, typ)
                    of SOME(t) =>
                        (case actualTy(t)
                        of Ty.RECORD((fieldList, uniq)) =>
                            (verifyFields(fields, fieldList);
                            {exp=(), ty=Ty.RECORD((fieldList, uniq))})
                        | _ => (throwUp(pos, "type given is not a record");
                            {exp=(), ty=Ty.RECORD([], ref ())}))
                    |  NONE => (throwUp(pos, "record type not found");
                        {exp=(), ty=Ty.RECORD([], ref ())})))

          | trexp(A.ArrayExp({typ=typ, size=size, init=init, pos=pos})) =
                (print("array: "^S.name(typ)^"\n");
                check(size, Ty.INT, pos, "Size of array must be of type INT.");
                (case S.look(tenv, typ)
                    of NONE => (throwUp(pos, "array type not found");
                        {exp=(), ty=Ty.ARRAY(Ty.BOTTOM, ref ())})
                    | SOME(t) =>
                        (print("TYPE: "^Ty.toString(actualTy(t)) ^ "\n");
                        (case actualTy(t)
                        of Ty.ARRAY(ty, uniq) => (check(init, ty, pos,
                            "initial value does not match array type");
                            {exp=(), ty=actualTy(t)})
                        | _ => (throwUp(pos, "type given is not an array");
                            {exp=(), ty=Ty.ARRAY(Ty.BOTTOM, ref ())})))))
(*	  | trexp(_) = (throwUp(0,"fell-off");{exp=(), ty=Ty.UNIT}) *)
			 (* trexp ENDS *)

      in
	trexp
      end

  fun transProg(e) = (transExp(E.base_venv, E.base_tenv) e;
		      if !errorExists
		      then throwUp(0,"Type-checking failed.")
		      else print("Done\n"))

end

signature MAIN =
sig
  val run: string -> unit
end

structure Main :> MAIN =
struct
fun run(filename:string) = Semant.transProg(Parse.parse(filename))
end
