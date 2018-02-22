structure A = Absyn
structure Ty = Types
structure S = Symbol
structure E = Env

		
structure Translate = struct type exp = unit end

signature SEMANT =
sig
  type venv = E.enventry S.table
  type tenv = Ty.ty S.table

  type expty = {exp: Translate.exp, ty: Ty.ty}

  (* datatype foo = bar of int * string option ref *)
  (* 	       | baz of int *)

  (* val testing: foo -> string * int * string option *)

  (* val transVar: venv * tenv * A.var -> expty *)
  (* val transExp: venv * tenv -> A.exp -> expty *)
  (* val transDecs: venv * tenv * A.dec list -> {venv:venv, tenv:tenv} *)
  (* val transTy: tenv * A.exp -> Ty.ty *)

  val transProg : A.exp -> unit
end

structure Semant :> SEMANT =
struct
  type venv = E.enventry S.table
  type tenv = Ty.ty S.table

  type expty = {exp: Translate.exp, ty: Ty.ty}

  fun transVar(_)  = {exp=(), ty=Ty.UNIT}

  (* Convenience *)
  val istype = Ty.isSubtype

  val ArithOp = [A.PlusOp, A.MinusOp, A.TimesOp, A.DivideOp]
  val CompareOp = [A.LtOp, A.LeOp, A.GtOp, A.GeOp]
  val EqualityOp = [A.EqOp, A.NeqOp]
  datatype binOpType = Arith | Compare | Equality

  val errorExists = ref false
  (* Sets a flag and outputs error messages *)
  fun throwUp(pos, msg) = (errorExists := true;
			   ErrorMsg.error(pos, pos, msg))
					   
					   
  fun inList(elem, []) = false
    | inList(elem, x::xs) = if elem=x then true else inList(elem, xs)

  fun typeOfOp(oper) =
      if inList(oper, ArithOp) then Arith
      else if inList(oper, CompareOp) then Compare
      else Equality

  fun transExp(venv:venv, tenv:tenv) =
      let
	fun check(expr:A.exp, targetType, pos, msg) =
	    if(not(istype(#ty(trexp(expr)), actualTy(targetType))))
	    then throwUp(pos, msg) else ()

	and findType(expr:A.exp, []) = NONE
	  | findType(expr:A.exp, ty::tyList) =
	    if(istype(#ty(trexp(expr)), ty))
	    then SOME(actualTy(ty))
	    else findType(expr:A.exp, tyList)

	and checkTwo(left:A.exp, right:A.exp, targetType, pos, msg) =
	    (check(left, targetType, pos, msg);
	     check(right, targetType, pos, msg))

	and checkTypeList([], []) = true
	  | checkTypeList([], _) = false
	  | checkTypeList(_, []) = false
	  | checkTypeList(e::es, t::ts) = #ty(trexp(e)) = actualTy(t)
					  andalso
					  checkTypeList(es, ts)

	and findField([], id) = NONE
	  | findField((x,ty)::xs, id) = if(id=x)
					then SOME(actualTy(ty))
					else findField(xs, id)

        and verifyFields([], recordFields) = ()
          | verifyFields((sym, exp, pos)::fields, recordFields) = 
                (case findField(recordFields, sym)
                    of NONE => throwUp(pos, "Invalid field for record")
                    | SOME(ty) => 
                        check(exp, actualTy(ty), pos, "Type doesn't match for field"))


	and actualTy(Ty.NAME(a,b)) =
	    (case !b of SOME(t) => actualTy(t)
		     | NONE => (throwUp(0,"Undefined type.\n");
				Ty.BOTTOM))
	  | actualTy(a) = a


	(* trvar: Type-check the Variables *)
	and trvar(A.SimpleVar(id, pos)) =
	    (case S.look(venv, id)
	      of SOME(E.VarEntry(ty)) =>
		 {exp=(), ty=actualTy((#ty(ty)))}

	       | _ => (throwUp(pos,
				  "undefined variable:"^S.name(id));
			 {exp=(), ty=Types.BOTTOM}))
	  (* fields and records *)
	  | trvar(A.FieldVar(var, id, pos)) =
	    (print("field-var\n");
	     case (#ty(trvar(var)))
	      of Ty.RECORD(a,b) =>
		 (let
		   val fieldType=findField(a, id)
		 in
		   (case fieldType of
			NONE => (throwUp(pos,
					 "invalid field id:"^S.name(id));
				 {exp=(), ty=Ty.BOTTOM})
		     | SOME(t) =>
		       {exp=(), ty=t})
		 end)
	       | _ => (throwUp(pos, "accessing field of non-record variable:"^S.name(id));{exp=(), ty=Ty.BOTTOM}))
	  (* Array type variables *)
	  | trvar(A.SubscriptVar(var, e, pos)) =
	    (print("array-var\n");case (#ty(trvar(var)))
	      of Ty.ARRAY(a,b) =>
		 (check(e, Ty.INT, pos, "non-int subscript");
		  {exp=(), ty=actualTy(a)})
	      | _ => (throwUp(pos, "subscripting non-array variable");{exp=(), ty=Ty.BOTTOM}))
	(* trvar ENDS *)


	(* Helper for adding headers for tydecs *)
	and addTypeHeads(tenv, []) = tenv
	  | addTypeHeads(tenv, x::xs) =
	    addTypeHeads(S.enter(tenv, (#name x),
				 Ty.NAME((#name x), ref NONE)), xs)


	(* DON'T USE actualTy *)
	and trTy(tydec, tenv) =
            (* case S.look(tenv, (#name(tydec)))
	        of SOME(Ty.NAME(a, b))  =>
                    (b := S.look(tenv, sym); tenv)
		   | _ => tenv *)
	    case (#ty(tydec)) of
  		A.NameTy(sym, pos) =>
		    (print("name ty: " ^ S.name(sym) ^"\n");
                    (case S.look(tenv, (#name(tydec)))
		        of SOME(Ty.NAME(a, b))  => (b := S.look(tenv, sym); tenv)
		        | _ => tenv))
                | A.RecordTy(fields) =>
                    (print("record ty\n");
                    (case S.look(tenv, (#name(tydec)))
                        of SOME(Ty.NAME(a, b)) =>
                            (b := SOME(Ty.RECORD(accRecord(fields, tenv), ref ())); tenv)
                        | _ => tenv))
	        | A.ArrayTy(sym, pos) =>
                    (print("array ty\n");
                    (case S.look(tenv, (#name(tydec)))
                        of SOME(Ty.NAME(a, b)) =>
                            ((case S.look(tenv, sym)
                                of SOME(ty) => (b := SOME(Ty.ARRAY(ty, ref ())))
                                | NONE => throwUp(pos, "Type for array not found\n"));
                            tenv)
                        | _ => tenv))

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
	and trexp(A.OpExp{left, oper=someOp, right, pos}) =
	    (* Handle Arithmetic ops: +,-,*,/ *)
	    (case typeOfOp(someOp) of
		 Arith =>
		 (checkTwo(left, right, Ty.INT, pos,
			   "integer required");
		  {exp=(), ty=Ty.INT})
			       (* comparison ops: <, <=, >=, > *)
	       | Compare =>
		 let val leftType = findType(left, [Ty.STRING, Ty.INT])
		 in
		   ((case leftType of
			NONE => throwUp(pos,
					"int or string required")
		      | SOME(t) => check(right, t, pos,
					 "both expressions should be int or string"));
		    {exp=(), ty=Ty.INT})
		 end
	       (* Handle equality checks: =, <> *)
	       | Equality =>
		 let
		   val leftType = findType(left, [Ty.INT,
						  Ty.STRING])
		 in
		   ((case leftType of
			NONE => throwUp(pos,
					"can't check equality with this type")
		     | SOME(t) => check(right, t, pos,
					"both expressions should have same type"));
		    {exp=(), ty=Ty.INT})
		 end)
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
