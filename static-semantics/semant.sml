structure A = Absyn
structure Ty = Types
structure S = Symbol
structure E = ErrorMsg
(* val error = ErrorMsg.error *)

structure Translate = struct type exp = unit end

signature SEMANT =
sig
  type venv = Env.enventry S.table
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
  type venv = Env.enventry S.table
  type tenv = Ty.ty S.table

  type expty = {exp: Translate.exp, ty: Ty.ty}

  (* fun isInt({exp,ty}) = *)
  (*     case ty of Ty.INT => true *)
  (* 	       | _ => false *)

  (* fun isString({exp,ty}) = *)
  (*     case ty of Ty.STRING => true *)
  (* 	       | _ => false *)

  (* fun check(b, pos, msg) = *)
  (*     if(not(b)) then E.error(pos, pos, msg) else () *)

  (* fun transVar(A.VarExp(var)) = *)
  (*     let *)
  (* 	val asdf = trexp(var) *)
  (*     in *)
  (* 	transExp(S.enter(venv, Symbol.symbolize(var), VarEntry(#ty(asdf))), tenv) *)
  (*     end *)
  (*   |  *)
  fun transVar(_)  = {exp=(), ty=Ty.UNIT}

  (* Convenience *)
  val istype = Ty.isSubtype

  val ArithOp = [A.PlusOp, A.MinusOp, A.TimesOp, A.DivideOp]
  val CompareOp = [A.LtOp, A.LeOp, A.GtOp, A.GeOp]
  val EqualityOp = [A.EqOp, A.NeqOp]
  datatype binOpType = Arith | Compare | Equality

  fun inList(elem, []) = false
    | inList(elem, x::xs) = if elem=x then true else inList(elem, xs)

  fun typeOfOp(oper) =
      if inList(oper, ArithOp) then Arith
      else if inList(oper, CompareOp) then Compare
      else Equality

  (* Check what happens with NONE (can we assume it won't happen?) *)
  (* fun actualTy(tenv:tenv, Ty.NAME(a,ref NONE)) = Ty.TOP *)
  (*   | actualTy(tenv:tenv, Ty.NAME(a,b)) = actualTy(tenv, valOf(!b)) *)
  (*   | actualTy(tenv, a) = a *)


  fun transExp(venv:venv, tenv:tenv) =
      let
	fun check(expr:A.exp, targetType, pos, msg) =
	    if(not(istype(#ty(trexp(expr)), actualTy(targetType))))
	    then E.error(pos, pos, msg) else ()

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

	(* and actualTy(Ty.NAME(a,b)) = *)
	(*     (case b of NONE => (E.error(0,0,"WHAAAA????\n"); *)
	(* 			Ty.BOTTOM) *)
	(* 	     | SOME(t) => (case valOf(!t) *)
	(* 			    of Ty.NAME(c, d) => *)
	(* 			       (let *)
	(* 				 val foo = S.look(tenv, c) *)
	(* 			       in *)
	(* 				 (case foo *)
	(* 				  of NONE => (E.error(0,0,"WHAAAA????\n"); *)
	(* 					      Ty.BOTTOM) *)
	(* 				   | SOME(bar) => actualTy(bar)) *)
	(* 			       end) *)
	(* 			    | _ => valOf(!t))) *)
	(*   | actualTy(a) = a *)
	(*     (* let *) *)
	(*     (*   val bTy = valOf(!b) *) *)
	(*     (*   val bbTy = (case bTy of Ty.NAME(c, d) => S.look(tenv, c) *) *)
	(*     (* 			   | _ => SOME(bTy)) *) *)
	(*     (* in *) *)
	(*     (*   (case bbTy of SOME(t) => actualTy(t) *) *)
	(*     (* 		  | NONE => ) *) *)
	(*     (* end *) *)

	and actualTy(Ty.NAME(a,b)) =
	    (case !b of SOME(t) => actualTy(t)
		     | NONE => (E.error(0,0,"Undefined type.\n");
				Ty.BOTTOM))
	  | actualTy(a) = a


	(* trvar: Type-check the Variables *)
	and trvar(A.SimpleVar(id, pos)) =
	    (case S.look(venv, id)
	      of SOME(Env.VarEntry(ty)) =>
		 {exp=(), ty=actualTy((#ty(ty)))}

	       | _ => (E.error(pos, pos,
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
			NONE => (E.error(pos, pos,
					 "invalid field id:"^S.name(id));
				 {exp=(), ty=Ty.BOTTOM})
		     | SOME(t) =>
		       {exp=(), ty=t})
		 end)
	       | _ => (E.error(pos, pos, "accessing field of non-record variable:"^S.name(id));{exp=(), ty=Ty.BOTTOM}))
	  (* Array type variables *)
	  | trvar(A.SubscriptVar(var, e, pos)) =
	    (print("array-var\n");case (#ty(trvar(var)))
	      of Ty.ARRAY(a,b) =>
		 (check(e, Ty.INT, pos, "non-int subscript");
		  {exp=(), ty=actualTy(a)})
	      | _ => (E.error(pos, pos, "subscripting non-array variable");{exp=(), ty=Ty.BOTTOM}))
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
                                | NONE => E.error(pos, pos, "Type for array not found\n"));
                            tenv)
                        | _ => tenv))

        and accRecord([], tenv) = []
          | accRecord(field::fields, tenv) =
            case (S.look(tenv, (#typ(field))))
                of NONE => (E.error(#pos(field), #pos(field), "Type of field is not defined"); accRecord(fields, tenv))
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
			       then E.error(pos2, pos2,
		    			    "init value doesn't match provided type")
			       else ())
		    	    | NONE =>
		    	      E.error(pos2, pos2, "unknown type chosen for variable:"^S.name(name)))
		       | NONE => ());
		     trDecs(S.enter(venv, name,
				    Env.VarEntry({ty=initType})), tenv, ds))
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
		    then (E.error(0,0, "Cyclic type-def\n");
			  trDecs(venv, tenv, ds))
		    else trDecs(venv, tenv', ds)
		  )
		 end
	       (* Catch all *)
	       | _ => {venv=venv,tenv=tenv})
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
			NONE => E.error(pos, pos,
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
			NONE => E.error(pos, pos,
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
	      of SOME(Env.FunEntry({formals, result})) =>
		 (if(not(checkTypeList(args, formals)))
		  then E.error(pos, pos,
			       "function args don't match type")
		  else ();
		   {exp=(), ty=actualTy(result)})
	       | _ => (E.error(pos, pos,
			      "undefined function:"^S.name(func));
		       {exp=(), ty=Types.BOTTOM}))
	  (* Assignments, not too difficult *)
	  | trexp(A.AssignExp({var, exp, pos})) =
	    let
	      val varType= #ty(trvar(var))
	      val expType= #ty(trexp(exp))
	    in
	      (if(varType=expType) then ()
	       else E.error(pos,pos,"Assigning expression of type "
				    ^Ty.toString(expType)^
				    " to variable of type "^
				    Ty.toString(varType));
	       {exp=(), ty=Ty.UNIT})
	    end
	  (* Three kinds of sequences. non-last exp forced to unit  *)
	  | trexp(A.SeqExp([])) = ({exp=(), ty=Ty.UNIT})
	  | trexp(A.SeqExp((x, pos)::[])) = (trexp(x))
	  | trexp(A.SeqExp((x, pos)::xs)) =
	    if(#ty(trexp(x))=Ty.UNIT)
	    then (trexp(A.SeqExp(xs)))
	    else (E.error(pos, pos, "Need Unit type for non-final SeqExp expressions."); {exp=(), ty=Ty.BOTTOM})
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
	    let val venv'=S.enter(venv, sym, Env.VarEntry({ty=Ty.INT}))
	    in
	      (check(exp1, Ty.INT, pos,
		     "Low value in `for` must be of type INT.");
	       check(exp2, Ty.INT, pos,
		     "High value in `for` must be of type INT.");
	       (* sym cannot be assigned to. DO THAT! *)
	       (* This is essentially check which takes in extra
		venv param. Consider writing another func for this. *)
	       if (#ty(transExp(venv', tenv) exp3)<> Ty.UNIT)
	       then E.error(pos, pos,
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
                        of Ty.RECORD(r) => (* TODO: check record fields *)
                            {exp=(), ty=Ty.RECORD(r)}
                        | _ => (E. error(pos, pos, "type given is not a record");
                            {exp=(), ty=Ty.RECORD([], ref ())}))
                    |  NONE => (E.error(pos, pos, "record type not found");
                        {exp=(), ty=Ty.RECORD([], ref ())})))

          | trexp(A.ArrayExp({typ=typ, size=size, init=init, pos=pos})) =
                (print("array: "^S.name(typ)^"\n");
                check(size, Ty.INT, pos, "Size of array must be of type INT.");
                (case S.look(tenv, typ)
                    of NONE => (E. error(pos, pos, "array type not found");
                        {exp=(), ty=Ty.ARRAY(Ty.BOTTOM, ref ())})
                    | SOME(t) =>
                        (print("TYPE: "^Ty.toString(actualTy(t)) ^ "\n");
                        (case actualTy(t)
                        of Ty.ARRAY(ty, uniq) => (check(init, ty, pos,
                            "initial value does not match array type");
                            {exp=(), ty=actualTy(t)})
                        | _ => (E. error(pos, pos, "type given is not an array");
                            {exp=(), ty=Ty.ARRAY(Ty.BOTTOM, ref ())})))))
(*	  | trexp(_) = (E.error(0,0,"fell-off");{exp=(), ty=Ty.UNIT}) *)
			 (* trexp ENDS *)

      in
	trexp
      end

  fun transProg(e) = (transExp(Env.base_venv, Env.base_tenv) e;
		      print("Done\n"))

end

signature MAIN =
sig
  val run: string -> unit
end

structure Main :> MAIN =
struct
fun run(filename:string) = Semant.transProg(Parse.parse(filename))
end
