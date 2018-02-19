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

  (* datatype foo = bar of int * string option ref *)
  (* 	       | baz of int *)

  (* fun testing(bar(i,s)) = ("bar", i, !s) *)
  (*    |testing(baz(i)) = ("baz", i, NONE) *)
	     
  fun actualTy(tenv:tenv, Ty.NAME(a,b)) = actualTy(tenv, valOf(!b))
    | actualTy(tenv, a) = a

  (* fun actualTy(tenv:tenv, ty:S.symbol) = *)
  (*     case S.look(tenv, ty) *)
  (*      of NONE => NONE *)
  (* 	| SOME(Ty.NAME(a, ref(SOME(Ty.NAME(b, c))))) => actualTy(tenv, b) *)
  (* 	| SOME(Ty.NAME(a, ref(NONE))) => NONE *)
  (* 	| SOME(a) => SOME(a) *)


  (* fun transTy(tenv, tydec) = *)
  (*     case #ty(tydec) of *)
  (* 	  A.NameTy(sym, pos) => S.enter(tenv,  *)
      
  fun transExp(venv:venv, tenv:tenv) =
      let
	fun check(expr:A.exp, targetType, pos, msg) =
	    if(not(istype(#ty(trexp(expr)), targetType)))
	    then E.error(pos, pos, msg) else ()

	and findType(expr:A.exp, []) = Ty.TOP
	  | findType(expr:A.exp, ty::tyList) =
	    if(istype(#ty(trexp(expr)), ty)) then ty
	    else findType(expr:A.exp, tyList)
	
	and checkTwo(left:A.exp, right:A.exp, targetType, pos, msg) = 
	    (check(left, targetType, pos, msg);
	     check(right, targetType, pos, msg))

	and checkTypeList([], []) = true
	  | checkTypeList([], _) = false
	  | checkTypeList(_, []) = false
	  | checkTypeList(e::es, t::ts) = #ty(trexp(e)) = t andalso
					 checkTypeList(es, ts)
	    
	and findField([], id) = Ty.TOP
	  | findField((x,ty)::xs, id) = if(id=x) then ty
					else findField(xs, id)

	(* trvar: Type-check the Variables *)
	and trvar(A.SimpleVar(id, pos)) =
	    (print("simple-var\n");
	     case S.look(venv, id)
	      of SOME(Env.VarEntry(ty)) =>
		 {exp=(), ty=actualTy(tenv, (#ty(ty)))}
		   
	       | _ => (E.error(pos, pos,
				  "undefined variable:"^S.name(id));
			 {exp=(), ty=Types.TOP}))
	  (* fields and records *)
	  | trvar(A.FieldVar(var, id, pos)) =
	    (print("field-var\n");
	     case (#ty(trvar(var)))
	      of Ty.RECORD(a,b) =>
		 (let
		   val fieldType=findField(a, id)
		 in
		   (if fieldType=Ty.TOP
		    then E.error(pos, pos, "invalid field id:"^S.name(id))
		    else ();
		    {exp=(), ty=fieldType})
		 end)
	       | _ => (E.error(pos, pos, "accessing field of non-record variable:"^S.name(id));{exp=(), ty=Ty.TOP}))
	  (* Array type variables *)
	  | trvar(A.SubscriptVar(var, e, pos)) =
	    (print("array-var\n");case (#ty(trvar(var)))
	      of Ty.ARRAY(a,b) =>
		 (check(e, Ty.INT, pos, "non-int subscript");
		  {exp=(), ty=actualTy(tenv, a)})
	      | _ => (E.error(pos, pos, "subscripting non-array variable");{exp=(), ty=Ty.TOP}))
	(* trvar ENDS *)

	(* Helper for adding headers for tydecs *)
	(* and addTypeHeads([]) = tenv *)
	(*   | addTypeHeads(x::xs) = S.enter(addTypeHeads(xs), *)
	(* 				  (#name x), *)
	(* 				  Ty.NAME((#name x), *)
	(* 					  ref NONE)) *)
	      
	(* trDecs: type check declarations *)
	and trDecs(a:venv, b:tenv, []) = {venv=a,tenv=b}
	  | trDecs(a:venv, b:tenv, d::ds) =
	    (case d
		    (* normal variables *)
	      of A.VarDec({name, escape, typ, init, pos}) =>
		 let
		   val initType = #ty(transExp(a, b) init)
		 in
		   ((case typ of
		    	 SOME(ty, pos2) =>
		    	 (case S.look(b, ty) of
		    	      SOME(t) => check(init, t, pos2,
		    			       "init value doesn't match provided type")
		    	    | NONE =>
		    	      E.error(pos2, pos2, "unknown type chosen for variable:"^S.name(name)))
		       | NONE => ());
		     trDecs(S.enter(a, name,
				    Env.VarEntry({ty=initType})), b, ds))
		 end
	       (* Types *)
	       (* | A.TypeDec(tyList) => *)
	       (* 	 (addTypeHeads(tyList); *)
	       (* 	  map transTy tyList; *)
	       (* 	  {tenv=tenv, venv=venv} *)
	       (* 	 ) *)
				      
	       (* Catch all *)
	       | _ => {venv=a,tenv=b})
	(* trDecs ENDS *)
	      

	(* The actual workhorse *)
	and trexp(A.OpExp{left, oper=someOp, right, pos}) =
	    (* Handle Arithmetic ops: +,-,*,/ *)
	    (print("opExp\n");case typeOfOp(someOp) of 
		 Arith =>
		 (print("Arith\n");checkTwo(left, right, Ty.INT, pos,
			   "integer required");
		  {exp=(), ty=Ty.INT})
			       (* comparison ops: <, <=, >=, > *)
	       | Compare =>
		 let val leftType = findType(left, [Ty.STRING, Ty.INT])
		 in
		   (print("Compare\n");
		    if (leftType=Ty.TOP)
		    then
		      E.error(pos, pos, "int or string required")
		    else ();
		    (check(right, leftType,
			   pos, "both expressions should be int or string");
		     {exp=(), ty=Ty.INT}))
		 end
	       (* Handle equality checks: =, <> *)
	       | Equality => 
		 let
		   val leftType = findType(left, [Ty.INT,
						  Ty.STRING])
		 in
		   (print("Equality\n");
		    if (leftType=Ty.TOP)
		    then
		      E.error(pos, pos, "can't check equality with this type")
		    else () ;
		    (check(right, leftType,
			   pos, "both expressions should have same type");
		     {exp=(), ty=Ty.INT}))
		 end)
	  (* Integers, strings, nils *)
	  | trexp(A.IntExp(number)) = (print("Int\n");
				       {exp=(), ty=Ty.INT})
	  | trexp(A.StringExp(string)) = (print("String:"^(#1(string))^"\n");
					  {exp=(), ty=Ty.STRING})
	  | trexp(A.NilExp) = {exp=(), ty=Ty.NIL}
	  (* variables, outsourced to trvar *)
	  | trexp(A.VarExp(var)) = (print("VarExp\n");trvar(var))
	  (* Function calls *)
	  | trexp(A.CallExp({func, args, pos})) = 
	    (print("calling:"^S.name(func)^"\n");
	     case S.look(venv, func)
	      of SOME(Env.FunEntry({formals, result})) =>
		 (if(not(checkTypeList(args, formals)))
		  then E.error(pos, pos,
			       "function args don't match type")
		  else ();
		   {exp=(), ty=result})
	       | _ => (E.error(pos, pos,
			      "undefined function:"^S.name(func));
		       {exp=(), ty=Types.TOP}))
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
	  | trexp(A.SeqExp([])) = (print("empty-Seq\n");
				   {exp=(), ty=Ty.UNIT})
	  | trexp(A.SeqExp((x, pos)::[])) = (print("single-Seq\n");
					     trexp(x))
	  | trexp(A.SeqExp((x, pos)::xs)) =
	    if(#ty(trexp(x))=Ty.UNIT)
	    then (print("Seq\n");
		  trexp(A.SeqExp(xs)))
	    else (E.error(pos, pos, "Need Unit type for non-final SeqExp expressions."); {exp=(), ty=Ty.TOP})
	  (* Let expressions. Decs outsourced to trDecs *)
	  | trexp(A.LetExp({decs, body, pos})) =
	    let
	      val {venv=venv', tenv=tenv'}=trDecs(venv, tenv, decs)
	    in (print("Let\n");
		transExp(venv', tenv') body)
	    end
	  | trexp(_) = (E.error(0,0,"fell-off");{exp=(), ty=Ty.UNIT})
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
