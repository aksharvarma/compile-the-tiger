structure A = Absyn
structure Ty = Types
structure S = Symbol
structure E = ErrorMsg
val error = E.error

structure Translate = struct type exp = unit end

signature SEMANT =
sig
  type venv = Env.enventry S.table
  type tenv = Ty.ty S.table

  type expty = {exp: Translate.exp, ty: Ty.ty}

  val transVar: venv * tenv * A.var -> expty
  val transExp: venv * tenv -> A.exp -> expty
  val transDec: venv * tenv * A.dec -> {venv:venv, tenv:tenv}
  val transTy: tenv * A.exp -> Ty.ty

  val transProg : A.exp -> unit
end

structure Semant :> SEMANT =
struct
  type venv = Env.enventry S.table
  type tenv = Ty.ty S.table

  type expty = {exp: Translate.exp, ty: Ty.ty}

  fun isInt({exp,ty}) =
      case ty of Ty.INT => true
	       | _ => false
			
  fun isString({exp,ty}) =
      case ty of Ty.STRING => true
	       | _ => false
			
  fun check(b, pos, msg) =
      if(b) then ()
      else E.error(pos, pos, msg)

  fun transVar(a,b,c) = {exp=(), ty=Ty.UNIT}

  fun transExp(venv:venv, tenv:tenv) =
      let
	fun checkTwoInt(left:A.exp, right:A.exp, pos) = 
	    (check(isInt(trexp(left)), pos, "integer required");
	     check(isInt(trexp(right)), pos, "integer required");
	     {exp=(), ty=Ty.INT})
	and checkTwoIntsOrStrings(left:A.exp, right:A.exp, pos) =
	    let
	      val b = (isInt(trexp(left)) andalso isInt(trexp(right)))
		      orelse (isString(trexp(left))
			      andalso isString(trexp(right)))
	    in
	      (check(b, pos, "two ints or two strings required");
	       {exp=(), ty=Ty.INT})
	    end
	and
	trexp(A.OpExp{left, oper=A.PlusOp, right, pos}) =
	    checkTwoInt(left, right, pos)
	| trexp(A.OpExp{left, oper=A.MinusOp, right, pos}) =
	    checkTwoInt(left, right, pos)
	| trexp(A.OpExp{left, oper=A.TimesOp, right, pos}) =
	    checkTwoInt(left, right, pos)
	| trexp(A.OpExp{left, oper=A.DivideOp, right, pos}) =
	    checkTwoInt(left, right, pos)
	| trexp(A.OpExp{left, oper=A.LtOp, right, pos}) =
	    checkTwoIntsOrStrings(left, right, pos)
	| trexp(A.OpExp{left, oper=A.LeOp, right, pos}) =
	    checkTwoIntsOrStrings(left, right, pos)
	| trexp(A.OpExp{left, oper=A.GtOp, right, pos}) =
	    checkTwoIntsOrStrings(left, right, pos)
	| trexp(A.OpExp{left, oper=A.GeOp, right, pos}) =
	    checkTwoIntsOrStrings(left, right, pos)
	| trexp(A.StringExp(s,pos)) = {exp=(), ty=Ty.STRING} 
	| trexp(A.IntExp(a))= {exp=(), ty=Ty.INT}
	| trexp(_) = (error(0,0,"fell-off");{exp=(), ty=Ty.UNIT})
      in
	trexp
      end

  fun transDec(a:venv,
	       b:tenv,
	       c:A.dec) = {venv=S.empty,
			       tenv=S.empty}
  fun transTy(a,b) = Ty.UNIT

  fun transProg(e) = (transExp(Env.base_venv, Env.base_tenv) e;print("done"))
		 
end
  
signature MAIN =
sig
  val run: string -> unit
end

structure Main :> MAIN =
struct
fun run(filename:string) = Semant.transProg(Parse.parse(filename))
end
