structure Types =
struct

  type unique = unit ref

  datatype ty =
	   TOP			(* Catch-all, invalid type *)
	   | RECORD of (Symbol.symbol * ty) list * unique
           | NIL
           | INT
           | STRING
           | ARRAY of ty * unique
	   | NAME of Symbol.symbol * ty option ref
	   (* NAME is Symbol.symbol * ((ty option) ref) *)
	   | UNIT		(* valueless expressions *)
	   | BOTTOM		(* Used only for `break` *)
           | UNASSIGNABLE (* Subtype of int *)
	       
	      
  fun isSubtype(BOTTOM, ty2) = true    (* subtype of everything *)
    | isSubtype(NIL, RECORD(_)) = true (* every NIL is a RECORD *)
    | isSubtype(UNASSIGNABLE, INT) = true (* unassignable is a subtype of int *)
    | isSubtype(_, TOP) = true	       (* TOP is supertype *)
    | isSubtype(a, b) = a=b	       (* handle a=a *)
			    
  fun join(NIL, RECORD(a)) = RECORD(a)
    | join(UNASSIGNABLE, INT) = INT
    | join(BOTTOM, a) = a
    | join(a,b)  = if (a=b) then a else TOP

  fun toString(TOP) = "INVALID"
    | toString(RECORD(_)) = "Record"
    | toString(ARRAY(_)) = "Array"
    | toString(NIL) = "NIL"
    | toString(INT) = "Int"
    | toString(UNASSIGNABLE) = "Unassignable"
    | toString(STRING) = "String"
    | toString(UNIT) = "Unit"
    | toString(BOTTOM) = "Bottom"
    | toString(NAME(sym, ty))  = "NAME("^Symbol.name(sym)^")"
    (* | toString(_) = "___" *)

			   (* fun meet([], _) = Ty.BOTTOM *)
  (*   | meet(_, []) = Ty.BOTTOM *)
  (*   | meet(x::xs, x::xs) = *)

end

