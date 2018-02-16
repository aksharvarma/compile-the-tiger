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
	   | UNIT		(* valueless expressions *)
	   | BOTTOM		(* Used only for `break` *)
	       
	      
  fun isSubtype(BOTTOM, ty2) = true    (* subtype of everything *)
    | isSubtype(NIL, RECORD(_)) = true (* every NIL is a RECORD *)
    | isSubtype(_, TOP) = true	       (* TOP is supertype *)
    | isSubtype(a, b) = a=b	       (* handle a=a *)
			    
  fun join(NIL, RECORD(a)) = RECORD(a)
    | join(BOTTOM, a) = a
    | join(a,b)  = if (a=b) then a else TOP

  fun toString(TOP) = "INVALID"
    | toString(RECORD(_)) = "Record"
    | toString(ARRAY(_)) = "Array"
    | toString(NIL) = "NIL"
    | toString(INT) = "Int"
    | toString(STRING) = "String"
    | toString(UNIT) = "Unit"
    | toString(BOTTOM) = "Bottom"
    | toString(_) = "___"

			   (* fun meet([], _) = Ty.BOTTOM *)
  (*   | meet(_, []) = Ty.BOTTOM *)
  (*   | meet(x::xs, x::xs) = *)

end

