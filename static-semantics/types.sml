structure Types =
struct

  type unique = unit ref

  datatype ty =  RECORD of (Symbol.symbol * ty) list * unique
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
    | isSubtype(a, b) = a=b	       (* handle a=a *)

  fun toString(RECORD(_)) = "Record"
    | toString(ARRAY(_)) = "Array"
    | toString(NIL) = "NIL"
    | toString(INT) = "Int"
    | toString(UNASSIGNABLE) = "Unassignable"
    | toString(STRING) = "String"
    | toString(UNIT) = "Unit"
    | toString(BOTTOM) = "Bottom"
    | toString(NAME(sym, ty))  = "NAME("^Symbol.name(sym)^")"
end

