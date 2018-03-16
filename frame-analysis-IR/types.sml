structure Types =
struct

  type unique = unit ref

  datatype ty =  RECORD of (Symbol.symbol * ty) list * unique
           | NIL
           | INT
           | STRING
           | ARRAY of ty * unique
           | NAME of Symbol.symbol * ty option ref
           | UNIT               (* valueless expressions *)
           | BOTTOM             (* Used only for `break` *)
           | UNASSIGNABLE (* Subtype of int *)

  (* This is the function that defines the lattice structure
     RECORD, ARRAY, INT, STRING, UNIT are all distinct types
     NIL is a subtype of every RECORD
     UNASSIGNABLE is a special type of INT (used for the loop variable)
     BOTTOM is a subtype of everything (used for break statement)
   *)
  fun isSubtype(BOTTOM, ty2) = true    (* subtype of everything *)
    | isSubtype(NIL, RECORD(_)) = true (* every NIL is a RECORD *)
    | isSubtype(UNASSIGNABLE, INT) = true (* int, but can't assign *)
    | isSubtype(a, b) = a=b            (* handle a is subtype of a *)

  (* Helper function to print type of an expression. *)
  fun toString(RECORD(_)) = "Record"
    | toString(ARRAY(_)) = "Array"
    | toString(NIL) = "Nil"
    | toString(INT) = "Int"
    | toString(UNASSIGNABLE) = "Unassignable"
    | toString(STRING) = "String"
    | toString(UNIT) = "Unit"
    | toString(BOTTOM) = "Bottom"
    | toString(NAME(sym, ty))  = "NAME("^Symbol.name(sym)^")"
end

