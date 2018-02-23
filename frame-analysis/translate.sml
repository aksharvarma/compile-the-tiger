(* Dummy Translate module to be fleshed out in later assignment *)
structure Translate =
struct

type exp = unit
type level = int
type access = level * (Frame.access option)
			
val outermost = 0
fun newLevel({parent:level, name: Temp.label, formals:bool list}) =
    (Frame.newFrame({name=name, formals=formals});
     parent+1)

fun formals(level) = []

fun allocLocal(level) = (fn b => (0, NONE))
		    
		  
end
