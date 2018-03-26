(* make this an abstraction sometime *)
structure Temp : TEMP =
struct
type temp = int
val temps = ref 100
fun newTemp() = let val t = !temps in temps := t+1; t end

structure Table = IntMapTable(type key = int
                              fun getInt n = n)

fun makeString t = "t" ^ Int.toString t

                                      
type label = Symbol.symbol

local structure F = Format
      fun postinc x = let val i = !x in x := i+1; i end
      val labs = ref 0
in
fun newLabel() = Symbol.symbolize(F.format "L%d" [F.INT(postinc labs)])

(* namedLabel function modified to conform with the linking conventions
 * mentioned in the relevant section on the runtime page.
 * https://course.ccs.neu.edu/csu4410/runtime/
 *)
val namedLabel = (fn s => Symbol.symbolize("tig_"^s))
end


end
