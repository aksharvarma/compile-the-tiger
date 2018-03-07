(* Should mipsframe know about the internals of Tree? *)
structure T = Tree

structure MipsFrame :> FRAME =
struct
datatype access = InReg of Temp.temp | InFrame of int

val FP = Temp.newTemp()
val wordSize = 4

(* a frame contains a name, a list of the accesses associated
   with each formal parameter, and the number of local variables
   allocated so far *)
type frame = {name:Temp.label,
	      formals: access list,
              locals: int ref}

fun exp(InFrame(k)) = (fn(ex) => T.MEM(T.BINOP(T.PLUS, ex, T.CONST(k))))
  | exp(InReg(t)) = (fn (ex) => T.TEMP t)

(* TODO:  write view shift instructions *)
fun newFrame({name: Temp.label, formals: bool list}) =
    let
        fun createAccesses([], index) = []
          | createAccesses(f::fs, index) =
                (* if the given boolean is true, then that parameter needs to
                   be stored in the frame, else it can be kept in a register *)
                (if f then InFrame(index * wordSize) else InReg(Temp.newTemp()))
                (* note: we always increase the index regardless of whether the
                   param will be stored in the frame or a register so as to reserve
                   space for all formal parameters *)
                ::createAccesses(fs, index + 1)
    in
        {name=name,
        formals= createAccesses(formals, 0),
        locals= ref 0}
    end

(* get the name of the given frame *)
fun name({name, formals, locals}) = name

(* get the list of accesses for the formal parameters of the given frame *)
fun formals({name, formals, locals}) = formals

(* allocate space in the given frame for a new local variable *)
fun allocLocal({name, formals, locals}) =
    (* increment the number of local variables allocated in the given frame *)
    (locals := !locals + 1;
    (* return a function that takes in a boolean and returns the correct
       access depending on the input *)
    (* TODO: should the locals variable only be incremented if we're going
       to store the local variable in the frame? (yes?)
       Should the incrementing happen when the function we're making gets called?
       that might produce dangerous things if the function is called more than once. (so no?)
       are there going to be any weird referencing problem with the locals ref
       since we're returning a function that will do the calculation later?
       function that we return could potentially return a different offset depending
       on at what point it's called - that seems like a problem.
       should we be dereferencing locals and doing the calculation now or later?
       how can we make the calculation outside the function but also only increment the
       variable depending on what the boolean is? *)
    fn (b) => if b then InFrame(~(!locals) * wordSize) else InReg(Temp.newTemp()))

end

(* We are only supporting an implementation for the MIPS architecture at this time *)
structure Frame :> FRAME = MipsFrame
