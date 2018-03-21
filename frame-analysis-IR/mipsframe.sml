(* Should mipsframe know about the internals of Tree? *)
structure T = Tree

structure MipsFrame :> FRAME =
struct
datatype access = InReg of Temp.temp | InFrame of int

val FP = Temp.newTemp()
val wordSize = 4
val RV = Temp.newTemp()

(* a frame contains a name, a list of the accesses associated
   with each formal parameter, and the number of local variables
   allocated so far *)
type frame = {name:Temp.label,
              formals: access list,
              locals: int ref}

datatype frag = PROC of {body: Tree.stm, frame: frame}
              | STRING of Temp.label * string

fun exp(InFrame(k)) = (fn(ex) => T.MEM(T.BINOP(T.PLUS, ex, T.CONST(k))))
  | exp(InReg(t)) = (fn (ex) => T.TEMP t)

fun printExp(msg, e) = (print(msg ^ "\n");
                        Printtree.printtree(TextIO.stdOut, e);
                        print("-------------\n"))

fun printAccess(InReg(t)) = print("in reg: t" ^ Int.toString(t) ^ "\n")
  | printAccess(InFrame(i)) = print("frame offset: " ^ Int.toString(i) ^ "\n")

fun printFrame({name, formals, locals}) =
        (printExp("Frame name", T.LABEL name);
         print("num formals: " ^ Int.toString(List.length(formals)) ^ "\n");
         (app printAccess formals);
         print("\nlocals: " ^ Int.toString(!locals) ^ "\n"))

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
    (* return a function that takes in a boolean and returns the correct
       access depending on the input *)
    (* when we call this, we always call the returned function immediately,
       but if this function were to be called more than once, it would assign
       a new location/temp for the variable *)
    fn (b) => if b
              then (locals := !locals + 1; InFrame(~(!locals) * wordSize))
              else InReg(Temp.newTemp())

(* C functions may need to drop first argument (sl) if translate passes it in *)
fun externalCall(s, args) = T.CALL(T.NAME(Temp.namedLabel(s)), args)

fun procEntryExit1(frame, body) =
    let
        fun moveArgs() = T.LABEL(Temp.namedLabel("entryExit1: moveArgs (step 4)"))
        (*
        fun moveArgs(i, []) = T.EXP(T.CONST 0)
          | moveArgs(i, InReg(t)::[]) =
            (* includes the sl as arg 0 *)
            if i <= 4
            then T.MOVE(T.TEMP t, r(i))

          | moveArgs(a::as) =
          *)
        fun storeCalleeSaves() = T.LABEL(Temp.namedLabel("entryExit1: storeCalleeSaves (step 5)"))
        fun restoreCalleeSaves() = T.LABEL(Temp.namedLabel("entryExit1: restoreCalleeSaves (step 8)"))
        fun combine(stm1, stm2, stm3, stm4) =
                T.SEQ(stm1, T.SEQ(stm2, T.SEQ(stm3, stm4)))
    in
        combine(moveArgs(), storeCalleeSaves(), body, restoreCalleeSaves())
    end

end



(* We are only supporting an implementation for the MIPS architecture at this time *)
structure Frame :> FRAME = MipsFrame
