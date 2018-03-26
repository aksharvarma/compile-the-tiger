(* This is the module that contains all the machine specific details
 * of MIPS needed to go from AST to IR tree (and for later phases)
 *
 * It is used to implement the following functionalities:
 * -  Abstracts out how variables need to be accessed
 *    (via temp registers or somewhere in the stack frame).
 * - Has the global FramePointer and ReturnValue temp regs (FP and RV).
 * - Abstracts out how the frame looks like.
 *   Other modules use functions to get at this functionality.
 * - Has the fragments datatype which is used later to finish compiling
 * - procEntryExit is used to add prologue and epilogue to functions.
 *)
structure T = Tree

structure MipsFrame :> FRAME =
struct

(* The wordSize of the machine in bytes *)
val wordSize = 4

(* This determines where a variables stays.
 * - In a temp register (InReg)
 * - In the frame (InFrame)
 *)
datatype access = InReg of Temp.temp | InFrame of int

(* The frame pointer register. This is fixed for all frames *)
val FP:Temp.temp = Temp.newTemp()
(* The return value register. This is fixed for all frames *)
val RV:Temp.temp = Temp.newTemp()

(* A stack frame contains the following information
 * - A label to the start of the function (check this)
 * - A list of the accesses associated with each formal parameter
 * - The number of escaping local variables allocated so far
 *
 * This frame type contains all the information needed to finish the
 * frame in later stages of the compiler.
 * What isn't available in this type, is either available elsewhere or
 * can be computed easily.
 *)
type frame = {name:Temp.label,
              formals: access list,
              locals: int ref}

(* The final translation is going to be a list of these fragments
 * - PROC are procedures which have the code and associated frame
 * - STRING is a string literal that is stored at given label
 *
 * This list is enough to finish later compiler stages.
 *
 * Every function and its body becomes a PROC frag
 * - The body is in the Tree.stm
 * - The frame information needed later is present in frame.
 * These fully determine whatever we need for a function in assembly
 *
 * At assembly level a string is a label and the string literal
 * This information is also available in the STRING frag.
 * The STRING frags will be added to the code in a later phase.
 *)
datatype frag = PROC of {body: Tree.stm, frame: frame}
              | STRING of Temp.label * string

(* exp: access -> Tree.exp -> Tree.exp
 *
 * Given an access, it returns a function that takes a static link
 * and returns the location of a variable
 * - either the temp reg as a Tree.exp, or
 * - the memory location in stack (correctly offset)
 *
 * Need this because Translate shouldn't know machine specific details.
 * Thus, it needs this interface to correctly use InFrame/InReg.
 *)
fun exp(InFrame(k)) = (fn(ex) => T.MEM(T.BINOP(T.PLUS, ex, T.CONST(k))))
  | exp(InReg(t)) = (fn (ex) => T.TEMP t)


(* Printing functions for debugging *)
(* printExp: string * Tree.exp -> unit *)
fun printExp(msg, e) = (print(msg ^ "\n");
                        Printtree.printtree(TextIO.stdOut, e);
                        print("-------------\n"))

(* printAccess: access -> unit *)
fun printAccess(InReg(t)) = print("in reg: t"^Int.toString(t)^"\n")
  | printAccess(InFrame(i)) = print("frame offset: "^Int.toString(i)^"\n")

(* printFrame: frame -> unit *)
fun printFrame({name, formals, locals}) =
    (printExp("Frame name", T.LABEL name);
     print("num formals: "^Int.toString(List.length(formals))^"\n");
     (app printAccess formals);
     print("\nlocals: " ^ Int.toString(!locals) ^ "\n"))

(* newFrame: {name: Temp.label, formals: bool list} -> frame
 *
 * This creates a new frame for a function.
 * - name is the label of the function
 * - formals is a list of bools denoting whether the formals escape.
 *   The static link is always added to this list as an escaping arg.
 *
 * This is the function that will actually do the view shift
 * It is not being done now because we need many more machine specific
 * details including the names for various special registers.
 * Since this is not available as of now, we defer the view shift
 * implementation until later.
 *)
fun newFrame({name: Temp.label, formals: bool list}) =
    let
      (* createAccesses: bool list * int -> access list
       *
       * Returns an access for the formal given whether it escapes.
       * Escaping formals go in the frame, else they stay in regs.
       *
       * Note: We always increment the index regardless of where the
       * param is stored so as to reserve space for all formal params.
       * This helps with having >4 params because we always know the
       * offset for the kth argument of the function enabling later access
       * It is also useful to have space to move args into if we want
       * to use the arg registers to call other functions because we
       * know the physical registers used only post regalloc.
       *
       * This implementation keeps things reasonably flexible to refine
       * in later phases of the project.
       *)
      fun createAccesses([], index) = []
        | createAccesses(f::fs, index) =
          (* Choose where to keep variable *)
          (if f then InFrame(index * wordSize)
           else InReg(Temp.newTemp()))
          (* always increment index.*)
          ::createAccesses(fs, index + 1)
    in
      (* 0 offset is the static link. It's where the FP points to *)
      {name=name,
       formals= createAccesses(formals, 0),
       locals= ref 0}
    end

(* name: frame -> Temp.label
 * Get the name (label) of the given frame (function) *)
fun name({name, formals, locals}) = name

(* formals: frame -> access list
 * Get list of accesses for formal parameters of the given frame  *)
fun formals({name, formals, locals}) = formals

(* allocLocal: frame -> bool -> access
 *
 * Make an access for a new local variable in a frame
 *
 * Note 1: We only allocate space in the stack for escaping locals
 * Otherwise they go in regs and are saved by the callee/caller saves
 * convention.
 *
 * Note 2: Although we return a function, it's usage is very restricted
 * in the sense that it is immediately called and is only called once.
 * This allows performing the increment of count in that function.
 * The curried form of the function is based on the signature given
 * in Chapter 6 of the book. Our usage doesn't need a curried function.
 *
 * Warning wrt Note 2!
 * Don't call the returned function twice for the same local,
 * it will create a new temp and a new frame location unnecessarily.
 *
 * Note 3: In the final signature for the Frame module provided in the
 * book at pg 260, chapter 12, the allocLocal function has the
 * following signature:
 * allocLocal: frame -> int
 *
 * We keep the earlier signature until we find out why the signature
 * changes later.
 *)
fun allocLocal({name, formals, locals}) =
    fn (b) => if b
              then (locals := !locals + 1;
                    InFrame(~(!locals) * wordSize))
              else InReg(Temp.newTemp())

(* externalCall: string * Tree.exp list -> Tree.exp
 *
 * We use the simple form given in the book for now and defer further
 * refinement and details until a later stage when we know exactly how
 * the called external functions's MIPS assembly code looks like.
 *
 * Since all external functions are implemented in C, we do not call
 * this function with any static link and only pass in the formals.
 * Also, Temp.namedLabel has been modified to have a 'tig_' prefix
 * which seems to be what the final assembly labels will be.
 * Based on linking conventions mentioned in:
 * https://course.ccs.neu.edu/csu4410/runtime/
 *)
fun externalCall(s, args) = T.CALL(T.NAME(Temp.namedLabel(s)), args)

(* procEntryExit1: frame * Tree.stm -> Tree.stm
 *
 * This is the function that adds the prologue and epilogue to the code
 * of the function. Currently it is quite barren and will be filled in
 * at a later stage when we have more details of MIPS (esp. registers)
 *
 * The required code will come in the following helper functions:
 * - moveArgs: will move escaping args (including SL) into frame
   and others into fresh temporary registers.
 * - storeCalleeSaves: store the callee-saves registers in frame
 * - restoreCalleeSaves: restore the callee-saves registers from frame
 * - combine: combine prologue+body+epilogue
 *)
fun procEntryExit1(frame, body) =
    let
      (* will move escaping args (including SL) into frame
       * and others into fresh temporary registers. *)
      fun moveArgs() =
          T.LABEL(Temp.namedLabel(Symbol.name(name(frame))^"_entryExit1_moveArgs_step4"))

      (* store the callee-saves registers in frame *)
      fun storeCalleeSaves() =
          T.LABEL(Temp.namedLabel(Symbol.name(name(frame))^"_entryExit1_storeCalleeSaves_step5"))
      (* restore the callee-saves registers from frame *)
      fun restoreCalleeSaves() =
          T.LABEL(Temp.namedLabel(Symbol.name(name(frame))^"_entryExit1_restoreCalleeSaves_step8"))
      (* combine prologue+body+epilogue *)
      fun combine(moveArgsStm, storeRegsStm, bodyStm, restoreRegsStm) =
          T.SEQ(moveArgsStm,
                T.SEQ(storeRegsStm,
                      T.SEQ(bodyStm, restoreRegsStm)))
    in
      combine(moveArgs(),
              storeCalleeSaves(),
              body,
              restoreCalleeSaves())
    end

(* TODO: To be filled in later *)
fun string(lab, str) = (Symbol.name(lab) ^ ": .asciiz \"" ^ str ^ "\"\n")

                                                                   
(* Registers *)
type register = string
                  
val specialRegs:(register * Temp.temp) list = [("$v0", RV),
                                               ("$zero", Temp.newTemp()),
                                               ("$ra", Temp.newTemp()),
                                               ("$sp", Temp.newTemp())]

val argRegs:(register * Temp.temp) list = [("$v1", Temp.newTemp()),
                                           ("$a0", Temp.newTemp()),
                                           ("$a1", Temp.newTemp()),
                                           ("$a2", Temp.newTemp()),
                                           ("$a3", Temp.newTemp())]

val calleeSaves:(register * Temp.temp) list = [("$s0", Temp.newTemp()),
                                               ("$s1", Temp.newTemp()),
                                               ("$s2", Temp.newTemp()),
                                               ("$s3", Temp.newTemp()),
                                               ("$s4", Temp.newTemp()),
                                               ("$s5", Temp.newTemp()),
                                               ("$s6", Temp.newTemp()),
                                               ("$s7", Temp.newTemp()),
                                               ("$r30", Temp.newTemp())]
(* The last thing in the calleeSaves list is actually the fp. But we do not use
 *   *)
                                                
val callerSaves:(register * Temp.temp) list = [("$t0", Temp.newTemp()),
                                               ("$t1", Temp.newTemp()),
                                               ("$t2", Temp.newTemp()),
                                               ("$t3", Temp.newTemp()),
                                               ("$t4", Temp.newTemp()),
                                               ("$t5", Temp.newTemp()),
                                               ("$t6", Temp.newTemp()),
                                               ("$t7", Temp.newTemp()),
                                               ("$t8", Temp.newTemp()),
                                               ("$t9", Temp.newTemp())]

(* These are reserved registers  *)
val reservedRegs:(register * Temp.temp) list = [("$at", Temp.newTemp()),
                                               ("$k0", Temp.newTemp()),
                                               ("$k1", Temp.newTemp()),
                                               ("$gp", Temp.newTemp())]


fun getRegNum(str, tempNum) = tempNum
                                  
(* val tab = Temp.Table *)
val tempMap:register Temp.Table.table =
    foldr (fn ((str, n), table) => Temp.Table.enter(table, n, str))
          Temp.Table.empty
          (specialRegs@argRegs@calleeSaves@callerSaves@reservedRegs)

                                          
end
structure Frame:>FRAME = MipsFrame
