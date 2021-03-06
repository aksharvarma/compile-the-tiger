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
structure MipsFrame :> FRAME =
struct

structure T = Tree

(* The wordSize of the machine in bytes *)
val wordSize = 4

(* This determines where a variables stays.
 * - In a temp register (InReg)
 * - In the frame (InFrame)
 *)
datatype access = InReg of Temp.temp | InFrame of int

(* The frame pointer register. This is fixed for all frames *)
val FP:Temp.temp = Temp.newTemp()
(* The stack pointer register. This is fixed for all frames *)
val SP:Temp.temp = Temp.newTemp()
(* The return value register. This is fixed for all frames *)
val RV:Temp.temp = Temp.newTemp()
val zero:Temp.temp = Temp.newTemp()

(* Pulls out the temp values for the argument registers.
 * Used for referencing the correct temp when assigning arguments
 * to arg regs
 *)
val a0 = Temp.newTemp()
and a1 = Temp.newTemp()
and a2 = Temp.newTemp()
and a3 = Temp.newTemp()
and ra = Temp.newTemp()

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
              maxOutgoing: int ref,
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
fun printFrame({name, formals, maxOutgoing, locals}) =
    (printExp("Frame name", T.LABEL name);
     print("num formals: "^Int.toString(List.length(formals))^"\n");
     (app printAccess formals);
     print("\nmax outgoing args: " ^Int.toString(!maxOutgoing));
     print("\nlocals: " ^ Int.toString(!locals) ^ "\n"))

(* newFrame: {name: Temp.label, formals: bool list} -> frame
 *
 * This creates a new frame for a function.
 * - name is the label of the function
 * - formals is a list of bools denoting whether the formals escape.
 *   The static link is always added to this list as an escaping arg.
 * - maxOutgoing is the max number of outgoing arguments
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
       formals=createAccesses(formals, 0),
       maxOutgoing= ref 0,
       locals= ref 0}
    end

(* name: frame -> Temp.label
 * Get the name (label) of the given frame (function) *)
fun name({name, formals, maxOutgoing, locals}) = name

(* formals: frame -> access list
 * Get list of accesses for formal parameters of the given frame  *)
fun formals({name, formals, maxOutgoing, locals}) = formals

(* setOutgoingArgs: frame * int -> unit
 * Set the max outgoing args for the given frame if n is larger than the current
 * max *)
fun setOutgoingArgs({name, formals, maxOutgoing, locals}, n) =
    if n > !maxOutgoing then maxOutgoing := n else ()

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
fun allocLocal({name, formals, maxOutgoing, locals}) =
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

(* string: Temp.label * string -> string
 *
 * Translates a string literal to the correct assembly form
 *)
fun string(lab, str) = (Symbol.name(lab) ^ ":\n.word "^Int.toString(String.size(str))
                        ^"\n.ascii \"" ^ str ^ "\"\n")

(* Registers *)
type register = string

(* Includes return value ($v0), zero reg ($zero), return address ($ra), and
 * stack pointer ($sp)
 *)
val specialRegs:(register * Temp.temp) list = [("$v0", RV),
                                               ("$zero", zero),
                                               ("$ra", ra),
                                               ("$sp", SP)]
(* Argument registers: $a0-$a3 *)
val argRegs:(register * Temp.temp) list = [("$a0", a0),
                                           ("$a1", a1),
                                           ("$a2", a2),
                                           ("$a3", a3)]

(* Callee saved registers: $s0-$s7.
 * This list also includes the frame pointer register ($r30) and the second
 * return value register ($v1=$r3) as they will not be used for their
 * special purposes
 *)
val calleeSaves:(register * Temp.temp) list = [("$s0", Temp.newTemp()),
                                               ("$s1", Temp.newTemp()),
                                               ("$s2", Temp.newTemp()),
                                               ("$s3", Temp.newTemp()),
                                               ("$s4", Temp.newTemp()),
                                               ("$s5", Temp.newTemp()),
                                               ("$s6", Temp.newTemp()),
                                               ("$s7", Temp.newTemp()),
                                               ("$r30", Temp.newTemp()),
                                               ("$r3", Temp.newTemp())]

(* Caller saved registers: $t0-$t9 *)
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

(* These are reserved registers that cannot be used by the program  *)
val reservedRegs:(register * Temp.temp) list = [("$at", Temp.newTemp()),
                                                ("$k0", Temp.newTemp()),
                                                ("$k1", Temp.newTemp()),
                                                ("$gp", Temp.newTemp())]

(* Some useful lists of registers *)
val allUserRegs = specialRegs@argRegs@calleeSaves@callerSaves@reservedRegs
val allUserRegs = specialRegs@argRegs@calleeSaves@callerSaves
val physicalRegsT = map (fn (s, t) => t) allUserRegs
val trashedByCall = ra::RV::(map (fn (s, t) => t) (argRegs@callerSaves))

(* tempMap: register Temp.Table.table
 *
 * Maps temp numbers to their string names if they correspond to a special
 * register
 *)
val tempMap:register Temp.Table.table =
    foldr (fn ((str, n), table) => Temp.Table.enter(table, n, str))
          Temp.Table.empty allUserRegs

(* findTemp: string -> Temp.temp
 *
 * Given a string representation of a register, find corresponding temp
 *)
fun findTemp(queryStr) =
    case List.find (fn (str, t) => str=queryStr) allUserRegs
     of SOME((s,t)) => t
      | _ => Temp.newTemp()

(* tempToString : register Temp.Table.table -> Temp.temp -> string
 *
 * Nice printing of temps.
 * Given a temp map, produces a function that will look up the name of a given
 * temp in the map and return that string, otherwise calls Temp.makeString
 *)
fun tempToString map =
    (fn t => case Temp.Table.look(map, t)
              of SOME(str) => str
               | NONE => (Temp.makeString(t)))

(* procEntryExit1: frame * Tree.stm -> Tree.stm
 *
 * This is the function that adds the prologue and epilogue to the code
 * of the function.
 * The tree move statements needed to process the incoming arguments are created
 * by genViewShiftMoves and they are added in front of the body along with
 * the tree statements needed to save the callee save registers.
 * The statements needed to restore the callee save registers are appended after
 * the body.
 *)
fun procEntryExit1(frame as {name, formals, maxOutgoing, locals}, body) =
    let
      (* genViewShiftMoves: access list * int * Tree.stm -> Tree.stm
       *
       * Generates the tree moves to move the arguments from where they were
       * passed in to their correct locations according to the list of accesses.
       * Arguments 1-4 are taken from registers $a0-$a3.
       * Arguments >4 are read from the appropriate place above the frame
       * pointer.
       *)
      fun genViewShiftMoves([], index, stm) = stm
        | genViewShiftMoves(access::accesses, index, stm) =
          let
            val fromLoc = case index
                            of 0 => T.TEMP(a0)
                             | 1 => T.TEMP(a1)
                             | 2 => T.TEMP(a2)
                             | 3 => T.TEMP(a3)
                             | _ => T.MEM(T.BINOP(T.PLUS, T.TEMP FP,
                                                  T.CONST(index * wordSize)))

            val newStm =
              case access
                of InFrame(k) =>
                      T.SEQ(T.MOVE(T.MEM(T.BINOP(T.PLUS, T.TEMP FP, T.CONST k)),
                                 fromLoc), stm)
                 | InReg(t) => T.SEQ(T.MOVE(T.TEMP t, fromLoc), stm)
          in
            genViewShiftMoves(accesses, index + 1, newStm)
          end

      val regsToSave = ra::(map (fn (s, t) => t) calleeSaves)
      (* Temp, reg pairs for everything we want to put in prolog *)
      val tempTemps = map (fn reg => (Temp.newTemp(), reg)) regsToSave
      (* Generate the tree statements to save the callee save registers in new
       * temps *)
      val prolog = (foldr (fn ((t, r), stm) => T.SEQ(T.MOVE(T.TEMP(t),
                                                            T.TEMP(r)), stm))
                          (T.EXP(T.CONST(0))) tempTemps)
      (* Generate the tree statements to restore the callee save registers *)
      val epilog = (foldr (fn ((t, r), stm) => T.SEQ(T.MOVE(T.TEMP(r),
                                                            T.TEMP(t)), stm))
                          (T.EXP(T.CONST(0))) tempTemps)
    in
      (* Surround the body in the prolog and epilog *)
      T.SEQ(prolog, genViewShiftMoves(formals, 0, T.SEQ(body, epilog)))
    end

(* procEntryExit2: frame * Assem.instr list -> Assem.instr list
 *
 * Adds a vacuous instruction to the end of a function body so that liveness
 * analysis can know that certain registers are live when a function returns.
 *)
fun procEntryExit2(frame, body) =
    let
      val liveTemps = map (fn (str, tmp) => tmp) (specialRegs@calleeSaves)
    in
      body @ [Assem.OPER{assem="",
                         src=liveTemps, dst=[],
                         jump=SOME[]}]
    end

(* procEntryExit3: frame * Assem.instr list -> {prolog:string,
 *                                              body: Assem.instr list,
 *                                              epilog:string}
 *
 * Adds the prolog and epilog to the functions.
 * Also writes the framesize to a constant at the beginning of the function
 *)
fun procEntryExit3({name, formals, maxOutgoing, locals}, body: Assem.instr list) =
    {prolog=Symbol.name(name)^":\n",
     body=body,
     epilog="jr $ra\n"}
end

structure Frame:>FRAME = MipsFrame
