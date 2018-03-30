signature CODEGEN =
sig
  val codeGen : Frame.frame -> Tree.stm -> Assem.instr list
end

structure MipsGen :> CODEGEN =
struct

structure T = Tree
(* codegen : Frame.frame -> Tree.stm -> Assem.instr list
 *
 * Given a frame, returns a function that produces the
 * machine-specific assembly instructions for the given statement.
 * Implements the maximal munch algorithm to tile the nodes in the
 * Tree.stm to appropriate assembly instructions in a greedy manner
 *)
fun codeGen(frame) (stm: Tree.stm) : Assem.instr list =
    let
      (* the list of instructions that we have generated so far *)
      val ilist = ref (nil: Assem.instr list)

      (* We assume that the framesize will be written to a label
       * <FUNLABEL>_framesize by the register allocator.
       * We are assuming that this framesize will only encompass the
       * offset from the frame pointer to just before the outgoing
       * argument space.
       *)
      val fs = Symbol.symbolize(Symbol.name(Frame.name(frame))^"_framesize")

      (* Tree code for adding the framesize to the stack pointer: SP + fs *)
      val FPtoSP = T.BINOP(T.PLUS, T.TEMP Frame.SP, T.MEM(T.NAME fs))

      (* emit : Assem.instr -> unit
       *
       * Adds the given instruction to the instruction list
       *)
      fun emit x = ilist := x :: !ilist

      (* result : (Temp.temp -> unit) -> Temp.temp
       *
       * Generates a new temp and calls the given generator function
       * with it, then returns the new temp.
       * Used by munchExp to munch something, put it in a reg & return the reg
       *)
      fun result(gen) =
          let val t = Temp.newTemp()
          in gen t; t
          end

      (* munchStm : Tree.stm -> unit
       *
       * Greedily chooses the biggest tile corresponding to an assembly
       * instruction compatible with the root node of the given statement
       *
       * We sort first by root node and then by number of nodes munched.
       * This is done for readability while still attaining the maximal
       * munch, since each type of root node will be maximally munched.
       *)
      fun munchStm(T.SEQ(a,b)) = (munchStm a; munchStm b) (* simple seq case *)

        (******************T.MOVE***************************)
        (* store into an address offset by a constant (right)
         * Nodes: 5
         *)
        | munchStm(T.MOVE(T.MEM(T.BINOP(T.PLUS, e1, T.CONST i)), e2)) =
          emit(Assem.OPER{assem="sw 's0, "^Int.toString(i)^"('d0)\n",
                          src=[munchExp e2],
                          dst=[munchExp e1],
                          jump=NONE})

        (* store into an address offset by a constant (left)
         * Nodes: 5
         *)
        | munchStm(T.MOVE(T.MEM(T.BINOP(T.PLUS, T.CONST i, e1)), e2)) =
          emit(Assem.OPER{assem="sw 's0, "^Int.toString(i)^"('d0)\n",
                          src=[munchExp e2],
                          dst=[munchExp e1],
                          jump=NONE})

        (* load from an address offset by a constant (right)
         * Nodes: 5
         *)
        | munchStm(T.MOVE(e1, T.MEM(T.BINOP(T.PLUS, e2, T.CONST i)))) =
          emit(Assem.OPER{assem="lw 'd0, "^Int.toString(i)^"('s0)\n",
                          src=[munchExp e2],
                          dst=[munchExp e1],
                          jump=NONE})

        (* load from an address offset by a constant (left)
         * Nodes: 5
         *)
        | munchStm(T.MOVE(e1, T.MEM(T.BINOP(T.PLUS, T.CONST i, e2)))) =
          emit(Assem.OPER{assem="lw 'd0, "^Int.toString(i)^"('s0)\n",
                          src=[munchExp e2],
                          dst=[munchExp e1],
                          jump=NONE})

        (* store into an address without offset
         * Nodes: 2
         *)
        | munchStm(T.MOVE(T.MEM(e1), e2)) =
          emit(Assem.MOVE{assem="sw 's0, 0('d0)\n",
                          src=(munchExp e2),
                          dst=(munchExp e1)})

        (* Load constant into the destination e1
         * Nodes: 2
         *)
        | munchStm(T.MOVE(e1, T.CONST i)) =
          emit(Assem.OPER({assem="li 'd0, "^Int.toString(i)^"\n",
                           src=[],
                           dst=[munchExp e1],
                           jump=NONE}))

        (* Move something into a temp
         * Nodes: 2
         *)
        | munchStm(T.MOVE(T.TEMP(t1), T.TEMP(t2))) =
          if t1=Frame.FP
          then munchStm(T.MOVE(T.TEMP(munchExp FPtoSP), T.TEMP(t2)))
          else if t2=Frame.FP
          then munchStm(T.MOVE(T.TEMP(t1), FPtoSP))
          else emit(Assem.MOVE{assem="move 'd0, 's0\n",
                               src=t2,
                               dst=t1})

        (* Move something into a temp
         * Nodes: 2
         *)
        | munchStm(T.MOVE(T.TEMP(t), e)) =
          if t=Frame.FP
          then munchStm(T.MOVE(T.TEMP(munchExp FPtoSP), e))
          else  emit(Assem.MOVE{assem="move 'd0, 's0\n",
                                src=(munchExp e),
                                dst=t})

        (* load from an address without offset
         * Nodes: 2
         *)
        | munchStm(T.MOVE(e1, T.MEM(e2))) =
          emit(Assem.MOVE{assem="lw 'd0, 0('s0)\n",
                          src=(munchExp e2),
                          dst=(munchExp e1)})


        (******************T.CJUMP***************************)
        (* Branch if equal to zero (left)
         * Nodes: 3
         *)
        | munchStm(T.CJUMP(T.EQ, T.CONST 0, e, l1, l2)) =
          emit(Assem.OPER{assem="beqz 's0, "^Symbol.name(l1)^"\n",
                          src=[munchExp e],
                          dst=[],
                          jump=SOME([l1,l2])})

        (* Branch if equal to zero (right)
         * Nodes: 3
         *)
        | munchStm(T.CJUMP(T.EQ, e, T.CONST 0, l1, l2)) =
          emit(Assem.OPER{assem="beqz 's0, "^Symbol.name(l1)^"\n",
                          src=[munchExp e],
                          dst=[],
                          jump=SOME([l1,l2])})

        (* Branch if greater than or equal to zero (left)
         * Nodes: 3
         *)
        | munchStm(T.CJUMP(T.GE, T.CONST 0, e, l1, l2)) =
          emit(Assem.OPER{assem="bgez 's0, "^Symbol.name(l1)^"\n",
                          src=[munchExp e],
                          dst=[],
                          jump=SOME([l1,l2])})

        (* Branch if greater than or equal to zero (right)
         * Nodes: 3
         *)
        | munchStm(T.CJUMP(T.GE, e, T.CONST 0, l1, l2)) =
          emit(Assem.OPER{assem="bgez 's0, "^Symbol.name(l1)^"\n",
                          src=[munchExp e],
                          dst=[],
                          jump=SOME([l1,l2])})

        (* Branch if greater than zero (left)
         * Nodes: 3
         *)
        | munchStm(T.CJUMP(T.GT, T.CONST 0, e, l1, l2)) =
          emit(Assem.OPER{assem="bgtz 's0, "^Symbol.name(l1)^"\n",
                          src=[munchExp e],
                          dst=[],
                          jump=SOME([l1,l2])})

        (* Branch if greater than zero (right)
         * Nodes: 3
         *)
        | munchStm(T.CJUMP(T.GT, e, T.CONST 0, l1, l2)) =
          emit(Assem.OPER{assem="bgtz 's0, "^Symbol.name(l1)^"\n",
                          src=[munchExp e],
                          dst=[],
                          jump=SOME([l1,l2])})

        (* Branch if less than or equal to zero (left)
         * Nodes: 3
         *)
        | munchStm(T.CJUMP(T.LE, T.CONST 0, e, l1, l2)) =
          emit(Assem.OPER{assem="blez 's0, "^Symbol.name(l1)^"\n",
                          src=[munchExp e],
                          dst=[],
                          jump=SOME([l1,l2])})

        (* Branch if less than or equal to zero (right)
         * Nodes: 3
         *)
        | munchStm(T.CJUMP(T.LE, e, T.CONST 0, l1, l2)) =
          emit(Assem.OPER{assem="blez 's0, "^Symbol.name(l1)^"\n",
                          src=[munchExp e],
                          dst=[],
                          jump=SOME([l1,l2])})

        (* Branch if less than zero (left)
         * Nodes: 3
         *)
        | munchStm(T.CJUMP(T.LT, T.CONST 0, e, l1, l2)) =
          emit(Assem.OPER{assem="bltz 's0, "^Symbol.name(l1)^"\n",
                          src=[munchExp e],
                          dst=[],
                          jump=SOME([l1,l2])})

        (* Branch if less than zero (right)
         * Nodes: 3
         *)
        | munchStm(T.CJUMP(T.LT, e, T.CONST 0, l1, l2)) =
          emit(Assem.OPER{assem="bltz 's0, "^Symbol.name(l1)^"\n",
                          src=[munchExp e],
                          dst=[],
                          jump=SOME([l1,l2])})

        (* Branch if not equal to zero (left)
         * Nodes: 3
         *)
        | munchStm(T.CJUMP(T.NE, T.CONST 0, e, l1, l2)) =
          emit(Assem.OPER{assem="bnez 's0, "^Symbol.name(l1)^"\n",
                          src=[munchExp e],
                          dst=[],
                          jump=SOME([l1,l2])})

        (* Branch if not equal to zero (right)
         * Nodes: 3
         *)
        | munchStm(T.CJUMP(T.NE, e, T.CONST 0, l1, l2)) =
          emit(Assem.OPER{assem="bnez 's0, "^Symbol.name(l1)^"\n",
                          src=[munchExp e],
                          dst=[],
                          jump=SOME([l1,l2])})

        (* Branch if equal
         * Nodes: 2
         *)
        | munchStm(T.CJUMP(T.EQ, e1, e2, l1, l2)) =
          emit(Assem.OPER{assem="beq 's0, 's1, "^Symbol.name(l1)^"\n",
                          src=[munchExp e1, munchExp e2],
                          dst=[],
                          jump=SOME([l1,l2])})

        (* Branch if greater than or equal
         * Nodes: 2
         *)
        | munchStm(T.CJUMP(T.GE, e1, e2, l1, l2)) =
          emit(Assem.OPER{assem="bge 's0, 's1, "^Symbol.name(l1)^"\n",
                          src=[munchExp e1, munchExp e2],
                          dst=[],
                          jump=SOME([l1,l2])})

        (* Branch if greater than
         * Nodes: 2
         *)
        | munchStm(T.CJUMP(T.GT, e1, e2, l1, l2)) =
          emit(Assem.OPER{assem="bgt 's0, 's1, "^Symbol.name(l1)^"\n",
                          src=[munchExp e1, munchExp e2],
                          dst=[],
                          jump=SOME([l1,l2])})

        (* Branch if less than or equal to
         * Nodes: 2
         *)
        | munchStm(T.CJUMP(T.LE, e1, e2, l1, l2)) =
          emit(Assem.OPER{assem="ble 's0, 's1, "^Symbol.name(l1)^"\n",
                          src=[munchExp e1, munchExp e2],
                          dst=[],
                          jump=SOME([l1,l2])})

        (* Branch if less than
         * Nodes: 2
         *)
        | munchStm(T.CJUMP(T.LT, e1, e2, l1, l2)) =
          emit(Assem.OPER{assem="blt 's0, 's1, "^ Symbol.name(l1)^"\n",
                          src=[munchExp e1, munchExp e2],
                          dst=[],
                          jump=SOME([l1,l2])})

        (* Branch if not equal
         * Nodes: 2
         *)
        | munchStm(T.CJUMP(T.NE, e1, e2, l1, l2)) =
          emit(Assem.OPER{assem="bne 's0, 's1, "^Symbol.name(l1)^"\n",
                          src=[munchExp e1, munchExp e2],
                          dst=[],
                          jump=SOME([l1,l2])})


        (******************T.JUMP***************************)
        (* Jump to the specified label
         * Note that the first argument in T.JUMP will have to be a name after
         * the canonicalizer because there are no longer any ESEQ's and the
         * the first expression must be one of the labels in the list.
         *
         * Nodes: 2
         *)
        | munchStm(T.JUMP(T.NAME lab, labs)) =
          emit(Assem.OPER{assem="j "^Symbol.name(lab)^"\n",
                          src=[], dst=[], jump=SOME(labs)})


        (******************T.EXP***************************)
        (* Function calls.
         * Nodes: 3
         *)
        | munchStm(T.EXP(T.CALL(T.NAME(funName), args))) =
          (* Delegate to munchExp to get the necessary precall/postcall stuff
           * by eliminating the surrounding T.EXP,
           * and then drop the register that it returns (RV) on the floor
           * because this function was called as a statement and should not
           * have a return value.
           *)
          (munchExp(T.CALL(T.NAME(funName), args)); ())

        (* These are the nop's
         * Nodes: 2
         *)
        | munchStm(T.EXP(T.CONST 0)) = ()

        (* There should not be any other legal cases of EXP's in the tree
         * after the canonicalizer. If there are then this is a compiler bug
         *)
        | munchStm(T.EXP(_)) =
          let exception UncaughtT_EXP in raise UncaughtT_EXP end

        (******************T.LABEL***************************)
        (* Create assembly label
         * Nodes: 1
         *)
        | munchStm(T.LABEL lab) =
          emit(Assem.LABEL{assem=Symbol.name(lab)^":\n", lab=lab})


        (* The cases above should cover all legal cases. If we get
         * here without matching, then that is a compiler bug
         *)
        | munchStm(e) =
          let
            exception MissedStmCase
          in
            (Printtree.printtree(TextIO.stdOut, e);
             raise MissedStmCase)
          end

      (* munchExp : Tree.exp -> Temp.temp
       *
       * Greedily chooses the biggest tile corresponding to an assembly
       * instruction compatible with the root node of the given expression.
       * It returns the temp at which the result of the expression will
       * be stored, creating a new temp to return if necessary.
       *
       * We sort first by root node and then by number of nodes munched.
       * This is done for readability while still attaining the maximal
       * munch, since each type of root node will be maximally munched.
       *)
      and munchExp(T.TEMP(t)) =
          (* T.TEMP:
           * Nodes: 1
           *
           * If the temp is the frame pointer, then we need to
           * convert the reference to the frame pointer to SP + fs.
           *
           * Note that this is the biggest tile we can match when converting
           * the FP references, because an separate assembly instruction is
           * needed to add the framesize to the SP in all cases
           *)
          if t=Frame.FP
          then munchExp(FPtoSP)
          else t

        (***************************T.MEM***************************)
        (* Load from an address offset by a constant (right)
         * Nodes: 4
         *)
        | munchExp(T.MEM(T.BINOP(T.PLUS, e1, T.CONST i))) =
          (result(fn r => emit(Assem.OPER{assem="lw 'd0, "^Int.toString(i)^"('s0)\n",
                                          src=[munchExp e1], dst=[r],
                                          jump=NONE})))

        (* Load from an address offset by a constant (left)
         * Nodes: 4
         *)
        | munchExp(T.MEM(T.BINOP(T.PLUS, T.CONST i, e1))) =
          (result(fn r => emit(Assem.OPER{assem="lw 'd0, "^Int.toString(i)^"('s0)\n",
                                          src=[munchExp e1], dst=[r],
                                          jump=NONE})))

        (* Load from an address with no offset
         * Nodes: 1
         *)
        | munchExp(T.MEM(e)) =
          (result(fn r => emit(Assem.MOVE{assem="lw 'd0, 0('s0)\n",
                                          src=(munchExp e), dst=r})))


        (***************************T.BINOP***************************)
        (* Add a constant to the result of the expression on the left
         * Nodes: 3
         *)
        | munchExp(T.BINOP(T.PLUS, e, T.CONST i)) =
          (result(fn r => emit(Assem.OPER{assem="addi 'd0, 's0, "^Int.toString(i)^"\n",
                                          src=[munchExp e], dst=[r],
                                          jump=NONE})))

        (* Add a constant to the result of the expression on the right
         * Nodes: 3
         *)
        | munchExp(T.BINOP(T.PLUS, T.CONST i, e)) =
          (result(fn r => emit(Assem.OPER{assem="addi 'd0, 's0, "^Int.toString(i)^"\n",
                                          src=[munchExp e], dst=[r],
                                          jump=NONE})))

        (* Add the results of the two given expressions
         * Nodes: 2
         *)
        | munchExp(T.BINOP(T.PLUS, e1, e2)) =
          (result(fn r => emit(Assem.OPER{assem="add 'd0, 's0, 's1\n",
                                          src=[munchExp e1, munchExp e2],
                                          dst=[r],
                                          jump=NONE})))

        (* Subtract the results of the two given expressions
         * Nodes: 2
         *)
        | munchExp(T.BINOP(T.MINUS, e1, e2)) =
          (result(fn r => emit(Assem.OPER{assem="sub 'd0, 's0, 's1\n",
                                          src=[munchExp e1, munchExp e2],
                                          dst=[r],
                                          jump=NONE})))

        (* Multiply the results of the two given expressions
         * Nodes: 2
         *)
        | munchExp(T.BINOP(T.MUL, e1, e2)) =
          (result(fn r => emit(Assem.OPER{assem="mul 'd0, 's0, 's1\n",
                                          src=[munchExp e1, munchExp e2],
                                          dst=[r],
                                          jump=NONE})))

        (* Divide the results of the two given expressions
         * Nodes: 2
         *)
        | munchExp(T.BINOP(T.DIV, e1, e2)) =
          (result(fn r => emit(Assem.OPER{assem="div 'd0, 's0, 's1\n",
                                          src=[munchExp e1, munchExp e2],
                                          dst=[r],
                                          jump=NONE})))


        (************************T.NAME*****************************)
        (* Load the address of the given label
         * Nodes: 1
         *)
        | munchExp(T.NAME(l)) =
          (result(fn r => emit(Assem.OPER{assem="la 'd0, "^Symbol.name(l)^"\n",
                                          src=[], dst=[r],
                                          jump=NONE})))


        (************************T.CONST*****************************)
        (* Load the given constant into a temp
         * Nodes: 1
         *)
        | munchExp(T.CONST(i)) =
          (result(fn r => emit(Assem.OPER{assem="li 'd0, "^Int.toString(i)^"\n",
                                          src=[], dst=[r],
                                          jump=NONE})))


        (************************T.CALL*****************************)
        (* Instructions associated with making a function call.
         * Nodes: 2
         *
         * Precall: Before we jump to the callee, the caller needs
         * to set up the outgoing arguments at the bottom of its frame.
         * To do so, we might need to write some outgoing arguments
         * to the frame (if there are more than 4)
         * So for this, we need to move the stack pointer down to make space
         * for all outgoing arguments now, before writing them.
         * But since we use the SP + fs for calculating the offset to the
         * frame pointer, we need to also adjust the frame size to
         * maintain correctness there.
         * This involves loading the address of the frame size label,
         * loading the value of the frame size,
         * adding the correct value to the frame size,
         * and finally storing the updated value back into the address at the
         * frame size label.
         *
         * Call: simple jal to the callee's function label
         *
         * Postcall: We need to undo all of the precall setup to return
         * the caller's state to it's precall size. This way, if the caller
         * makes another function call it will be cleanly set up to do it
         * in the same way as the first one.
         *)
        | munchExp(T.CALL(T.NAME(funName), args)) =
          let
            (* Gets all of the temps for the registers that are expected
             * to be trashed by a function call:
             * caller-saves, return address, return value
             *)
            val trashedByCall = map Frame.findTemp ["$t0", "$t1", "$t2", "$t3",
                                                    "$t4", "$t5", "$t6", "$t7",
                                                    "$t8", "$t9", "$ra"]
          in
              (* Pre-call things *)
              (* Extend the stack pointer to accommate the space for the
               * outgoing arguments
               *)
              (* Actual function call *)
              (emit(Assem.OPER{assem="jal "^Symbol.name(funName)^"\n",
                              src=munchArgs(0, args),
                              dst=Frame.RV::trashedByCall,
                              jump=NONE});
              (* Undo the pre-call things *)
              (* Return the return value *)
              Frame.RV)
          end

        (* The cases above should cover all legal cases. If we get
         * here without matching, then that is a compiler bug
         *)
        | munchExp(e) =
          let exception Uncaught_munchExp in raise Uncaught_munchExp end


      (* munchArgs: int * Tree.exp list -> Temp.temp list
       *
       * Caller's POV view shift will happen here.
       * i in {1, ..., 4} => arguments for regs put in $a0-$a3
       * i > 4 => store onto the frame, at appropriate offset
       *)
      and munchArgs(_, []) = []
        | munchArgs(i, arg::args) =
           (if i<4
           then (* Pass in $a0--$a3 *)
             let
               val argTemp = Frame.findTemp("$a"^Int.toString(i))
             in
               (emit(Assem.MOVE{assem="move $a"^Int.toString(i)^", 's0\n",
                                src=(munchExp arg),
                                dst=argTemp});
                argTemp::munchArgs(i+1, args))
              end
           else (* Put on the stack frame *)
             (emit(Assem.OPER{assem="sw 's0,"^Int.toString(i*Frame.wordSize)^"($sp)\n",
                              src=[munchExp arg],
                              dst=[],
                              jump=NONE});
              munchArgs(i+1, args)))

    in
      munchStm stm;
      rev(!ilist)
    end
end
