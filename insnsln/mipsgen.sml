signature CODEGEN =
sig
  val codeGen : Frame.frame -> Tree.stm -> Assem.instr list
end

structure MipsGen :> CODEGEN =
struct

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

      val fs = Temp.namedLabel(Symbol.name(Frame.name(frame))^"_framesize")
      val FPtoSP = T.MEM(T.BINOP(T.PLUS, T.TEMP Frame.SP, T.MEM(T.NAME fs)))

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
       * TODO:
       * We can sort by root node and then by number of nodes munched due to
       * manner in which SML matches patterns. This is done for readability.
       *)
      fun munchStm(T.SEQ(a,b)) = (munchStm a; munchStm b)
        (* store into an address offset by a constant (right)
         * Nodes: 5
         *)
        | munchStm(T.MOVE(T.MEM(T.BINOP(T.PLUS, e1, T.CONST i)), e2)) =
          emit(Assem.MOVE{assem="sw 's0, " ^ Int.toString(i) ^ "('d0)\n",
                          src=(munchExp e2),
                          dst=(munchExp e1)})
        (* store into an address offset by a constant (left)
         * Nodes: 5
         *)
        | munchStm(T.MOVE(T.MEM(T.BINOP(T.PLUS, T.CONST i, e1)), e2)) =
          emit(Assem.MOVE{assem="sw 's0, " ^ Int.toString(i) ^ "('d0)\n",
                          src=(munchExp e2),
                          dst=(munchExp e1)})
        (* load into an address offset by a constant (right)
         * Nodes: 5
         *)
        | munchStm(T.MOVE(e1, T.MEM(T.BINOP(T.PLUS, e2, T.CONST i)))) =
          emit(Assem.MOVE{assem="lw 'd0, " ^ Int.toString(i)^"('s0)\n",
                          src=(munchExp e2),
                          dst=(munchExp e1)})
        (* load into an address offset by a constant (left)
         * Nodes: 5
         *)
        | munchStm(T.MOVE(e1, T.MEM(T.BINOP(T.PLUS, T.CONST i, e2)))) =
          emit(Assem.MOVE{assem="lw 'd0, " ^ Int.toString(i) ^ "('s0)\n",
                          src=(munchExp e2),
                          dst=(munchExp e1)})
        (* Branch if equal to zero (left)
         * Nodes: 3
         *)
        | munchStm(T.CJUMP(T.EQ, T.CONST 0, e, l1, l2)) =
          emit(Assem.OPER{assem="beqz 's0, " ^ Symbol.name(l1)^"\n",
                          src=[munchExp e],
                          dst=[],
                          jump=SOME([l1,l2])})
        (* Branch if equal to zero (right)
         * Nodes: 3
         *)
        | munchStm(T.CJUMP(T.EQ, e, T.CONST 0, l1, l2)) =
          emit(Assem.OPER{assem="beqz 's0, " ^ Symbol.name(l1)^"\n",
                          src=[munchExp e],
                          dst=[],
                          jump=SOME([l1,l2])})
        (* Branch if greater than or equal to zero (left)
         * Nodes: 3
         *)
        | munchStm(T.CJUMP(T.GE, T.CONST 0, e, l1, l2)) =
          emit(Assem.OPER{assem="bgez 's0, " ^ Symbol.name(l1)^"\n",
                          src=[munchExp e],
                          dst=[],
                          jump=SOME([l1,l2])})
        (* Branch if greater than or equal to zero (right)
         * Nodes: 3
         *)
        | munchStm(T.CJUMP(T.GE, e, T.CONST 0, l1, l2)) =
          emit(Assem.OPER{assem="bgez 's0, " ^ Symbol.name(l1)^"\n",
                          src=[munchExp e],
                          dst=[],
                          jump=SOME([l1,l2])})
        (* Branch if greater than zero (left)
         * Nodes: 3
         *)
        | munchStm(T.CJUMP(T.GT, T.CONST 0, e, l1, l2)) =
          emit(Assem.OPER{assem="bgtz 's0, " ^ Symbol.name(l1)^"\n",
                          src=[munchExp e],
                          dst=[],
                          jump=SOME([l1,l2])})
        (* Branch if greater than zero (right)
         * Nodes: 3
         *)
        | munchStm(T.CJUMP(T.GT, e, T.CONST 0, l1, l2)) =
          emit(Assem.OPER{assem="bgtz 's0, " ^ Symbol.name(l1)^"\n",
                          src=[munchExp e],
                          dst=[],
                          jump=SOME([l1,l2])})
        (* Branch if less than or equal to zero (left)
         * Nodes: 3
         *)
        | munchStm(T.CJUMP(T.LE, T.CONST 0, e, l1, l2)) =
          emit(Assem.OPER{assem="blez 's0, " ^ Symbol.name(l1)^"\n",
                          src=[munchExp e],
                          dst=[],
                          jump=SOME([l1,l2])})
        (* Branch if less than or equal to zero (right)
         * Nodes: 3
         *)
        | munchStm(T.CJUMP(T.LE, e, T.CONST 0, l1, l2)) =
          emit(Assem.OPER{assem="blez 's0, " ^ Symbol.name(l1)^"\n",
                          src=[munchExp e],
                          dst=[],
                          jump=SOME([l1,l2])})
        (* Branch if less than zero (left)
         * Nodes: 3
         *)
        | munchStm(T.CJUMP(T.LT, T.CONST 0, e, l1, l2)) =
          emit(Assem.OPER{assem="bltz 's0, " ^ Symbol.name(l1)^"\n",
                          src=[munchExp e],
                          dst=[],
                          jump=SOME([l1,l2])})
        (* Branch if less than zero (right)
         * Nodes: 3
         *)
        | munchStm(T.CJUMP(T.LT, e, T.CONST 0, l1, l2)) =
          emit(Assem.OPER{assem="bltz 's0, " ^ Symbol.name(l1)^"\n",
                          src=[munchExp e],
                          dst=[],
                          jump=SOME([l1,l2])})
        (* Branch if not equal to zero (left)
         * Nodes: 3
         *)
        | munchStm(T.CJUMP(T.NE, T.CONST 0, e, l1, l2)) =
          emit(Assem.OPER{assem="bnez 's0, " ^ Symbol.name(l1)^"\n",
                          src=[munchExp e],
                          dst=[],
                          jump=SOME([l1,l2])})
        (* Branch if not equal to zero (right)
         * Nodes: 3
         *)
        | munchStm(T.CJUMP(T.NE, e, T.CONST 0, l1, l2)) =
          emit(Assem.OPER{assem="bnez 's0, " ^ Symbol.name(l1)^"\n",
                          src=[munchExp e],
                          dst=[],
                          jump=SOME([l1,l2])})
        (* Function calls.
         * Nodes: 3
         *)
        | munchStm(T.EXP(T.CALL(T.NAME(funName), args))) =
          emit(Assem.OPER{assem="jal "^Symbol.name(funName)^"\n",
                          src=munchArgs(0, args),
                          dst=[],
                          jump=NONE})


        (* Branch if equal
         * Nodes: 2
         *)
        | munchStm(T.CJUMP(T.EQ, e1, e2, l1, l2)) =
          emit(Assem.OPER{assem="beq 's0, 's1, " ^ Symbol.name(l1)^"\n",
                          src=[munchExp e1, munchExp e2],
                          dst=[],
                          jump=SOME([l1,l2])})
        (* Branch if greater than or equal
         * Nodes: 2
         *)
        | munchStm(T.CJUMP(T.GE, e1, e2, l1, l2)) =
          emit(Assem.OPER{assem="bge 's0, 's1, " ^ Symbol.name(l1)^"\n",
                          src=[munchExp e1, munchExp e2],
                          dst=[],
                          jump=SOME([l1,l2])})
        (* Branch if greater than
         * Nodes: 2
         *)
        | munchStm(T.CJUMP(T.GT, e1, e2, l1, l2)) =
          emit(Assem.OPER{assem="bgt 's0, 's1, " ^ Symbol.name(l1)^"\n",
                          src=[munchExp e1, munchExp e2],
                          dst=[],
                          jump=SOME([l1,l2])})
        (* Branch if less than or equal to
         * Nodes: 2
         *)
        | munchStm(T.CJUMP(T.LE, e1, e2, l1, l2)) =
          emit(Assem.OPER{assem="ble 's0, 's1, " ^ Symbol.name(l1)^"\n",
                          src=[munchExp e1, munchExp e2],
                          dst=[],
                          jump=SOME([l1,l2])})
        (* Branch if less than
         * Nodes: 2
         *)
        | munchStm(T.CJUMP(T.LT, e1, e2, l1, l2)) =
          emit(Assem.OPER{assem="blt 's0, 's1, " ^ Symbol.name(l1)^"\n",
                          src=[munchExp e1, munchExp e2],
                          dst=[],
                          jump=SOME([l1,l2])})
        (* Branch if not equal
         * Nodes: 2
         *)
        | munchStm(T.CJUMP(T.NE, e1, e2, l1, l2)) =
          emit(Assem.OPER{assem="bne 's0, 's1, " ^ Symbol.name(l1)^"\n",
                          src=[munchExp e1, munchExp e2],
                          dst=[],
                          jump=SOME([l1,l2])})

        (* Load constant into the destination e1
         * Nodes: 2
         *)
        | munchStm(T.MOVE(e1, T.CONST i)) =
          emit(Assem.OPER({assem="li 'd0, " ^ Int.toString(i) ^ "\n",
                           src=[],
                           dst=[munchExp e1],
                           jump=NONE}))

        (* store into an address without offset
         * Nodes: 2
         *)
        | munchStm(T.MOVE(T.MEM(e1), e2)) =
          emit(Assem.MOVE{assem="sw 's0, 0('d0)"^"\n",
                          src=(munchExp e2),
                          dst=(munchExp e1)})

        (* load into an address without offset
         * Nodes: 2
         *)
        | munchStm(T.MOVE(e1, T.MEM(e2))) =
          emit(Assem.MOVE{assem="lw 'd0, 0('s0)"^"\n",
                          src=(munchExp e2),
                          dst=(munchExp e1)})
        (* Jump to the specified label
         * Note that the first argument in T.JUMP will have to be a name after
         * the canonicalizer because there are no longer any ESEQ's and the
         * the first expression must be one of the labels in the list.
         * 
         * Nodes: 2
         *)
        | munchStm(T.JUMP(T.NAME lab, labs)) =
          emit(Assem.OPER{assem="j " ^ Symbol.name(lab) ^ "\n",
                          src=[], dst=[], jump=SOME(labs)})


        (* Move something into a temp
         * Nodes: 2
         *)
        | munchStm(T.MOVE(T.TEMP(t1), T.TEMP(t2))) =
          if t1=Frame.FP
          then munchStm(T.MOVE(FPtoSP, T.TEMP(t2)))
          else if t2=Frame.FP
          then munchStm(T.MOVE(T.TEMP(t1), FPtoSP))
          else emit(Assem.OPER{assem="move 'd0, 's0\n",
                               src=[t2],
                               dst=[t1],
                               jump=NONE})
        (* Move something into a temp
         * Nodes: 2
         *)
        | munchStm(T.MOVE(T.TEMP(t), e)) =
          if t=Frame.FP
          then munchStm(T.MOVE(FPtoSP, e))
          else  emit(Assem.OPER{assem="move 'd0, 's0\n",
                                src=[munchExp e],
                                dst=[t],
                                jump=NONE})

        (* These is the nop
         * Nodes: 2
         *)
        | munchStm(T.EXP(T.CONST 0)) = ()
        | munchStm(T.EXP(_)) =
          let exception UncaughtT_EXP in raise UncaughtT_EXP end
        (* Create assembly label
         * Nodes: 1
         *)
        | munchStm(T.LABEL lab) =
          emit(Assem.LABEL{assem=Symbol.name(lab) ^ ":\n", lab=lab})

        (* Catch an error *)
        | munchStm(e) =
          let
            exception MissedStmCase
          in
            (Printtree.printtree(TextIO.stdOut, e);
             raise MissedStmCase)
          end

            
      and munchExp(T.TEMP(t)) =
          if t=Frame.FP
          then munchExp(FPtoSP)
          else t
        | munchExp(T.MEM(T.BINOP(T.PLUS, e1, T.CONST i))) =
          (result(fn r => emit(Assem.OPER{assem="lw 'd0, "^
                                                Int.toString(i)^"('s0)\n",
                                          src=[munchExp e1], dst=[r],
                                          jump=NONE})))
        | munchExp(T.MEM(T.BINOP(T.PLUS, T.CONST i, e1))) = 
          (result(fn r => emit(Assem.OPER{assem="lw 'd0, "^
                                                Int.toString(i)^"('s0)\n",
                                          src=[munchExp e1], dst=[r],
                                          jump=NONE})))
        (* | munchExp(T.MEM(T.CONST i)) = Falling through *)
        | munchExp(T.MEM(e)) =
          (result(fn r => emit(Assem.OPER{assem="lw 'd0, 0('s0)\n",
                                          src=[munchExp e], dst=[r],
                                          jump=NONE})))
        | munchExp(T.BINOP(T.PLUS, e, T.CONST i)) =
          (result(fn r => emit(Assem.OPER{assem="addi 'd0, 's0, "^
                                                Int.toString(i)^"\n",
                                          src=[munchExp e], dst=[r],
                                          jump=NONE})))
        | munchExp(T.BINOP(T.PLUS, T.CONST i, e)) =
          (result(fn r => emit(Assem.OPER{assem="addi 'd0, 's0, "^
                                                Int.toString(i)^"\n",
                                          src=[munchExp e], dst=[r],
                                          jump=NONE})))
        | munchExp(T.BINOP(T.PLUS, e1, e2)) =
          (result(fn r => emit(Assem.OPER{assem="add 'd0, 's0, 's1\n",
                                          src=[munchExp e1, munchExp e2],
                                          dst=[r],
                                          jump=NONE})))
        | munchExp(T.BINOP(T.MINUS, e1, e2)) =
          (result(fn r => emit(Assem.OPER{assem="sub 'd0, 's0, 's1\n",
                                          src=[munchExp e1, munchExp e2],
                                          dst=[r],
                                          jump=NONE})))
        | munchExp(T.BINOP(T.MUL, e1, e2)) =
          (result(fn r => emit(Assem.OPER{assem="mul 'd0, 's0, 's1\n",
                                          src=[munchExp e1, munchExp e2],
                                          dst=[r],
                                          jump=NONE})))
        | munchExp(T.BINOP(T.DIV, e1, e2)) =
          (result(fn r => emit(Assem.OPER{assem="div 'd0, 's0, 's1\n",
                                          src=[munchExp e1, munchExp e2],
                                          dst=[r],
                                          jump=NONE})))
        | munchExp(T.NAME(l)) =
          (result(fn r => emit(Assem.OPER{assem="la 'd0, "^Symbol.name(l)^"\n",
                                          src=[], dst=[r],
                                          jump=NONE})))
        | munchExp(T.CONST(i)) = 
          (result(fn r => emit(Assem.OPER{assem="li 'd0, "^Int.toString(i)^"\n",
                                          src=[], dst=[r],
                                          jump=NONE})))
        | munchExp(T.CALL(T.NAME(funName), args)) = 
          let
            val fsAddr = Temp.newTemp()
            and fsVal = Temp.newTemp()
            and newfsVal = Temp.newTemp()
            val argsSpace = Int.toString(Frame.wordSize*List.length(args))
          in
            (                     (* Pre-call things *)
              emit(Assem.OPER{assem="addi $sp, $sp, -"^argsSpace^"\n",
                              src=[Frame.SP],
                              dst=[Frame.SP],
                              jump=NONE});
              emit(Assem.OPER{assem="la 'd0, "^Symbol.name(fs)^"\n",
                              src=[],
                              dst=[fsAddr],
                              jump=NONE});
              emit(Assem.OPER{assem="lw 'd0, ('s0)\n",
                              src=[fsAddr],
                              dst=[fsVal],
                              jump=NONE});
              emit(Assem.OPER{assem="addi 'd0, 's0, "^argsSpace^"\n",
                              src=[fsVal],
                              dst=[newfsVal],
                              jump=NONE});
              emit(Assem.OPER{assem="sw 's0, ('d0)\n",
                              src=[newfsVal],
                              dst=[fsAddr],
                              jump=NONE});
              (* Actual funcall *)
              emit(Assem.OPER{assem="jal "^Symbol.name(funName)^"\n",
                              src=munchArgs(0, args),
                              (* TODO: Add caller-saves, and $ra.
                               * Should we use a function to access reglists?
                               * Or can they be visible to other modules?
                               *)
                              dst=[Frame.RV],
                              jump=NONE});
              (* Undo the pre-call things *)
              emit(Assem.OPER{assem="la 'd0, "^Symbol.name(fs)^"\n",
                              src=[],
                              dst=[fsAddr],
                              jump=NONE});
              emit(Assem.OPER{assem="lw 'd0, ('s0)\n",
                              src=[fsAddr],
                              dst=[fsVal],
                              jump=NONE});
              emit(Assem.OPER{assem="addi 'd0, 's0, -"^argsSpace^"\n",
                              src=[fsVal],
                              dst=[newfsVal],
                              jump=NONE});
              emit(Assem.OPER{assem="sw 's0, ('d0)\n",
                              src=[newfsVal],
                              dst=[fsAddr],
                              jump=NONE});
              emit(Assem.OPER{assem="addi $sp, $sp, "^argsSpace^"\n",
                              src=[Frame.SP],
                              dst=[Frame.SP],
                              jump=NONE});
              (* Return the return value *)
              Frame.RV)
          end
            
        | munchExp(e) =
          let exception Uncaught_munchExp in raise Uncaught_munchExp end


      (* munchArgs: int * Tree.exp list -> Temp.temp list
       * 
       * Caller's POV view shift will happen here. 
       * i=0 => SL. Put in SL register.
       * i in {1, ..., 4} => arguments for regs (put there)
       * i > 4 => move onto the frame, at appropriate offset
       *)
      and munchArgs(_, []) = []
        | munchArgs(i, arg::args) =
          ((* print(Int.toString(i)^"::"); *)
           (* Printtree.printtree(TextIO.stdOut, T.EXP(arg));            *)
           (* TODO: Find a better way for findArgTemp *)
           (* if i=0               (* SL *) *)
           (* then *)
           (*   (emit(Assem.OPER{assem="move $v1, 's0\n", *)
           (*                    src=[munchExp arg], *)
           (*                    dst=[Frame.findArgTemp("$v1")], *)
           (*                    jump=NONE}); *)
           (*    Frame.findArgTemp("$v1")::munchArgs(i+1, args)) *)
           if i<4
           then                 (* Pass in $a0--$a3 *)
             (emit(Assem.OPER{assem="move $a"^Int.toString(i)^", 's0\n",
                              src=[munchExp arg],
                              dst=[Frame.findArgTemp("$a"^Int.toString(i))],
                              jump=NONE});
              Frame.findArgTemp("$a"^Int.toString(i))::munchArgs(i+1, args))
           else                 (* Put on the stack frame *)
             (emit(Assem.OPER{assem="sw 's0, "^
                                    Int.toString(i*Frame.wordSize)^"('d0)\n",
                              src=[munchExp arg],
                              dst=[],
                              jump=NONE});
              munchArgs(i+1, args)))

    in
      (* emit(Assem.LABEL{assem=Frame.name(frame), *)
      (*                  lab=Symbol.symbolize(Frame.name(frame))}); *)
      munchStm stm;
      rev(!ilist)
    end
end
