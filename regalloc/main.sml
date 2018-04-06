structure Main = struct

(* modifiedMakeString: Temp.temp -> string
 *
 * Look up the given temp in the tempMap.
 * If found, return that string, otherwise default to Temp.makeString.
 *)
fun modifiedMakeString(t) =
    case Temp.Table.look(Frame.tempMap, t)
     of SOME(str) => str
      | NONE => Temp.makeString(t)

(* createFlowGraph : Assem.instr list * (Assem.instr -> string)
 *                                      -> Flow.flowgraph * Graph.node list
 *
 * Creates a control flow graph for the given list of assembly instructions,
 * after applying the given format instruction to the assembly string in each
 * instruction.
 *)
fun createFlowGraph(body, format) =
      let
        (* replaceAssem : Assem.instr * string -> Assem.instr
         *
         * Replaces the assem string in the given instruction with the given new
         * assem string and returns a new (updated assem instruction
         *)
        fun replaceAssem(Assem.OPER{assem, dst, src, jump}, newAssem) =
            Assem.OPER{assem=newAssem, dst=dst, src=src, jump=jump}
          | replaceAssem(Assem.MOVE{assem, dst, src}, newAssem) =
            Assem.MOVE{assem=newAssem, dst=dst, src=src}
          | replaceAssem(Assem.LABEL{assem, lab}, newAssem) =
            Assem.LABEL{assem=newAssem, lab=lab}
      in
        MakeGraph.instrs2graph(ListPair.map replaceAssem (body, map format body))
      end

(* emitproc: TextIO.outstream -> Frame.frag -> unit
 *
 * Process and emit all function proc framents, but skip all string fragments
 *)
fun emitproc out (Frame.PROC{body,frame}) =
    let
      val _ = (* print to indicate start of new proc/frame *)
          print ("emit " ^ Symbol.name(Frame.name(frame)) ^ "\n")
      (* Call canonicalizer functions to linearize the body of the fragment
       * into basic blocks*)
      val stms = Canon.linearize body
      val stms' = Canon.traceSchedule(Canon.basicBlocks stms)
      (* Apply codeGen to transform the list of Tree statements into
       * a list of mips assembly instructions *)
      val instrs =  List.concat(map (MipsGen.codeGen frame) stms')
      val instrs' = Frame.procEntryExit2(frame, instrs)
      val {prolog, body=finalBody, epilog} = Frame.procEntryExit3(frame, instrs')
      (* Format the resulting assembly instructions to insert correct
       * temps/registers *)
      val format0 = Assem.format(modifiedMakeString)
      (* Create the control flow graph for the formatted body *)
      val (fg, nodes) = createFlowGraph(finalBody, format0)
      (* Compute liveness for the CFG and create the interference graph *)
      val interGraph = Liveness.computeLivenessAndBuild(fg)
      val _ = Liveness.show(TextIO.stdOut, interGraph)
    in
      (* Output the prolog, then the final proc body, followed by the epilog *)
      (TextIO.output(out, prolog);
       (app (fn i => TextIO.output(out,format0 i)) finalBody);
       TextIO.output(out, epilog))
    end
  (* Skip all string fragments in this pass *)
  | emitproc out (Frame.STRING(lab,s)) = ()

(* emitString: TextIO.outstream -> Frame.frag -> unit
 *
 * Emit all string framents, but skip all function proc fragments
 *)
fun emitString out (Frame.STRING(lab,s)) = TextIO.output(out, Frame.string(lab,s))
  | emitString out (Frame.PROC{body, frame}) = ()

(* withOpenFile: string -> (TextIO.outstream -> unit) -> unit
 *
 * Open the given file and apply the given function to it before
 * closing it.
 *)
fun withOpenFile fname f =
    let val out = TextIO.openOut fname
    in (f out before TextIO.closeOut out)
        handle e => (TextIO.closeOut out; raise e)
    end

(* compile: string -> unit
 *
 * Main entry point to the compiler.
 *)
fun compile filename =
    let val absyn = Parse.parse filename
        val frags = (FindEscape.findEscape absyn; Semant.transProg absyn;
                     Translate.getResult())
    in
      withOpenFile (filename ^ ".s")
                   (fn out => ((app (emitString out) frags);
                               (app (emitproc out) (tl frags));
                               (emitproc out) (hd frags)))
    end
end
