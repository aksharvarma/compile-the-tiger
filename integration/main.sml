structure Main = struct

(* emitproc: TextIO.outstream -> Frame.frag -> unit
 *
 * Process and emit all function proc framents, but skip all string fragments
 *)
fun emitproc out (Frame.PROC{body,frame}) =
    let
      val _ = (* print to indicate start of new proc/frame *)
          print ("####\temit " ^ Symbol.name(Frame.name(frame)) ^ "\n")
      (* Call canonicalizer functions to linearize the body of the fragment
       * into basic blocks*)
      val stms = Canon.linearize body
      val stms' = Canon.traceSchedule(Canon.basicBlocks stms)
      (* Apply codeGen to transform the list of Tree statements into
       * a list of mips assembly instructions *)
      val instrs =  List.concat(map (MipsGen.codeGen frame) stms')
      val instrs' = Frame.procEntryExit2(frame, instrs)
      (* Call the register allocator:
       * - creates a CFG
       * - computes liveness
       * - builds an interference graph
       * - runs register allocation and assigns final registers
       *)
      val (almostReggedInstrs, allocMap) = RegAlloc.alloc(instrs', frame)
      val {prolog, body=finalBody, epilog} = Frame.procEntryExit3(frame,
                                                                  almostReggedInstrs)
      (* Format the resulting assembly instructions to insert correct
       * temps/registers *)
      val format0 = Assem.format(Frame.tempToString allocMap)
    in
      (* Output the prolog, then the final proc body, followed by the epilog *)
      (TextIO.output(out, prolog);
       (app (fn i => TextIO.output(out,format0 i)) almostReggedInstrs);
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

(* assemFilename: string -> string
 * Get the filename to save compiled assembly into.
 *)
fun assemFilename(filename) = filename ^ ".swor"

(* execFilename: string -> string
 * Get the filename to save compiled assembly into.
 *)
fun execFilename(filename) = filename ^ ".s"

(* compile: string -> unit
 *
 * Main function that produces assembly code from tiger file.
 *)
fun compile filename =
    let val absyn = Parse.parse filename
        val frags = (FindEscape.findEscape absyn; Semant.transProg absyn;
                     Translate.getResult())

        fun isStringFrag(Frame.STRING(_)) = true
          | isStringFrag(Frame.PROC(_)) = false

        val strFrags = List.filter isStringFrag frags
        val procFrags = List.filter (fn f => not(isStringFrag f)) frags
        val outFilename = assemFilename(filename)
    in
      withOpenFile (outFilename)
                   (fn out => (if List.null(strFrags)
                               then ()
                               else TextIO.output(out, ".data\n.align 4\n");
                               (app (emitString out) strFrags);
                               TextIO.output(out, ".text\n");
                               (app (emitproc out) (tl procFrags));
                               (emitproc out) (hd procFrags)))
    end

(* makeExecutable: string * string -> unit
 * 
 * Concatenate the runtime and the file to get executable assembly.
 *)
fun makeExecutable(runtime, filename) =
      let
        val assemFile =
            TextIO.inputAll (TextIO.openIn (assemFilename filename))
        val runtimeFile = TextIO.inputAll (TextIO.openIn runtime)
      in
        withOpenFile (execFilename(filename))
                     (fn out =>
                         (TextIO.output(out, runtimeFile);
                          TextIO.output(out, assemFile)))
      end

(* compileToExecutable: string * string -> unit
 *
 * Compile the tiger file, then make executable assembly file.
 *)
fun compileToExecutable(runtime, filename) =
    (compile(filename);
     makeExecutable(runtime, filename))
           
end
