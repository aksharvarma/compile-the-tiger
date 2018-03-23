structure Main = struct

   structure Tr = Translate
   structure F = Frame
   (* structure R = RegAlloc *)

 fun getsome(SOME x) = x
   | getsome(NONE) =
     let
       exception getsomeNONE
     in
       raise getsomeNONE
     end

   fun emitproc out (F.PROC{body,frame}) =
     let val _ = print ("emit " ^ Symbol.name(F.name(frame)) ^ "\n")
         val stms = Canon.linearize body
         val stms' = Canon.traceSchedule(Canon.basicBlocks stms)
         val instrs =  List.concat(map (MipsGen.codeGen frame) stms')
         val format0 = Assem.format(Temp.makeString)
     in
       (app (fn stm => (Printtree.printtree(TextIO.stdOut, stm);
            print("--------\n"))) stms';
        app (fn i => TextIO.output(out,format0 i)) instrs)
     end
     (* end *)
    | emitproc out (F.STRING(lab,s)) = TextIO.output(out,F.string(lab,s))

   fun withOpenFile fname f =
       let val out = TextIO.openOut fname
        in (f out before TextIO.closeOut out)
            handle e => (TextIO.closeOut out; raise e)
       end

   fun compile filename =
       let val absyn = Parse.parse filename
           val frags = (FindEscape.findEscape absyn; Semant.transProg absyn;
                        Translate.getResult())
        in
            withOpenFile (filename ^ ".s")
             (fn out => (app (emitproc out) frags))
       end

end



