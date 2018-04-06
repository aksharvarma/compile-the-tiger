signature LIVENESS = sig
  structure IG: GRAPH where S=Mips.RegSet
  val analyze: {mention: Mips.reg -> unit, 
	        interfere: Mips.reg -> Mips.reg -> unit} ->
               Mips.funcode -> unit
  val interference_graph: Mips.funcode -> IG.graph
  val printgraph: (string->unit) -> IG.graph -> unit
end

structure Liveness : LIVENESS = struct
  structure IG = Graph(Mips.RegSet)
  structure M = Mips
  structure RS = Mips.RegSet

 fun analyze {mention: M.reg -> unit, interfere: M.reg -> M.reg -> unit}
             (blocks: M.codeblock list) =
    ErrorMsg.impossible "Liveness.analyze unimplemented"

 fun printadj say g i = 
     (say (M.reg2name i); say ":";
      IG.S.app (fn j => (say " "; say (M.reg2name j))) (IG.adj g i);
      say "\n")

 fun printgraph say g = IG.S.app (printadj say g) (IG.nodes g);

 fun interference_graph(func: M.funcode) =
  let val _ = (print "################## LIVENESS: "; 
               print (Symbol.name(#1(List.nth(func,0)))); print "\n")
      val g = IG.newGraph()
      fun mention (r: M.reg) = (IG.succ g r; ())
      fun interfere r1 r2 = IG.mk_edge g {from=r1,to=r2}
   in analyze {mention=mention,interfere=interfere} func;
      print "################## INTERFERENCE GRAPH \n";
      printgraph print g;
      g
  end

end
