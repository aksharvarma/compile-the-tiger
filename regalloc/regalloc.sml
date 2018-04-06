signature REG_ALLOC =
sig
  type allocation = Frame.register Temp.Table.table
  val alloc: Assem.instr list *Frame.frame -> Assem.instr list * allocation
end

structure RegAlloc: REG_ALLOC =
struct

type allocation = Frame.register Temp.Table.table


(* alloc: Assem.instr list *Frame.frame -> Assem.instr list * allocation
 * 
 *)
fun alloc(instrs, frame) =
    let
      (* Flow.flowgraph * Graph.node list *)
      val (cfg, fgNodes) = MakeGraph.instrs2graph(instrs)

      val _ = WL.initialize()

      (* Also builds the interference graph *)
      val (igraph as Liveness.IGRAPH{graph, tnode, gtemp, moves}) =
          Liveness.computeLivenessAndBuild(cfg)

      val _ = WL.makeWLs(igraph)
                       
      fun simplify() = ()
      fun coalesce() = ()
      fun freeze() = ()
      fun selectSpill() = ()
      fun assignColors() = ()
      (* UGraph.node list *)
      fun rewriteProgram(l) = []
                      

      val spilledNodes: UGraph.node list ref= ref []
    in
      while not(WL.isNull(WL.SIMPLIFY) andalso
                WL.isNull(WL.FREEZE) andalso
                WL.isNull(WL.SPILL) andalso
                WL.isNull(WL.MOVES)) do
            (if WL.isNotNull(WL.SIMPLIFY)
             then simplify()
             else if WL.isNotNull(WL.MOVES)
             then coalesce()
             else if WL.isNotNull(WL.FREEZE)
             then freeze()
             else if WL.isNotNull(WL.SPILL)
             then selectSpill()
             else ());
      assignColors();
      if not(List.null(!spilledNodes))
      then (alloc(rewriteProgram(!spilledNodes), frame))
      else (instrs, Temp.Table.empty)
      (* ([],Temp.Table.empty) *)
    end

end
