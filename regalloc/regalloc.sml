signature REG_ALLOC =
sig
  (* type allocation = Frame.register Temp.Table.table *)
  val alloc: Assem.instr list *Frame.frame -> Assem.instr list * WL.allocation
end

structure RegAlloc: REG_ALLOC =
struct

(* type allocation = Frame.register Temp.Table.table *)


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
      (* val _ = Liveness.show(TextIO.stdOut, igraph) *)

      fun copyAdjList(Liveness.IGRAPH{graph, tnode, gtemp, moves}) =
          (foldr (fn ((n, adjSet), tab) =>
                    UGraph.Table.enter(tab, n, adjSet))
                UGraph.Table.empty
                (map (fn n => (n, UGraph.adjSet graph (n)))
                     (UGraph.nodeList(graph))))

      val adjTab = copyAdjList(igraph)
      
      fun nodeMoves(n) =
          let
            val movesTab = (case UGraph.Table.look(moves, n)
                         of SOME(t) => t
                          | NONE => WL.E.empty)
          in
          WL.E.intersection(movesTab, WL.E.union(WL.getMoveSet(WL.ACTIVE),
                                                 WL.getMoveSet(WL.MOVES)))
          end
                           
      fun moveRelated(n) = WL.E.isEmpty(nodeMoves(n))
                           
      fun makeWLs() =
          let
            val nodes = UGraph.nodeSet(graph)
          in
            UGraph.S.app
              (fn n => 
                  let
                    val t = gtemp(n)
                  in
                    if List.exists (fn t' => t=t') Frame.physicalRegsT
                    then (WL.addNode(WL.PRECOLORED, n))
                    else if (UGraph.degree graph (n)) >= Frame.K
                    then WL.addNode(WL.TOSPILL, n)
                    (* else if moveRelated(n) *)
                    (* then WL.addNode(WL.FREEZE, n) *)
                    else WL.addNode(WL.SIMPLIFY, n)
                  end)
              nodes
          end

      val _ = print("uncomment-for-freeze (regalloc(makeWL))\n")
      val _ = makeWLs()
      val _ = WL.precolor(gtemp)
                         
      fun adjacent(n) =
            WL.N.difference(UGraph.adjSet graph n,
                            WL.N.union(WL.getStackSet(),
                                       WL.getNodeSet(WL.COALESCED_N)))


      fun enableMoves(nodes) =
          WL.N.app
            (fn n =>
                WL.E.app
                  (fn m =>
                      if WL.isEin(WL.ACTIVE, m)
                      then (WL.removeMove(WL.ACTIVE, m);
                            WL.addMove(WL.MOVES, m))
                      else ())
                  (nodeMoves(n)))
            nodes
                
          
      fun processNeighbours(n, []) = ()
        | processNeighbours(n, m::ms) =
          (UGraph.rmEdge graph (n,m);
           if UGraph.degree graph (m) = Frame.K-1
           then (
                 (* enableMoves(WL.N.union(WL.N.singleton m, adjacent(m))); *)
                 (* WL.removeNode(WL.TOSPILL, m); *)
                 (* if moveRelated(m) *)
                 (* then WL.addNode(WL.FREEZE, m) *)
                 (* else *)
                   WL.addNode(WL.SIMPLIFY, m))
           else ();
           processNeighbours(n, ms)
          )
          
      fun simplify() =
          while WL.isNotNullN(WL.SIMPLIFY) do
                let
                  val n = WL.getAndRemoveNode(WL.SIMPLIFY)
                in
                  (WL.pushStack(n);
                   processNeighbours(n, UGraph.adjList graph (n)))
                end
                         
      fun coalesce() = ()
      fun freeze() = ()

      fun freezeMoves(m:UGraph.node) = ()

      fun selectSpill() =
          let
            val m = WL.getAndRemoveNode(WL.TOSPILL)
          in
            (WL.addNode(WL.SIMPLIFY, m);
             (* TODO: Implement freezeMoves *)
             freezeMoves(m))
          end
          
      (* UGraph.node list *)
      fun rewriteProgram(l) = []
                      

      (* val spilledNodes: UGraph.node list ref= ref [] *)
    in
      while not(WL.isNullN(WL.SIMPLIFY) andalso
                WL.isNullN(WL.TOSPILL)) do
      (* while not(WL.isNullN(WL.SIMPLIFY) andalso *)
      (*           WL.isNullN(WL.FREEZE) andalso *)
      (*           WL.isNullN(WL.TOSPILL) andalso *)
      (*           WL.isNullE(WL.MOVES)) do *)
            (if WL.isNotNullN(WL.SIMPLIFY)
             then (simplify();print(""))
             else if WL.isNotNullE(WL.MOVES)
             then (print("coalesce\n");coalesce())
             else if WL.isNotNullN(WL.FREEZE)
             then (print("freeze\n");freeze())
             else if WL.isNotNullN(WL.TOSPILL)
             then (selectSpill();print(""))
             else ());
      let
        (* val _ = print("starting color\n") *)
        val (colorAlloc, spilledList) =
            (Color.color{interference=igraph,
                         adjTab=adjTab,
                         spillCost=(fn n:UGraph.node => 1),
                         registers=Frame.registers})
        (* val _ = print("ending color\n") *)
                       
      in
        if not(List.null(spilledList))
        then
          (let exception cannotAlloc in raise cannotAlloc end)
           (* alloc(rewriteProgram(!spilledNodes), frame)) *)
        else (instrs, colorAlloc)
      end
    end

end
