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

      fun copyIGraph(Liveness.IGRAPH{graph, tnode, gtemp, moves}) =
          let
            val graphCopy = UGraph.newGraph()
            val _ = map (fn _ => UGraph.newNode graphCopy) (UGraph.nodeList(graph))

            fun nodeMap([], [], tab) = tab
              | nodeMap(newN::newNs, oldN::oldNs, tab) =
                nodeMap(newNs, oldNs, UGraph.Table.enter(tab, newN, oldN))
              | nodeMap(_, _, tab) = tab (* Just to satisfy the type-checker
                                          * will never happen
                                          *)
                
            val newToOldTab = nodeMap(UGraph.nodeList(graphCopy),
                                      UGraph.nodeList(graph),
                                      UGraph.Table.empty)
            val oldToNewTab = nodeMap(UGraph.nodeList(graph),
                                      UGraph.nodeList(graphCopy),
                                      UGraph.Table.empty)

            fun findNode(tab, n) =
                case UGraph.Table.look(tab, n)
                              of SOME(N) => N
                               | NONE => let exception wontHappen
                                         in raise wontHappen end

            fun gtempCopy(n) =
                gtemp(findNode(newToOldTab, n))

            fun tnodeCopy(t) =
                findNode(oldToNewTab, tnode(t))
                
            fun makeEdges([]) = ()
              | makeEdges(n::ns) =
                let
                  fun copyAdjacency([]) = ()
                    | copyAdjacency(m::ms) =
                      (UGraph.mkEdge graphCopy (findNode(oldToNewTab, n),
                                                findNode(oldToNewTab, m));
                       copyAdjacency(ms))
                in
                (copyAdjacency(UGraph.adjList graph n);
                 makeEdges(ns))
                end
                
            val _ = makeEdges(UGraph.nodeList(graph))

            val movesCopy =
                foldr (fn ((oldN, eSet), t) =>
                          UGraph.Table.enter(t, findNode(oldToNewTab, oldN),
                                             eSet))
                      UGraph.Table.empty (UGraph.Table.listItemsi(moves))
          in
            Liveness.IGRAPH{graph=graphCopy,
                            gtemp=gtempCopy,
                            tnode=tnodeCopy,
                            moves=movesCopy}
          end

      val (igraphCopy as Liveness.IGRAPH{graph=graphCopy,
                                         tnode=tnodeCopy,
                                         gtemp=gtempCopy,
                                         moves=movesCopy}) =
          copyIGraph(igraph)
            
      fun nodeMoves(n) =
          WL.E.intersection(UGraph.lookUpNode(moves, n),
                            WL.E.union(WL.getMoveSet(WL.ACTIVE),
                                       WL.getMoveSet(WL.MOVES)))

      fun moveRelated(n) = WL.E.isEmpty(nodeMoves(n))
                           
      fun makeWLs() =
          let
            val nodes = UGraph.nodeSet(graph)
            fun getNode() =
                (case UGraph.S.find (fn _ => true) nodes
                  of SOME(i) => i
                   | NONE => let exception SetEmpty
                             in raise SetEmpty end)
          in
            while not(UGraph.S.isEmpty(nodes)) do
                  let
                    val n = getNode()
                    val t = gtemp(n)
                  in
                    if List.exists (fn t' => t=t') Frame.physicalRegs
                    then (WL.addNode(WL.PRECOLORED, n))
                    else if (UGraph.degree graph (n)) >= Frame.K
                    then WL.addNode(WL.TOSPILL, n)
                    else if moveRelated(n)
                    then WL.addNode(WL.FREEZE, n)
                    else WL.addNode(WL.SIMPLIFY, n)
                  end
          end

      val _ = makeWLs()

      fun adjacent(n) =
            WL.N.difference(UGraph.adjSet graph n,
                            WL.N.union(WL.getStackSet(),
                                       WL.getNodeSet(WL.COALESCED_N)))


      fun enableMoves(nodes) =
          while not(WL.N.isEmpty(nodes)) do
                let
                  val n = WL.chooseN(nodes)
                  val nMoves = nodeMoves(n)
                in
                  while not(WL.E.isEmpty(nMoves)) do
                        let val m = WL.chooseE(nMoves)
                        in
                          if WL.isEin(WL.ACTIVE, m)
                          then (WL.removeMove(WL.ACTIVE, m);
                                WL.addMove(WL.MOVES, m))
                          else ()
                        end
                end
                
          
      fun processNeighbours(n, []) = ()
        | processNeighbours(n, m::ms) =
          (UGraph.rmEdge graph (n,m);
           if UGraph.degree graph (m) = Frame.K-1
           then (enableMoves(WL.N.union(WL.N.singleton m, adjacent(m)));
                 WL.removeNode(WL.TOSPILL, m);
                 if moveRelated(m)
                 then WL.addNode(WL.FREEZE, m)
                 else WL.addNode(WL.SIMPLIFY, m))
           else ();
           processNeighbours(n, ms)
          )
          
      fun simplify() =
          let
            val n = WL.getNode(WL.SIMPLIFY)
          in
            (WL.pushStack(n);
             processNeighbours(n, UGraph.adjList graph (n))
            )
          end
                         
      fun coalesce() = ()
      fun freeze() = ()
      fun selectSpill() = ()
      fun assignColors() = ()
      (* UGraph.node list *)
      fun rewriteProgram(l) = []
                      

      val spilledNodes: UGraph.node list ref= ref []
    in
      while not(WL.isNullN(WL.SIMPLIFY) andalso
                WL.isNullN(WL.FREEZE) andalso
                WL.isNullN(WL.TOSPILL) andalso
                WL.isNullE(WL.MOVES)) do
            (if WL.isNotNullN(WL.SIMPLIFY)
             then simplify()
             else if WL.isNotNullE(WL.MOVES)
             then coalesce()
             else if WL.isNotNullN(WL.FREEZE)
             then freeze()
             else if WL.isNotNullN(WL.TOSPILL)
             then selectSpill()
             else ());
      assignColors();
      if not(List.null(!spilledNodes))
      then (alloc(rewriteProgram(!spilledNodes), frame))
      else (instrs, Temp.Table.empty)
      (* ([],Temp.Table.empty) *)
    end

end
