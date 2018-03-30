signature MAKEGRAPH =
sig
  val instrs2graph: Assem.instr list -> Flow.flowgraph * Graph.node list
end

(* Constructs a control flow graph for a list of assembly instructions *)
structure MakeGraph :> MAKEGRAPH =
struct
  structure F = Flow
  (* instrs2graph: Assem.instr list -> Flow.flowgraph * Graph.node list
   *
   * Constructs a control flow graph for the list of given instructions.
   * Returns a flow graph and the list of nodes in the graph.
   *)
   fun instrs2graph(instrs) =
      let
        fun takeControl(F.FGRAPH({control, def, use, ismove})) = control

        val startGraph = Graph.newGraph()
        val labelTable = ref Symbol.empty : Graph.node Symbol.table ref

        fun findLabelNode(label, graph) =
          case Symbol.look(!labelTable, label)
            of SOME(node) => node
             | NONE =>
                let
                  val newNode = Graph.newNode(graph)
                in
                  (labelTable := Symbol.enter(!labelTable, label, newNode);
                   newNode)
                end

        fun updateFG(F.FGRAPH({control, def, use, ismove}),
                      node, dst, src, moveInstr) =
                let
                  val newDef = Graph.Table.enter(def, node, dst)
                  val newUse = Graph.Table.enter(use, node, src)
                  val newIsmove= Graph.Table.enter(ismove, node, moveInstr)
                in
                  F.FGRAPH({control=control, def=newDef, use=newUse,
                            ismove=newIsmove})
                end

        fun buildGraph([], fg) = fg
          | buildGraph(instr::instrs, fg) =
          let
            fun innerBuildGraph([], fromNode, graph) = graph
              | innerBuildGraph(Assem.LABEL({assem, lab})::instrs, fromNode, fg) =
                    let
                      val currNode = findLabelNode(lab, takeControl(fg))
                      val _ = printFoo(assem, fg)
                    in
                      (Graph.mk_edge{from=fromNode, to=currNode};
                       innerBuildGraph(instrs, currNode, updateFG(fg, currNode, [], [],
                                 false)))
                    end
              | innerBuildGraph(Assem.OPER({assem, dst, src, jump})::instrs, fromNode, fg) =
                    let
                      val currNode = Graph.newNode(takeControl(fg))
                      val _ = printFoo(assem, fg)
                      val jumps =
                        case jump
                          of SOME(l) => l
                           | NONE => (Graph.mk_edge{from=fromNode, to=currNode};
                             [])
                      fun addJumpEdge(j) =
                        Graph.mk_edge{from=currNode, to=findLabelNode(j,
                         takeControl(fg))}
                    in
                      ((app addJumpEdge jumps);
                       innerBuildGraph(instrs, currNode, updateFG(fg, currNode, dst, src,
                                 false)))
                    end
              | innerBuildGraph(Assem.MOVE({assem, dst, src})::instrs, fromNode, fg) =
                    let
                      val currNode = Graph.newNode(takeControl(fg))
                      val _ = printFoo(assem, fg)
                    in
                      (Graph.mk_edge{from=fromNode, to=currNode};
                       innerBuildGraph(instrs, currNode, updateFG(fg, currNode, [dst],
                      [src], true)))
                    end

            and printFoo(assem, fg) =
                    (print(assem);
                    (app (fn (n) => print(Graph.nodename(n)^", "))
                    (Graph.nodes(takeControl(fg)));
                    print("\n")))

            val firstNode = Graph.newNode(takeControl(fg))
            val newFG =
              case instr
               of Assem.OPER{assem, dst, src, jump} =>
                    let
                      val jumps =
                        case jump
                          of SOME(l) => l
                           | NONE => []
                      fun addJumpEdge(j) =
                        Graph.mk_edge{from=firstNode, to=findLabelNode(j,
                         takeControl(fg))}
                    in
                      ((app addJumpEdge jumps);
                       updateFG(fg, firstNode, dst, src, false))
                    end
                | Assem.MOVE{assem, dst, src} =>
                    updateFG(fg, firstNode, [dst], [src], true)
                | Assem.LABEL{assem, lab} =>
                    (labelTable := Symbol.enter(!labelTable, lab, firstNode);
                     updateFG(fg, firstNode, [], [], false))

          in
            innerBuildGraph(instrs, firstNode, newFG)
          end
        val builtGraph = buildGraph(instrs, Flow.FGRAPH({control=startGraph,
                                             def=Graph.Table.empty,
                                             use=Graph.Table.empty,
                                             ismove=Graph.Table.empty}))
      in
        (builtGraph, Graph.nodes(takeControl(builtGraph)))
      end
end
