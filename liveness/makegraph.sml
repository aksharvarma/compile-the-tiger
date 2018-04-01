signature MAKEGRAPH =
sig
  val debugHelper: string Graph.Table.table ref
  val instrs2graph: Assem.instr list -> Flow.flowgraph * Graph.node list
end

(* Constructs a control flow graph for a list of assembly instructions *)
structure MakeGraph :> MAKEGRAPH =
struct
structure F = Flow

val debugHelper: string Graph.Table.table ref = ref Graph.Table.empty
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


      fun makeNewNode(fg, Assem.LABEL({assem, lab})) =
          findLabelNode(lab, takeControl(fg))
        | makeNewNode(fg, _) =
          Graph.newNode(takeControl(fg))

      fun addFallThroughEdge(NONE, toNode) = ()
        | addFallThroughEdge(SOME(fromNode), toNode) =
          Graph.mk_edge{from=fromNode, to=toNode}

      fun debugMe(currNode, instr) =
          F.debugHelper := Graph.Table.enter(!F.debugHelper,
                                             currNode, Assem.getAssem(instr));

      fun updateFG(F.FGRAPH({control, def, use, ismove}), node, instr) =
          let
            val newDef = Graph.Table.enter(def, node, Assem.getDst(instr))
            val newUse = Graph.Table.enter(use, node, Assem.getSrc(instr))
            val newIsmove= Graph.Table.enter(ismove, node, Assem.isMove(instr))
          in
            F.FGRAPH({control=control,
                      def=newDef, use=newUse,
                      ismove=newIsmove})
          end
            
      fun buildGraph([], fromNode, fg) = fg
        | buildGraph(instr::instrs, fromNode, fg) =
          let
            val currNode = makeNewNode(fg, instr)
            fun addEdgeToLabel(j) =
                Graph.mk_edge{from=currNode, to=findLabelNode(j, takeControl(fg))}
            val jumps = (Assem.getJumps(instr))
            val succNode = if jumps=[] then SOME(currNode) else NONE
          in
            (debugMe(currNode, instr);
             addFallThroughEdge(fromNode, currNode);
             (app addEdgeToLabel jumps);
             buildGraph(instrs, succNode, updateFG(fg, currNode, instr)))
          end      

      val builtGraph = buildGraph(instrs, NONE, Flow.FGRAPH({control=startGraph,
                                                             def=Graph.Table.empty,
                                                             use=Graph.Table.empty,
                                                             ismove=Graph.Table.empty}))

    in
      (builtGraph, Graph.nodes(takeControl(builtGraph)))
    end
end
