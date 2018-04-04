signature MAKEGRAPH =
sig

 (* instrs2graph: Assem.instr list -> Flow.flowgraph * Graph.node list
  *
  * Constructs a control flow graph for the list of given instructions.
  * Returns a flow graph and the list of nodes in the graph.
  *)
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
      (* takeControl : Flow.flowgraph -> Graph.graph
       *
       * Extracts the control graph out of the given flowgraph
       *)
      fun takeControl(F.FGRAPH({control, def, use, ismove})) = control

      (* New initial graph *)
      val startGraph = Graph.newGraph()

      (* Table mapping labels (symbols) to their associated graph nodes *)
      val labelTable = ref Symbol.empty : Graph.node Symbol.table ref

      (* findLabelNode : Temp.label * Graph.graph -> Graph.node
       *
       * Looks up the given label in the label table.
       * If it is already present, then return the graph node already associated
       * with that label.
       * If not found, create a new graph node, add it to the label table, and
       * return the new node
       *)
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

      (* makeNewNode : Flow.flowgraph * Assem.instr -> Graph.node
       *
       * Creates a new node for the given assembly instruction.
       * If the given instruction is a label, first checks if there is already a
       * graph node associated with that label
       *)
      fun makeNewNode(fg, Assem.LABEL({assem, lab})) =
          findLabelNode(lab, takeControl(fg))
        | makeNewNode(fg, _) =
          Graph.newNode(takeControl(fg))

      (* addFallThroughEdge : Graph.node option * Graph.node -> unit
       *
       * If the fromNode is present, add an edge from the given fromNode to the
       * given toNode. Else do nothing
       *)
      fun addFallThroughEdge(NONE, toNode) = ()
        | addFallThroughEdge(SOME(fromNode), toNode) =
          Graph.mk_edge{from=fromNode, to=toNode}

      (* debugMe : Graph.node * Assem.instr -> unit
       *
       * For debugging only.
       * Enters the given node into the debug table mapped to the given assembly
       * instruction.
       *)
      fun debugMe(currNode, instr) =
          F.debugHelper := Graph.Table.enter(!F.debugHelper,
                                             currNode, Assem.getAssem(instr));

      (* updateFG : Flow.flowgraph * Graph.node * Assem.instr -> Flow.flowgraph
       *
       * Updates the given flow graph by adding the information associated with
       * the given node to the def, use, and ismove tables in the flowgraph.
       * Returns a new updated flowgraph.
       *)
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

      (* buildGraph : Assem.instr list * Graph.node * Flow.flowgraph -> Flow.flowgraph
       *
       * Builds  a control flow graph for the given instruction list through
       * accumulation.
       * Makes a new node for each instruction, adds edges from the given
       * fromNode to the current node, adds edges from the current node to all
       * of the nodes that it can jump to, and updates the given flowgraph with
       * the information associated with the given instructions
       *)
      fun buildGraph([], fromNode, fg) = fg
        | buildGraph(instr::instrs, fromNode, fg) =
          let
            (* Node corresponding to the first instruction in the list *)
            val currNode = makeNewNode(fg, instr)
            (* Extract the jumps from the first instruction *)
            val jumps = (Assem.getJumps(instr))
            (* If the current instruction does not have any jumps, then pass the
             * current node to the recursive call (as the fromNode for the next
             * instruction). Else pass NONE.
             *)
            val succNode = if jumps=[] then SOME(currNode) else NONE

            (* addEdgeToLabel : Temp.label -> unit
             *
             * Add an edge from the current node to the node associated with the
             * given label
             *)
            fun addEdgeToLabel(j) =
                Graph.mk_edge{from=currNode, to=findLabelNode(j, takeControl(fg))}
          in
            (* Record debugging information for this node/instruction pair *)
            (debugMe(currNode, instr);
             (* Add the fallthrough edge from the given fromNode to the current
              * node if necessary *)
             addFallThroughEdge(fromNode, currNode);
             (* Add all edges from the current node to the labels that it can
              * jump to *)
             (app addEdgeToLabel jumps);
             (* Recur with an updated flowgraph with the information for the
              * current node added *)
             buildGraph(instrs, succNode, updateFG(fg, currNode, instr)))
          end

      (* builtGraph : Flow.flowgraph
       *
       * Builds a control flow graph for the given list of assem instructions
       *)
      val builtGraph = buildGraph(instrs, NONE, Flow.FGRAPH({control=startGraph,
                                                             def=Graph.Table.empty,
                                                             use=Graph.Table.empty,
                                                             ismove=Graph.Table.empty}))
    in
      (builtGraph, Graph.nodes(takeControl(builtGraph)))
    end
end
