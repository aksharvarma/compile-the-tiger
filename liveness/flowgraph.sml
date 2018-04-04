structure Flow =
struct
datatype flowgraph = FGRAPH of {control: Graph.graph,
                                def: Temp.temp list Graph.Table.table,
                                use: Temp.temp list Graph.Table.table,
                                ismove: bool Graph.Table.table}

(* For debugging only:
 * Used to map nodes to their associated assembly instruction strings
 *)
val debugHelper: string Graph.Table.table ref = ref Graph.Table.empty

(* printFG : flowgraph -> unit
 *
 * For debugging purposes only.
 * Prints information associated with the given flow graph.
 * Prints the node name, and for each node:
 * - def set
 * - use set
 * - ismove (bool)
 * - predecessor nodes
 * - successor nodes
 *)
fun printFG(FGRAPH{control, def, use, ismove}) =
    let
      (* stringTempList : Temp.temp list * string -> string
       *
       * Prints the given temp list in a comma separated list followed by a
       * close paren.
       *)
      fun stringTempList([], acc) = acc^")"
        | stringTempList(t::[], acc) = acc^Temp.makeString(t)^")"
        | stringTempList(t::ts, acc) =
          stringTempList(ts, acc^Temp.makeString(t)^", ")

      (* nodeList: Graph.node list *)
      val nodeList = Graph.nodes(control)

      (* printNode : Graph.node -> unit
       *
       * Prints information associated with the given node:
       * - node name
       * - associated assembly instruction
       * - def set
       * - use set
       * - ismove
       * - predecessor nodes
       * - successor nodes
       *)
      fun printNode(node) =
          (print("\n"^Graph.nodename(node)^": ");
           print(valOf(Graph.Table.look(!debugHelper, node)));
           print("def:"^stringTempList(valOf(Graph.Table.look(def, node)),
                                       " ("));
           print(", use:"^stringTempList(valOf(Graph.Table.look(use, node)),
                                         "("));
           print(", ismove:"^Bool.toString(valOf(Graph.Table.look(ismove, node)))
                 ^"\n");
           print("pred: ");
           (app (fn (n) => print(Graph.nodename(n)^" ")) (Graph.pred(node)));
           print("\nsucc: ");
           (app (fn (n) => print(Graph.nodename(n)^" ")) (Graph.succ(node)));
           print("\n"))
    in
      app printNode nodeList
    end
end
