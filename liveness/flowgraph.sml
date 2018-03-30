structure Flow =
struct
    datatype flowgraph = FGRAPH of {control: Graph.graph,
                                    def: Temp.temp list Graph.Table.table,
                                    use: Temp.temp list Graph.Table.table,
                                    ismove: bool Graph.Table.table}

  (* Note:  any "use" within the block is assumed to be BEFORE a "def"
        of the same variable.  If there is a def(x) followed by use(x)
       in the same block, do not mention the use in this data structure,
       mention only the def.

     More generally:
       If there are any nonzero number of defs, mention def(x).
       If there are any nonzero number of uses BEFORE THE FIRST DEF,
           mention use(x).

     For any node in the graph,
           Graph.Table.look(def,node) = SOME(def-list)
           Graph.Table.look(use,node) = SOME(use-list)
   *)

    fun printFG(FGRAPH{control, def, use, ismove}) =
        let
          fun stringTempList([], acc) = acc^")"
            | stringTempList(t::ts, acc) =
                stringTempList(ts, acc^Temp.makeString(t)^", ")

          val nodeList = Graph.nodes(control)
          fun printNode(node) =
            (print(Graph.nodename(node)^": ");
             print(stringTempList(valOf(Graph.Table.look(def, node)), " ("));
             print(stringTempList(valOf(Graph.Table.look(use, node)), " ("));
             print(" "^Bool.toString(valOf(Graph.Table.look(ismove, node)))^"\n");
             (app (fn (n) => print(Graph.nodename(n)^", ")) (Graph.succ(node)));
             print("\n"))
        in
            app printNode nodeList
        end
end
