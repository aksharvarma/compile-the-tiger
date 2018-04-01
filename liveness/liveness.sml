signature LIVENESS =
sig
    datatype igraph =
      IGRAPH of {graph: Graph.graph,
                 tnode: Temp.temp -> Graph.node,
                 gtemp: Graph.node -> Temp.temp,
                 moves: (Graph.node * Graph.node) list}

    val interferenceGraph:
          Flow.flowgraph -> igraph * (Graph.node -> Temp.temp list)

    val show: TextIO.outstream * igraph -> unit
end

structure Liveness =
struct
  datatype igraph =
      IGRAPH of {graph: Graph.graph,
                 tnode: Temp.temp -> Graph.node,
                 gtemp: Graph.node -> Temp.temp,
                 moves: (Graph.node * Graph.node) list}

  type liveSet = unit Temp.Table.table * Temp.temp list
  type liveMap = liveSet Graph.Table.table

  (* interferenceGraph:
   *    Flow.flowgraph -> igraph * (Graph.node -> Temp.temp list
   *)
  fun interferenceGraph(Flow.FGRAPH{control, def, use, ismove}) =
    let
      val fgNodes = Graph.nodes(control)
      val interGraph = Graph.newGraph()

      fun getDst(n) = (case Graph.Table.look(def, n)
                          of SOME(dst) => dst
                            | _ => let exception NoDstFound
                                    in raise NoDstFound end)

      fun getSrc(n) = (case Graph.Table.look(use, n)
                            of SOME(src) => src
                             | _ => let exception NoSrcFound
                                    in raise NoSrcFound end)

      fun createNodeTempMaps([], tnode, gtemp) = (tnode, gtemp)
        | createNodeTempMaps(node::nodes, tnode, gtemp) =
            let
              val temps = (getDst node)@(getSrc node)

              fun fillTables([], tempMap, nodeMap) = (tempMap, nodeMap)
                | fillTables(t::ts: Temp.temp list, tempMap, nodeMap) =
                    (case Temp.Table.look(tempMap, t)
                      of SOME(n) => fillTables(ts, tempMap, nodeMap)
                       | NONE =>
                           let
                             val newNode = Graph.newNode(interGraph)
                           in
                             fillTables(ts,
                                         Temp.Table.enter(tempMap, t, newNode),
                                         Graph.Table.enter(nodeMap, newNode, t))
                           end)
              val (tnode', gtemp') = fillTables(temps, tnode, gtemp)
            in
              createNodeTempMaps(nodes, tnode', gtemp')
            end

      val emptyTnode = Temp.Table.empty
      val emptyGtemp = Graph.Table.empty

      val (tnodeMap, gtempMap) =
        createNodeTempMaps(fgNodes, emptyTnode, emptyGtemp)

      fun tnodeFun(temp:Temp.temp) =
        (case Temp.Table.look(tnodeMap, temp)
          of SOME(n) => n
           | NONE => let exception TempNotFound
                     in raise TempNotFound end)

      fun gtempFun(node) =
        (case Graph.Table.look(gtempMap, node)
          of SOME(t) => t
           | NONE => let exception NodeNotFound
                     in raise NodeNotFound end)

      fun computeMoves([]) = []
        | computeMoves(n::ns) =
            case Graph.Table.look(ismove, n)
              of SOME(true) =>
                let
                  val d = hd(getDst(n))
                  val s = hd(getSrc(n))
                in
                   (tnodeFun(d), tnodeFun(s))::computeMoves(ns)
                end
               | _ => computeMoves(ns)

      val liveIn : liveMap = Graph.Table.empty
      val liveOut : liveMap = Graph.Table.empty
(*
      fun initLiveMaps(liveIn, liveOut, []) = (liveIn, liveOut)
        | initLiveMaps(liveIn, liveOut, n::ns) =
          initLiveMaps(Graph.Table.enter(liveIn, n, *)
    in
      (IGRAPH{graph=interGraph,
       tnode=tnodeFun,
       gtemp=gtempFun,
       moves=computeMoves(fgNodes)},
      (fn (n) => []))
    end

  (* show: outstream * igraph -> unit *)
  fun show(out, IGRAPH{graph, tnode, gtemp, moves}) =
      let
        val nodes = Graph.nodes(graph)
      in
        app (fn (node) => (TextIO.output(out, Graph.nodename(node));
                          (app (fn (adj) =>
                                  TextIO.output(out, Graph.nodename(adj)^" "))
                                          (Graph.adj(node)))))
        nodes
      end
end
