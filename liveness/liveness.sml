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

structure ListSet = ListSetFn (struct
                                type ord_key = Temp.temp
                                val compare = Int.compare
                                end)
                       
                       
(* interferenceGraph:
 *    Flow.flowgraph -> igraph * (Graph.node -> Temp.temp list
 *)
fun interferenceGraph(Flow.FGRAPH{control, def, use, ismove}) =
    let
      val fgNodes = Graph.nodes(control)
      val interGraph = Graph.newGraph()

      (* Need to write this to create K_{32,32} for physical regs 
       * This function's output to be used to create initial tnode, gtemp maps
       *)
      fun addPhysicalRegs() = ()
                                     
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

      (* Call addPhysicalRegs instead of initializing to empty *)
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

      val liveIn: liveMap = 
          foldr (fn (n, tab) =>
                    Graph.Table.enter(tab, n, (Temp.Table.empty, [])))
                Graph.Table.empty (Graph.nodes(interGraph))
      val liveOut: liveMap = 
          foldr (fn (n, tab) =>
                    Graph.Table.enter(tab, n, (Temp.Table.empty, [])))
                Graph.Table.empty (Graph.nodes(interGraph))
                                    
      fun printLiveMaps(inMap, outMap) =
          let
            val nodeNums = Graph.nodes(control)
            fun getTempList(n, map) =
                case Graph.Table.look(map, n)
                 of SOME(t, l) => l
                  | NONE => let exception NodeNotFound
                            in raise NodeNotFound end

            fun printNodeLives([]) = ()
              | printNodeLives(n::ns) =
                (print("\nIN:  ");
                 (app (fn t => print(Temp.makeString(t)^" "))
                      (getTempList(n, inMap)));
                 print("\n"^Graph.nodename(n)^": ");
                 print(valOf(Graph.Table.look(!(Flow.debugHelper), n)));
                 print("OUT: ");
                 (app (fn t => print(Temp.makeString(t)^" "))
                      (getTempList(n, outMap)));
                 print("\n");
                 printNodeLives(ns))
          in
             printNodeLives(nodeNums)
          end

      val iterationCount = ref 0
      (* Better this way *)
      val initialWL = rev(Graph.nodes(control))
      (* For debugging using Appel's algo (livenessChanged) *)
      val livenessChanged = ref false
                         
      fun livenessWL([], liveIn, liveOut) = (liveIn, liveOut)
        (* For debugging using Appel's algo (livenessChanged) *)
        (* (if !livenessChanged *)
        (*  then (livenessChanged:=false; *)
        (*        livenessWL(initialWL, liveIn, liveOut)) *)
        (*  else (liveIn, liveOut)) *)
        | livenessWL(b::wl, liveIn, liveOut) =
          let
            val useTable = foldr (fn (elem, tab) =>
                                     Temp.Table.enter(tab, elem, ()))
                                 Temp.Table.empty (getSrc(b))
            val defTable = foldr (fn (elem, tab) =>
                                     Temp.Table.enter(tab, elem, ()))
                                 Temp.Table.empty (getDst(b))

            fun getLiveSet(lmap:liveMap, node:Graph.node) =
                (case Graph.Table.look(lmap, node)
                  of SOME(s) => s
                  | _ => let exception NodeNotFoundInLiveInMap
                         in raise NodeNotFoundInLiveInMap end)

            fun getLiveTable(lmap:liveMap, node:Graph.node) =
                let val (tab, _) = getLiveSet(lmap, node)
                in tab end

            fun getLiveList(lmap:liveMap, node:Graph.node) =
                let val (_, tempList) = getLiveSet(lmap, node)
                in tempList end
                  
            val bInTable = getLiveTable(liveIn, b)
            val bInTemps = getLiveList(liveIn, b)
            val bOutTable = getLiveTable(liveOut, b)
            val bOutTemps = getLiveList(liveOut, b)

            fun dummyF((), ()) = ()
                                   
            val newInTable =
                Temp.Table.union dummyF (useTable,
                                         Temp.Table.difference(bOutTable,
                                                               defTable))

            val succInTables = map (fn succ => getLiveTable(liveIn, succ))
                                   (Graph.succ(b))
            val newOutTable = foldr (fn (t1, t2) =>
                                        Temp.Table.union dummyF (t1, t2))
                                    Temp.Table.empty (succInTables)
            (* val changeB =  *)
            val newInTemps = map (fn (k, elem) => k)
                                 (Temp.Table.listItemsi(newInTable))
            val newOutTemps = map (fn (k, elem) => k)
                                 (Temp.Table.listItemsi(newOutTable))
            val newLiveIn =
                Graph.Table.enter(liveIn,
                                  b, (newInTable, newInTemps))
            val newLiveOut =
                Graph.Table.enter(liveOut,
                                  b, (newOutTable, newOutTemps))


            val oldInSize = List.length(bInTemps)
            val newInSize = List.length(newInTemps)
            val oldOutSize = List.length(bOutTemps)
            val newOutSize = List.length(newOutTemps)

            (* For debugging, to see if we remove anything from live sets *)
            fun isSubset([], _) = true
              | isSubset(item::rest:Temp.temp list, l2) = 
                if List.null(List.filter(fn x => x = item) l2)
                then false else isSubset(rest, l2)

            val newWL = wl@
                        (if newInSize > oldInSize
                            (* The line below shouldn't be needed *)
                            orelse newOutSize > oldOutSize
                         then Graph.pred(b)
                         else [])

            (* val _ = (print("----------\nNew Iteration: "^Graph.nodename(b)^"\n"); *)
            (*          (app (fn n => print(Graph.nodename(n)^" ")) newWL); *)
            (*          print("\n"); *)
            (*          printLiveMaps(newLiveIn, newLiveOut)) *)

          in
            (
              (* For debugging using Appel's algo (livenessChanged) *)
              (* if oldInSize < newInSize orelse oldOutSize < newOutSize *)
              (* then (livenessChanged := true) *)
              (* else (); *)

              (* Raise an exception if we removed anything from live sets *)
              if not(isSubset(bInTemps, newInTemps)) orelse
                 not(isSubset(bOutTemps, newOutTemps))
              then let exception YouDeletedSomething
                   in raise YouDeletedSomething end
              else ();
              (* Keep track of iteration count *)
              iterationCount:= !iterationCount+1;
              livenessWL(newWL, newLiveIn, newLiveOut))
          end
            
      val (finalLiveIn, finalLiveOut) = livenessWL(initialWL,
                                                   liveIn, liveOut)

      val _ = (print("--------\n");
               print("IterationCount: "^Int.toString(!iterationCount)^"\n");
               printLiveMaps(finalLiveIn, finalLiveOut);
               print("--------\n"))
      (* val _ = (print("--------\nLive-OUT\n"); *)
      (*          printLiveMaps(finalLiveOut)) *)


      fun makeAdj(a, b) =
          (Graph.mk_edge{from=a, to=b};
           Graph.mk_edge{from=b, to=a})

      (* Go through fg nodes, 
       * add interference edges where appropriate 
       * Basic implementation. Haven't tested.
       *)
      fun interfere([]) = ()
        | interfere(n::ns) =
          let
            val outTemps =
                (case Graph.Table.look(finalLiveOut, n)
                  of SOME((tab, lst)) => lst
                  | NONE => let exception NodeNotFound
                            in raise NodeNotFound end)

            val nIsMove =
                (case Graph.Table.look(ismove, n)
                  of SOME(t) => t
                   | NONE => let exception NodeNotFound
                             in raise NodeNotFound end)

            fun goThroughDsts([]) = ()
              | goThroughDsts(d::ds) =
                let
                  val inTemps = getSrc(n)
                  fun deleteFromList(item:Temp.temp, list) =
                      List.filter(fn x => x <> item) list
                  val effectiveOuts =
                      if nIsMove
                      then deleteFromList(hd(inTemps), outTemps)
                      else outTemps
                in
                  ((app (fn out => makeAdj(tnodeFun d, tnodeFun out))
                        effectiveOuts);
                   goThroughDsts(ds))
                end
                  
          in
            (if nIsMove
             then ()            (* No interference if move instruction *)
             else goThroughDsts(getDst(n));
             interfere(ns))
          end
          
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
