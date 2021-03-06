signature LIVENESS =
sig
  type tnodeFn
  type gtempFn

  (* An interference graph and related information. Contains:
   * - graph : the actual interference graph
   * - tnode : a mapping from temps to UGraph nodes
   * - gtemp : the inverse mapping from UGraph nodes to temps
   * - moves : a list of move instructions
   *)
  datatype igraph =
           IGRAPH of {graph: UGraph.graph,
                      tnode: tnodeFn,
                      gtemp: gtempFn,
                      moves: (UGraph.node * UGraph.node) list}

  (* Maps graph nodes to a particular live set *)
  type liveMap

  (* Constructs an interference graph from the given flow graph
   * by first computing liveness and then using the live out sets
   * to create the interference graph
   *)
  val computeLivenessAndBuild: Flow.flowgraph -> igraph

  (* For debugging purposes: prints out a list of nodes in the interference
   * graph and a list of all nodes adjacent to it
   *)
  val show: TextIO.outstream * igraph -> unit
end

(* Contains the logic for computing liveness for a given flow graph
 * and constructing an interference graph from the live sets
 *)
structure Liveness:LIVENESS =
struct

  type tnodeFn = Temp.temp -> UGraph.node
  type gtempFn = UGraph.node -> Temp.temp
  (* An interference graph and related information. Contains:
   * - graph : the actual interference graph
   * - tnode : a mapping from temps to UGraph nodes
   * - gtemp : the inverse mapping from UGraph nodes to temps
   * - moves : a list of move instructions
   *)
  datatype igraph =
           IGRAPH of {graph: UGraph.graph,
                      tnode: tnodeFn,
                      gtemp: gtempFn,
                      moves: (UGraph.node * UGraph.node) list}

(* Represents a set of temps that are live at a particular time.
 * The representation is redundant:
 * - the table is useful for efficient membership tests
 * - the list is useful for enumerating all live variables in the set
 *)
type liveSet = unit Temp.Table.table * Temp.temp list

(* Maps graph nodes to a particular live set *)
type liveMap = liveSet Graph.Table.table

(* interferenceGraph: Flow.flowgraph -> igraph * (Graph.node -> Temp.temp list
 *
 * Constructs an interference graph from the given flow graph
 * by first computing liveness and then using the live out sets
 * to create the interference graph
 *)
fun computeLivenessAndBuild(Flow.FGRAPH{control, def, use, ismove}) =
    let
      val fgNodes = Graph.nodes(control)
      val interGraph = UGraph.newGraph()

      (* getDef : Graph.node -> Temp.temp list
       *
       * Gets the list of temps that are in the def set of the given node
       *)
      fun getDef(n) = (case Graph.Table.look(def, n)
                        of SOME(def) => def
                         | _ => let exception NoDstFound
                                in raise NoDstFound end)

      (* getUse : Graph.node -> Temp.temp list
       *
       * Gets the list of temps that are in the use set of the given node
       *)
      fun getUse(n) = (case Graph.Table.look(use, n)
                        of SOME(use) => use
                         | _ => let exception NoSrcFound
                                in raise NoSrcFound end)

      (* createNodeTempMaps :
       *        Graph.node list * Temp.Table.table * Graph.Table.table
       *                -> Temp.Table.table * Graph.Table.table
       *
       * Create the tnode and gtemp tables that map the temps in the graph to
       * nodes and vice versa.
       *)
      fun createNodeTempMaps([], tnode, gtemp) = (tnode, gtemp)
        | createNodeTempMaps(node::nodes, tnode, gtemp) =
          let
            (* fillTables : Temp.temp list * Temp.Table.table * Graph.Table.table
             *                  -> Temp.Table.table * Graph.Table.table
             *
             * Fill the given tables with the given list of temps. If the node
             * associated with a temp is not already in the tempMap, then create
             * a new node in the intergraph for it.
             *)
            fun fillTables([], tempMap, nodeMap) = (tempMap, nodeMap)
              | fillTables(t::ts: Temp.temp list, tempMap, nodeMap) =
                (case Temp.Table.look(tempMap, t)
                  of SOME(n) => fillTables(ts, tempMap, nodeMap)
                   | NONE =>
                     let
                       val newNode = UGraph.newNode(interGraph)
                     in
                       fillTables(ts,
                                  Temp.Table.enter(tempMap, t, newNode),
                                  UGraph.Table.enter(nodeMap, newNode, t))
                     end)

            (* All of the temps referenced by the given node *)
            val temps = (getDef node)@(getUse node)

            (* Fill the given tables with the list of all of the temps
             * referenced by the given node *)
            val (tnode', gtemp') = fillTables(temps, tnode, gtemp)
          in
            createNodeTempMaps(nodes, tnode', gtemp')
          end

      val initTnode =
          (foldr (fn (t, tab) =>
                     Temp.Table.enter(tab, t, UGraph.newNode(interGraph)))
                 Temp.Table.empty
                 Frame.physicalRegsT)
      val initGtemp = foldr (fn ((t, n), tab) => UGraph.Table.enter(tab, n, t))
                            UGraph.Table.empty (Temp.Table.listItemsi(initTnode))

      (* Create the maps between temps and nodes *)
      val (tnodeMap, gtempMap) =
          createNodeTempMaps(fgNodes, initTnode, initGtemp)

      (* tnodeFun : Temp.temp -> Graph.node
       *
       * The function version of the tnodeMap to actually be used in the igraph.
       * Returns the graph node associated with the given temp.
       * All valid temps should have a node associated with them according to
       * the initialization algorithm above. If an invalid temp is given, throw
       * an exception.
       *)
      fun tnodeFun(temp:Temp.temp) =
          (case Temp.Table.look(tnodeMap, temp)
            of SOME(n) => n
             | NONE => let exception TempNotFound
                       in raise TempNotFound end)

      (* gtempFun : Graph.node -> Temp.temp
       *
       * The function version of the gtempMap to actually be used in the igraph.
       * Returns the temp associated with the given node.
       * All valid nodes should have a temp associated with them according to
       * the initialization algorithm above. If an invalid node is given, throw
       * an exception.
       *)
      fun gtempFun(node) = UGraph.lookUpNode(gtempMap, node)

      (* interferePhysicalRegs: Temp.temp list -> unit
       *
       * Adds edges between all given physical registers in the interference
       * graph
       *)
      fun interferePhysicalRegs([]) = ()
        | interferePhysicalRegs(r::regs) =
          ((app (fn t => UGraph.mkEdge interGraph (tnodeFun(r), tnodeFun(t)))
                regs);
           interferePhysicalRegs(regs))

      val _ = interferePhysicalRegs(Frame.physicalRegsT)

      (* computeMoves : Graph.node list -> (UGraph.node * UGraph.node) list
       *
       * Returns a list of the move instructions associated with the given list
       * of nodes in the form of a (fromNode, toNode) pair.
       *)
      fun computeMoves([]) = []
        | computeMoves(n::ns) =
          case Graph.Table.look(ismove, n)
           of SOME(true) =>
              let
                (* Moves should have exactly one dst and one src since this is
                 * how they are constrained in Assem *)
                val d = hd(getDef(n))
                val s = hd(getUse(n))
              in
                (tnodeFun(d), tnodeFun(s))::computeMoves(ns)
              end
            | _ => computeMoves(ns)

      (* A liveMap mapping nodes to their live in sets.
       * Initializes an entry for every node in the graph to be the empty set
       *)
      val liveIn: liveMap =
          foldr (fn (n, tab) =>
                     Graph.Table.enter(tab, n, (Temp.Table.empty, [])))
                Graph.Table.empty fgNodes

      (* A liveMap mapping nodes to their live out sets
       * Initializes an entry for every node in the graph to be the empty set
       *)
      val liveOut: liveMap =
          foldr (fn (n, tab) =>
                    Graph.Table.enter(tab, n, (Temp.Table.empty, [])))
                Graph.Table.empty fgNodes

      (* printLiveMaps : liveMap * liveMap -> unit
       *
       * For debugging purposes only.
       * Print the live in and live out sets for each node
       *)
      fun printLiveMaps(inMap, outMap) =
          let
            (* getTempList : Graph.node * livemap -> Temp.temp list
             *
             * Get the temp list for the given node from the given livemap.
             *)
            fun getTempList(n, map) =
                let val (t, l) = Graph.lookUpNode(map, n)
                in l end

            (* printNodeLives : Graph.node list -> unit
             *
             * Print:
             * - the live in set,
             * - the node name,
             * - the associated assembly instruction,
             * - and the live out set
             * for each of the nodes in the given list
             *)
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
             printNodeLives(fgNodes)
          end

      (* The algorithm will operate more efficiently if we reverse the list of
       * nodes/instructions *)
      val initialWL = rev(Graph.nodes(control))

      (* livenessWL : Graph.node list * liveMap * liveMap -> liveMap * liveMap
       *
       * Implements the liveness worklist algorithm presented in class.
       * The first argument is the worklist, represented as a list of nodes.
       * New live-in and live-out sets are created for the first item in the
       * worklist as specified by the set operation algorithm in the book.
       *)
      fun livenessWL([], liveIn, liveOut) = (liveIn, liveOut)
        | livenessWL(b::wl, liveIn, liveOut) =
          let
            (* Add all elements in the use and def lists for the given node
             * to tables so that we can use the more effecient set operations
             * on tables rather than lists.
             *)
            val useTable = foldr (fn (elem, tab) =>
                                     Temp.Table.enter(tab, elem, ()))
                                 Temp.Table.empty (getUse(b))
            val defTable = foldr (fn (elem, tab) =>
                                     Temp.Table.enter(tab, elem, ()))
                                 Temp.Table.empty (getDef(b))

            (* getLiveTable : liveMap * Graph.node -> Temp.Table.table
             *
             * Get the table representation of the liveSet associated with the
             * given node from the given livemap
             *)
            fun getLiveTable(lmap:liveMap, node:Graph.node) =
                let val (tab, _) = Graph.lookUpNode(lmap, node)
                in tab end

            (* getLiveList : liveMap * Graph.node -> Temp.temp list
             *
             * Get the list representation of the liveSet associated with the
             * given node from the given livemap
             *)
            fun getLiveList(lmap:liveMap, node:Graph.node) =
                let val (_, tempList) = Graph.lookUpNode(lmap, node)
                in tempList end

            (* Extract the in and out sets that will be needed for the algorithm *)
            val bInTemps = getLiveList(liveIn, b)
            val bOutTable = getLiveTable(liveOut, b)
            val bOutTemps = getLiveList(liveOut, b)

            (* Dummy function to be passed to union. Since our values in the
             * liveset tables are always unit, this function is trivial *)
            fun dummyF((), ()) = ()

            (* Extract all of the live in tables from the successors of b *)
            val succInTables = map (fn succ => getLiveTable(liveIn, succ))
                                   (Graph.succ(b))

            (* out[n] <- U_succ in[s] *)
            val newOutTable = foldr (fn (t1, t2) =>
                                        Temp.Table.union dummyF (t1, t2))
                                    Temp.Table.empty (succInTables)

            (* in[n] <- use[n] U (out[n] - def[n]) *)
            val newInTable =
                Temp.Table.union dummyF (useTable,
                                         Temp.Table.difference(newOutTable,
                                                               defTable))

            (* Get the new list of temps corresponding to the new in table *)
            val newInTemps = map (fn (k, elem) => k)
                                 (Temp.Table.listItemsi(newInTable))
            (* Get the new list of temps corresponding to the new out table *)
            val newOutTemps = map (fn (k, elem) => k)
                                 (Temp.Table.listItemsi(newOutTable))

            (* Enter the new livesets for b in the livemaps *)
            val newLiveIn =
                Graph.Table.enter(liveIn,
                                  b, (newInTable, newInTemps))
            val newLiveOut =
                Graph.Table.enter(liveOut,
                                  b, (newOutTable, newOutTemps))

            (* An invariant of this algorithm is that the live in/live out sets
             * never shrink during an iteration (everything that was there at
             * the start of the iteration should also be there at the end. Thus
             * we can tell if the in/out sets changed by simply comparing the
             * sizes of the lists *)
            val oldInSize = List.length(bInTemps)
            val newInSize = List.length(newInTemps)

            val newWL = wl@
                        (* If the live in set for b changed, then we need to add
                         * the predecessors of b to the worklist *)
                        (if newInSize > oldInSize
                         then Graph.pred(b)
                         else [])
          in
              livenessWL(newWL, newLiveIn, newLiveOut)
          end

      (* Calculate the final live sets *)
      val (finalLiveIn, finalLiveOut) = livenessWL(initialWL, liveIn, liveOut)

      (* liveOutFun : Graph.node -> Temp.temp list
       *
       * The function version of the final liveOut map to be return with the igraph.
       * Returns the live out set associated with the given node in list form.
       * All valid nodes in the control flow graph should have a live out set
       * associated with them now. If an invalid node is given, throw
       * an exception.
       *)
      fun liveOutFun(node) =
        let val (tab, lst) = Graph.lookUpNode(finalLiveOut, node)
        in lst end

      (* interfere : Graph.node list -> unit
       *
       * Iterate through fg nodes, adding interference edges where appropriate
       *)
      fun interfere([]) = ()
        | interfere(n::ns) =
          let
            val allDsts = getDef(n)
            (* extract the live out set for the given node in list form *)
            val outTemps =
                let val (tab, lst) = Graph.lookUpNode(finalLiveOut, n)
                in lst end

            (* look up whether the given node corresponds to a move instruction *)
            val nIsMove = Graph.lookUpNode(ismove, n)

            (* goThroughDsts : Temp.temp list -> Temp.temp list
             *
             * Delete any temps corresponding to moves in the given temp list
             *)
            fun goThroughDsts([]) = ()
              | goThroughDsts(d::ds) =
                let
                  (* deleteFromList: Temp.temp * Temp.temp list -> Temp.temp list*)
                  fun deleteFromList(item:Temp.temp, list) =
                      List.filter(fn x => x <> item) list

                  (* Delete the current dst temp from the out list if present
                   * allDsts are here because dsts interfere with each other.
                   *)
                  val outsWithoutSelf = deleteFromList(d, outTemps@allDsts)

                  val effectiveOuts =
                      if nIsMove
                      (* Delete the source of the copy instruction from
                       * the out list if it was a move. Since it was a move
                       * instruction, we know that it should have exactly one
                       * thing in its use set, so delete it *)
                      then
                        let
                          val s = tnodeFun(hd(getUse(n)))
                          val d = tnodeFun(hd(getDef(n)))
                        in (deleteFromList(hd(getUse(n)), outsWithoutSelf))
                        end
                      else outsWithoutSelf

                in
                  (* Add edges between the nodes associated with the dst temps
                   * and the nodes in the live out set *)
                  ((app (fn out => UGraph.mkEdge interGraph
                                                 (tnodeFun d, tnodeFun out))
                        effectiveOuts);
                   goThroughDsts(ds))
                end
          in
            (goThroughDsts(getDef(n));
             interfere(ns))
          end
    in
      (interfere(Graph.nodes(control));
       (IGRAPH{graph=interGraph,
               tnode=tnodeFun,
               gtemp=gtempFun,
               moves=computeMoves(fgNodes)}))
    end

(* show: outstream * igraph -> unit
 * For debugging purposes: prints out a list of nodes in the interference
 * graph and a list of all nodes adjacent to it
 *)
fun show(out, IGRAPH{graph, tnode, gtemp, moves}) =
    app (fn (node) => (TextIO.output(out, (Frame.tempToString Frame.tempMap
                                                             (gtemp(node)))^":\n");
                        (app (fn (adj) =>
                                 TextIO.output(out,
                                               (Frame.tempToString Frame.tempMap
                                                                  (gtemp(adj)))^" "))
                             (UGraph.adjList graph  node));
                        print("\n----\n")))
         (UGraph.nodeList(graph))

end
