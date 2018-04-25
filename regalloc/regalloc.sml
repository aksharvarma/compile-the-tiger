signature REG_ALLOC =
sig
  val alloc: Assem.instr list * Frame.frame -> Assem.instr list * WL.allocation
end

structure RegAlloc: REG_ALLOC =
struct

(* alloc: Assem.instr list * Frame.frame -> Assem.instr list * allocation
 *
 * Takes an instruction list and frame and performs register allocation
 *)
fun alloc(instrs, frame) =
    let
      (* Generate the control flow graph *)
      val (cfg, fgNodes) = MakeGraph.instrs2graph(instrs)

      (* Initialize worklists to empty as interference graph creation adds
       * things to some of the worklists. *)
      val _ = WL.initialize()
      (* Compute liveness and build the interference graph *)
      val (igraph as Liveness.IGRAPH{graph, tnode, gtemp, moves}) =
          Liveness.computeLivenessAndBuild(cfg)

      (* copyAdjList: Ugraph.graph -> UGraph.Table.table
       *
       * Keep a copy of the adjacency list for coloring.
       *)
      fun copyAdjList(graph) =
          (foldr (fn ((n, adjSet), tab) =>
                     UGraph.Table.enter(tab, n, adjSet))
                 UGraph.Table.empty
                 (map (fn n => (n, UGraph.adjSet graph (n)))
                      (UGraph.nodeList(graph))))

      (* The adjacency lists for each node
       * (used during allocating stack slots in Part 4) *)
      val originalAdjTab = copyAdjList(graph)

      (* A copy which will include changes due to coalescing
       * (used for main coloring) *)
      val adjTab = ref originalAdjTab

      (* copyMoves: UGraph.Table.table -> UGraph.Table.table
       *
       * Make a copy of the moves so that the original moves doesn't change
       * original is used for allocating stack slots in Part 4
       *)
      fun copyMoves(moves) =
          (foldr (fn ((n, set), tab) =>
                     UGraph.Table.enter(tab, n, set))
                 UGraph.Table.empty
                 (UGraph.Table.listItemsi(moves)))

      (* The copy of moves used instead of the original *)
      val movesCopy = ref (copyMoves(moves))
      (* val movesCopy = ref moves *)

      (* nodeMoves: UGraph.node -> WL.E.set
       *
       * Returns the intersection of move edges, and the union of the
       * ACTIVE and MOVES move worklists.
       *)
      fun nodeMoves(n) =
          let
            val movesTab = (case UGraph.Table.look(!movesCopy, n)
                             of SOME(t) => t
                              | NONE => WL.E.empty)
          in
            WL.E.intersection(movesTab, WL.E.union(WL.getMoveSet(WL.ACTIVE),
                                                   WL.getMoveSet(WL.MOVES)))
          end

      (* moveRelated: UGraph.node -> bool
       *
       * Returns true if a node is move related, false otherwise
       *)
      fun moveRelated(n) = not(WL.E.isEmpty(nodeMoves(n)))

      (* effectiveDegree: UGraph.graph -> UGraph.node -> int
       *
       * A wrapper around the degree function from the UGraph module,
       * to return an impossibly high value as the degree of precolored nodes.
       *)
      fun effectiveDegree graph =
          (fn n =>
              if WL.isNin(WL.PRECOLORED, n)
              then Frame.K*List.length(UGraph.nodeList graph)
              else UGraph.degree graph n)

      (* makeWLs: unit -> unit
       *
       * Initialize worklists. (MakeWorklist() in the book)
       *)
      fun makeWLs() =
          UGraph.S.app
            (fn n =>
                let
                  val t = gtemp(n)
                in
                  if List.exists (fn t' => t=t') Frame.physicalRegsT
                  then (WL.addNode(WL.PRECOLORED, n))
                  else if (effectiveDegree graph (n)) >= Frame.K
                  then WL.addNode(WL.TOSPILL, n)
                  else if moveRelated(n)
                  then WL.addNode(WL.FREEZE, n)
                  else WL.addNode(WL.SIMPLIFY, n)
                end)
            (UGraph.nodeSet(graph))

      (* Initialize all worklists correctly *)
      val _ = makeWLs()

      (* Color all precolored nodes with appropriate registers *)
      val _ = WL.precolor(gtemp)

      (* adjacent: UGraph.node -> WL.N.set
       *
       * Returns the adjacency list of the original untouched graph
       * This bridges differences between our Graph module invariants
       * and the invariants that Appel's algorithm assumes.
       * Esp., with the PRECOLORED's adjList being empty, and adjList always
       * staying constant.
       *)
      fun adjacent(n) =
          if WL.isNin(WL.PRECOLORED, n)
          then WL.N.empty
          else WL.N.difference(UGraph.lookUpNode(!adjTab, n),
                               WL.N.union(WL.getStackSet(),
                                          WL.getNodeSet(WL.COALESCED_N)))

      (* enableMoves: UGraphs.node list -> unit
       *
       * Change worklists of nodeMoves(n) (for every n \in nodes)
       * so that they are now enabled for coalescing.
       *)
      fun enableMoves(nodes) =
          WL.N.app (fn n => WL.E.app
                              (fn m =>
                                  if WL.isEin(WL.ACTIVE, m)
                                  then (WL.removeMove(WL.ACTIVE, m);
                                        WL.addMove(WL.MOVES, m))
                                  else ())
                              (nodeMoves(n))) nodes


      (* processNeighbours: UGraph.node * UGraph.node list -> unit
       *
       * Does what decrementDegree(m) did in the book, but does it for
       * all m at once.
       * This can be done because decrementDegree is always called with
       * the adjacent(n) in the algorithm.
       *)
      fun processNeighbours(n, []) = ()
        | processNeighbours(n, m::ms) =
          (UGraph.rmEdge graph (n,m);
           if effectiveDegree graph (m) = Frame.K-1
           then (
             enableMoves(WL.N.union(WL.N.singleton m, adjacent(m)));
             WL.removeNode(WL.TOSPILL, m);
             if moveRelated(m)
             then WL.addNode(WL.FREEZE, m)
             else WL.addNode(WL.SIMPLIFY, m))
           else ();
           processNeighbours(n, ms))

      (* simplify: unit -> unit
       *
       * Does the simplify phase (Simplify() from the book)
       *)
      fun simplify() =
          while WL.isNotNullN(WL.SIMPLIFY) do
                let
                  val n = WL.getAndRemoveNode(WL.SIMPLIFY)
                in
                  (WL.pushStack(n);
                   processNeighbours(n, WL.N.listItems(adjacent n)))
                end

      (* coalesce: unit -> unit
       *
       * Does the coalescing phase (Coalesce() from the book)
       *)
      fun coalesce() =
          let

            (* addWorkList: UGraph.node -> unit
             *
             * Moves node from Freeze to Simplify if certain conditions met.
             *)
            fun addWorkList(u) =
                if (not(WL.isNin(WL.PRECOLORED, u))
                    andalso not(moveRelated(u))
                    andalso (effectiveDegree graph u) < Frame.K)
                then ((case WL.whichNWL(u)
                        of SOME(WL.FREEZE) => ()
                         | _ => (print("WL before removal attempt: ");
                                 WL.printNWL(u);
                                 (* Catches a bug *)
                                 let exception notInFreezeWhenRemoving
                                 in raise notInFreezeWhenRemoving end));
                      WL.removeNode(WL.FREEZE, u);
                      WL.addNode(WL.SIMPLIFY, u))
                else ()

            (* checkNewSpill: UGraph.node -> unit
             *
             * Checks if a node should be moved to TOSPILL
             * This maintains the invariant that the algorithm assumes
             *)
            fun checkNewSpill(t) =
                (* If degree condition satisfied *)
                if (effectiveDegree graph t) >= Frame.K then
                  let
                    val wlt = (case WL.whichNWL(t)
                                of SOME(nwl) => nwl
                                 | NONE => let exception shouldNotBeInStack
                                           in raise shouldNotBeInStack end)
                  in
                    if wlt <> WL.PRECOLORED (* Don't move if precolored *)
                    then (WL.removeNode(wlt, t);
                          WL.addNode(WL.TOSPILL, t))
                    else ()
                  end
                else ()

            (* combine: UGraph.node * UGraph.node -> unit
             *
             * Does the actual coalescing of v into u.
             *)
            fun combine(u, v) =
                (if WL.isNin(WL.FREEZE, v)
                 then WL.removeNode(WL.FREEZE, v)
                 else WL.removeNode(WL.TOSPILL, v);
                 WL.addNode(WL.COALESCED_N, v);
                 WL.setAlias(v, u);
                 movesCopy :=
                 UGraph.Table.enter(!movesCopy, u,
                                    WL.E.union(UGraph.lookUpNode(!movesCopy, u),
                                               UGraph.lookUpNode(!movesCopy, v)));
                 enableMoves(WL.N.singleton(v));
                 (WL.N.app
                    (fn t =>
                        let
                          val tAdjSet = UGraph.lookUpNode(!adjTab, t)
                          val uAdjSet = UGraph.lookUpNode(!adjTab, u)
                        in
                          (UGraph.mkEdge graph (t, u);
                           adjTab :=
                           UGraph.Table.enter(UGraph.Table.enter(!adjTab, t,
                                                                 UGraph.S.add(
                                                                   tAdjSet, u)),
                                              u, UGraph.S.add(uAdjSet, t));
                           checkNewSpill(t); checkNewSpill(u))
                        end
                    ) (adjacent v));
                 processNeighbours(v, WL.N.listItems(adjacent v));
                 if (effectiveDegree graph u) >= Frame.K
                    andalso WL.isNin(WL.FREEZE, u)
                 then (WL.removeNode(WL.FREEZE, u);
                       WL.addNode(WL.TOSPILL, u))
                 else ())

            (* OK: UGraph.node * UGraph.node -> bool
             *
             * Does part of the test needed for the George heuristic
             *)
            fun OK(t, r) =
                (effectiveDegree graph t) < Frame.K
                orelse WL.isNin(WL.PRECOLORED, t)
                orelse UGraph.S.member(UGraph.lookUpNode(!adjTab, r), t)

            (* coalesceGeorge: UGraph.node * UGraph.node -> bool
             *
             * The George Heuristic.
             *)
            fun coalesceGeorge(u, v) =
                not((WL.N.exists (fn t => not(OK(t, u))) (adjacent(v))))

            (* conservative: UGraph.node * UGraph.node -> bool
             *
             * Does part of the test needed for the Briggs heuristic.
             * Counts the number of elements in the nodes set which have degree
             * greater than K. If there are < K such elements, return true
             *)
            fun conservative(nodes) =
                WL.N.numItems(WL.N.filter
                                (fn n => (effectiveDegree graph n) >= Frame.K)
                                nodes) < Frame.K

            (* coalesceGeorge: UGraph.node * UGraph.node -> bool
             *
             * The Briggs Heuristic.
             *)
            fun coalesceBriggs(u, v) =
                conservative(WL.N.union(adjacent(u), adjacent(v)))

          in
            (* The loop from the book *)
            while WL.isNotNullE(WL.MOVES) do
                  let
                    val m = WL.getAndRemoveMove(WL.MOVES)
                    val (x', y') = WL.getMoveContents(m)
                    val x = WL.getAlias(x')
                    and y =  WL.getAlias(y')
                    val (u, v) = if WL.isNin(WL.PRECOLORED, y)
                                 then (y, x) else (x, y)
                  in
                    if u=v
                    then (WL.addMove(WL.COALESCED_E, m);
                          addWorkList(u))
                    else if WL.isNin(WL.PRECOLORED, v) orelse
                            UGraph.S.member(UGraph.lookUpNode(!adjTab, u), v)
                    then (WL.addMove(WL.CONSTRAINED, m);
                          addWorkList(u);
                          addWorkList(v))
                    else if (WL.isNin(WL.PRECOLORED, u)
                             andalso coalesceGeorge(u, v)) orelse
                            (not(WL.isNin(WL.PRECOLORED, u))
                             andalso coalesceBriggs(u, v))
                    then (WL.addMove(WL.COALESCED_E, m);
                          combine(u, v);
                          addWorkList(u))
                    else (WL.addMove(WL.ACTIVE, m))
                  end
          end


      (* freezeMoves: UGraph.node -> unit
       *
       * Moves nodes to the frozen worklist, if allocation cannot happen
       * otherwise.
       *)
      fun freezeMoves(u) =
          (WL.E.app (fn m =>
                        let
                          val (x, y) = WL.getMoveContents(m)
                          val v = if WL.getAlias(y)=WL.getAlias(u)
                                  then WL.getAlias(x)
                                  else WL.getAlias(y)
                        in
                          WL.removeMove(WL.ACTIVE, m);
                          WL.addMove(WL.FROZEN, m);
                          if WL.E.isEmpty(nodeMoves(v))
                             andalso (effectiveDegree graph v) < Frame.K
                          then
                            (WL.removeNode(WL.FREEZE, v);
                             WL.addNode(WL.SIMPLIFY, v))
                          else ()
                        end
                    ) (nodeMoves(u)))

      (* freeze: unit -> unit
       *
       * Does the freezing phase (Freeze() from the book)
       *)
      fun freeze() = let val u = WL.getAndRemoveNode(WL.FREEZE)
                     in (WL.addNode(WL.SIMPLIFY, u); freezeMoves(u)) end


      (* selectSpill: unit -> unit
       *
       * Does maybe spill phase (SelectSpill() from the book)
       *)
      fun selectSpill() =
          let
            (* Choose a node of high degree to maybe spill *)
            val m = WL.N.foldr (fn (n, max) =>
                                   if (effectiveDegree graph n) >
                                      (effectiveDegree graph max)
                                   then n else max
                               ) (WL.getNode(WL.TOSPILL))
                               (WL.getNodeSet(WL.TOSPILL))
          in
            (WL.removeNode(WL.TOSPILL, m);
             WL.addNode(WL.SIMPLIFY, m);
             freezeMoves(m))
          end

      (* allocateStackSlots: unit -> Temp.Table.table
       *
       * Allocates stack frame slots to temps that cannot need to be spilled
       * Uses coloring to do this smarter than "give every temp a new slot".
       *)
      fun allocateStackSlots() =
          let
            (* Getting the correct interference graph:
             * Start with all the nodes,
             * remove edges containing non-spilling nodes
             * add edges containing spilling nodes
             *
             * These are necessary to get the correct interference graph
             * and remove any effects of the simplify/coalesce phases earlier
             *)
            val _ =
                UGraph.S.app
                  (fn n => if not(WL.isNin(WL.SPILLED, n))
                           then (UGraph.S.app
                                   (fn m => UGraph.rmEdge graph (n,m)
                                   ) (UGraph.adjSet graph n))
                           else (UGraph.S.app
                                   (fn m => if WL.isNin(WL.SPILLED, m)
                                            then UGraph.mkEdge graph (n,m)
                                            else ())
                                   (* Use originalAdjTab here to make sure we do
                                    * not retain any effects of the previous
                                    * simplifyy/coalescing phases *)
                                   (UGraph.lookUpNode(originalAdjTab, n)))
                  ) (UGraph.nodeSet graph)

            (* Just like in main coloring, retain a copy of the adjTab for the
             * coloring at the end *)
            val spillAdjTab = ref (copyAdjList(graph))

            (* Drop move table entries unless it's for a SPILLED node *)
            val moves' =
                foldr (fn ((n, eSet), set) =>
                          if not(WL.isNin(WL.SPILLED, n))
                          then set
                          else WL.E.union(set, eSet))
                      WL.E.empty
                      (UGraph.Table.listItemsi(moves))

            (* Now remove any moves between spilled and unspilled nodes;
             * they are not eligible moves anymore.
             *)
            val newMoves =
                ref (WL.E.foldr
                       (fn (nSet, newESet) =>
                           if (WL.N.exists (fn n =>
                                               not(WL.isNin(WL.SPILLED, n)))
                                           nSet)
                           then newESet else WL.E.add(newESet, nSet))
                       WL.E.empty
                       moves')

            (* Table keeping track of the aliases of nodes (for coalescing)
             * and getter/setter for the table.
             *)
            val aliasTab =
                ref (WL.N.foldr (fn (n, tab) => UGraph.Table.enter(tab, n, n))
                                UGraph.Table.empty (WL.getNodeSet(WL.SPILLED)))
            (* getAlias: UGraph.node -> UGraph.node *)
            fun getAlias(n) = case UGraph.Table.look(!aliasTab, n)
                               of SOME(m) => if n=m
                                             then n else getAlias(m)
                                | NONE => let exception unInitAlias
                                          in raise unInitAlias end
            (* setAlias: UGraph.node * UGraph.node -> unit *)
            fun setAlias(n, aliasN) =
                aliasTab := UGraph.Table.enter(!aliasTab, n, aliasN)

            (* The coalescedSet, containing nodes that have been coalesced *)
            val coalescedSet = ref WL.N.empty

            (* combine: UGraph.node * UGraph.node -> unit
             *
             * Does the actual coalescing of v into u.
             * Much leaner than main coloring.
             *)
            fun combine(u, v) =
                (setAlias(v, u);
                 coalescedSet := WL.N.add(!coalescedSet, v);
                 newMoves :=
                 (WL.E.foldr (fn (nSet, newESet) =>
                                 WL.E.add(newESet,
                                          (WL.N.map
                                             (fn x => if x=v then u else x)
                                             nSet))
                             ) WL.E.empty (!newMoves));
                 (WL.N.app
                    (fn t =>
                        let
                          val tAdjSet = UGraph.lookUpNode(!spillAdjTab, t)
                          val uAdjSet = UGraph.lookUpNode(!spillAdjTab, u)
                        in
                          (UGraph.mkEdge graph (t, u);
                           UGraph.Table.enter(
                             UGraph.Table.enter(!spillAdjTab, t,
                                                UGraph.S.add(tAdjSet, u)),
                             u, UGraph.S.add(uAdjSet, t));
                           UGraph.rmEdge graph (t, v))
                        end
                    ) (UGraph.adjSet graph v)))

            (* Simplify worklist *)
            val simplify = ref WL.N.empty
            (* The order of the "stack" *)
            val colorOrder = ref []
            (* doSimplify: unit -> unit (simplify phase) *)
            fun doSimplify() =
                let
                  val n =       (* Pick min-degree node to simplify *)
                      (WL.N.foldr (fn (n, minDeg) =>
                                      if (UGraph.degree graph n) <
                                         (UGraph.degree graph minDeg)
                                      then n else minDeg
                                  ) (WL.chooseN(!simplify)) (!simplify))
                in
                  simplify := WL.N.delete(!simplify, n);
                  colorOrder := n::(!colorOrder);
                  UGraph.S.app (fn t => UGraph.rmEdge graph (n, t))
                               (UGraph.adjSet graph n)
                end

            (* The chosen accesses (stack slots) *)
            val accessTab = ref Temp.Table.empty

          in
            (* The coalescing loop *)
            while not(WL.E.isEmpty(!newMoves)) do
                  let
                    val m = case WL.E.find (fn _ => true) (!newMoves)
                             of SOME(m') => m'
                              | NONE => let exception WontHappen
                                        in raise WontHappen end
                    val _ = (newMoves := WL.E.delete(!newMoves, m))
                    val (x', y') = WL.getMoveContents(m)
                    val u = getAlias(x')
                    and v = getAlias(y')
                  in
                    if u=v
                    then coalescedSet := WL.N.add(!coalescedSet, v)
                    else if not(UGraph.S.member(UGraph.adjSet graph u, v))
                    then combine(u, v) else ()
                  end;
            (* Get set of nodes to be simplified  *)
            simplify := WL.N.filter (fn n => if WL.N.exists (fn m => m=n)
                                                            (!coalescedSet)
                                             then false else true)
                                    (WL.getNodeSet(WL.SPILLED));

            (* Simplify phase *)
            while not(WL.N.isEmpty(!simplify)) do doSimplify();


            (* Color simplified nodes, color coalesced nodes, return coloring *)
            (WL.N.foldr (fn (n, tab) =>
                            let
                              val aliasN = getAlias(n)
                              val aliasColor =
                                  case Temp.Table.look(tab, gtemp aliasN)
                                   of SOME(i) => i
                                    | NONE => let exception uncoloredAlias
                                              in raise uncoloredAlias end
                            in
                              Temp.Table.enter(tab,
                                               gtemp n, aliasColor)
                            end
                        ) (Color.colorStack{frame=frame,
                                            colorOrder=(!colorOrder),
                                            gtemp=gtemp,
                                            getAlias=getAlias,
                                            spillAdjTab=(!spillAdjTab)})
                        (!coalescedSet))
          end

      (* rewriteProgram: unit -> Assem.instr list
       *
       * Modify the code to add load/store code for spilling nodes.
       *)
      fun rewriteProgram() =
          let
            val accessTab = allocateStackSlots()

            (* rewriteInstr: Assem.intsr -> Assem.intsr list
             *
             * Do the actual rewriting of one instruction
             *)
            fun rewriteInstr(instr as Assem.LABEL{assem, lab}) = [instr]
              | rewriteInstr(instr) =
                let
                  (* Figure out which kind of instr you have *)
                  val (assem, dst, src, jump, isOper) =
                      case instr
                       of Assem.OPER{assem, src, dst, jump} =>
                          (assem, dst, src, jump, true)
                        | Assem.MOVE{assem, src, dst} =>
                          (assem, [dst], [src], NONE, false)
                        | Assem.LABEL{assem, lab} =>
                          let exception labelNotCaught
                          in raise labelNotCaught end

                  (* replaceTemp: Temp.temp list * Temp.temp list *
                   *              Temp.Table.table * bool ->
                   *              Temp.temp list * Temp.Table.table * bool
                   *
                   * Go through either dsts or srcs and replace spillers
                   * with new temps, and keep track of the changes
                   *)
                  fun replaceTemp([], newTempList, tempMap, replaced) =
                      (rev(newTempList), tempMap, replaced)
                    | replaceTemp(temp::tempList,
                                  newTempList, tempMap, replaced) =
                      if WL.isNin(WL.SPILLED, tnode temp)
                      then
                        let val newT = Temp.newTemp() in
                          replaceTemp(tempList, newT::newTempList,
                                      Temp.Table.enter(tempMap, temp, newT), true)
                        end
                      else replaceTemp(tempList, temp::newTempList,
                                       tempMap, replaced)

                  (* replaceTemp on dsts
                   * newDsts should be used from now.
                   * dstTab has the mapping from spilled to new temps
                   * dstSpills: Does it even spill?
                   *)
                  val (newDst, dstTab, dstSpills) =
                      replaceTemp(dst, [], Temp.Table.empty, false)

                  (* replaceTemp on srcs
                   * newSrcs should be used from now.
                   * srcTab has the mapping from spilled to new temps
                   * srcSpills: Does it even spill?
                   *)
                  val (newSrc, srcTab, srcSpills) =
                      replaceTemp(src, [], Temp.Table.empty, false)


                  (* putFPintoTemp: Temp.temp -> Assem.instr
                   *
                   * Returns an instr that puts "FP" into input temp
                   * so that we can access things in the frame.
                   *)
                  fun putFPintoTemp(fpTemp) =
                        Assem.OPER{assem="addi 'd0, 's0, "^
                                          Symbol.name(Frame.name(frame))
                                          ^"_framesize\n",
                                   dst=[fpTemp], src=[Frame.SP], jump=NONE}

                  (* findVarOffset: Temp.temp -> string
                   *
                   * Find the offset from FP, given a spilling temp *)
                  fun findVarOffset(t) =
                      case Temp.Table.look(accessTab, t)
                       of SOME(offset) => Assem.ourIntToString(offset)
                        | NONE => ((* print(Temp.makeString(t)^"\n"); *)
                          let exception accessNotFound
                          in raise accessNotFound end)

                  (* Add instrs to handle spilling temps in srcs *)
                  val prevInstr =
                      if srcSpills
                      then
                        let val fpTemp = Temp.newTemp() in
                          putFPintoTemp(fpTemp)
                          ::(map (fn (t, newT) =>
                                     Assem.OPER({assem="lw 'd0, "^
                                                       findVarOffset(t)
                                                       ^"('s0)\n",
                                                 src=[fpTemp], jump=NONE,
                                                 dst=[newT]}))
                                 (Temp.Table.listItemsi(srcTab)))
                        end
                      else []

                  (* Add instrs to handle spilling temps in dsts *)
                  val nextInstr =
                      if dstSpills
                      then
                        let val fpTemp = Temp.newTemp() in
                          putFPintoTemp(fpTemp)
                          ::(map (fn (t, newT) =>
                                     Assem.OPER({assem="sw 's0, "^
                                                       findVarOffset(t)^"('s1)\n",
                                                 src=[newT, fpTemp], jump=NONE,
                                                 dst=[]}))
                                 (Temp.Table.listItemsi(dstTab)))
                        end
                      else []

                  (* Current instruction now uses the newSrc and newDst *)
                  val currInstr =
                      if isOper
                      then [Assem.OPER{assem=assem, jump=jump,
                                       dst=newDst, src=newSrc}]
                      else [Assem.MOVE{assem=assem,
                                       dst=hd(newDst), src=hd(newSrc)}]
                in
                  (* Replaced list of instructions
                   * This needs to be reversed, because when the overall rewrite
                   * is reversed, we want this whole chunk to be considered as
                   * one instr, conceptually. Hence, we pre-reverse it to balance
                   * the parity of reverses.
                   *)
                  rev(prevInstr@currInstr@nextInstr)
                end

            (* rewriteAll:Assem.instr list*Assem.instr list->Assem.instr list
             * Go through all instructions and rewrite each.
             *)
            fun rewriteAll([], acc) = rev(acc)
              | rewriteAll(instr::instrs, acc) =
                rewriteAll(instrs, (rewriteInstr(instr))@acc)

          in
            rewriteAll(instrs, [])
          end

      (* removeRedundantMoves: WL.allocation * bool * Assem.instr list *
       *                       Assem.instr list
       *
       * Remove moves that have the same temp as its two arguments from the list
       * If comment is true, only comment that instruction out.
       *)
      fun removeRedundantMoves(colorMap:WL.allocation, comment,
                               instrs, newInstrs) =
          let
            (* actual: Temp.temp -> Temp.temp
             * Find the actual chosen register for given temp.
             *)
            fun actual(t) = case Temp.Table.look(colorMap, t)
                             of SOME(t') => t'
                              | NONE => let exception uncoloredTemp
                                        in raise uncoloredTemp end

            (* dropMove: Assem.instr list * Assem.instr list -> Assem.instr list
             * Actual removal happens here.
             *)
            fun dropMove([], newInstrs) = rev(newInstrs)
              | dropMove((instr as Assem.MOVE{assem, src, dst})::instrs,
                         newInstrs) =
                if actual(src)=actual(dst)
                then if comment
                     then dropMove(instrs,
                                   Assem.MOVE{assem="# "^assem, src=src, dst=dst}::
                                   newInstrs)
                     else dropMove(instrs, newInstrs)
                else dropMove(instrs, instr::newInstrs)
              | dropMove(instr::instrs, newInstrs) =
                dropMove(instrs, instr::newInstrs)
          in
            dropMove(instrs, newInstrs)
          end
    in
      (* Main loop which does the coloring. *)
      while not(WL.isNullN(WL.SIMPLIFY) andalso
                WL.isNullN(WL.FREEZE) andalso
                WL.isNullN(WL.TOSPILL) andalso
                WL.isNullE(WL.MOVES)) do
            (if WL.isNotNullN(WL.SIMPLIFY)
             then (simplify())
             else if WL.isNotNullE(WL.MOVES)
             then (coalesce())
             else if WL.isNotNullN(WL.FREEZE)
             then (freeze())
             else if WL.isNotNullN(WL.TOSPILL)
             then (selectSpill())
             else ());
      let
        (* Color the temps *)
        val colorAlloc = Color.colorRegs{interference=igraph, adjTab=(!adjTab)}
      in
        (* Recur after spilling if couldn't fully color *)
        if WL.isNotNullN(WL.SPILLED)
        then (alloc(rewriteProgram(), frame))
        else
          (* Coloring worked, return the result after removing redundant moves
           * Set 2nd argument to true to keep redundant moves (commented out)
           * false will remove them from the list of instrs.
           * eventually the second argument to this can be removed.
           *)
          (removeRedundantMoves(colorAlloc, false, instrs, []), colorAlloc)
      end
    end

end
