signature REG_ALLOC =
sig
  (* type allocation = Frame.register Temp.Table.table *)
  val alloc: Assem.instr list *Frame.frame -> Assem.instr list * WL.allocation
end

structure RegAlloc: REG_ALLOC =
struct

(* type allocation = Frame.register Temp.Table.table *)


(* alloc: Assem.instr list *Frame.frame -> Assem.instr list * allocation
 *
 *)
fun alloc(instrs, frame) =
    let
      (* Flow.flowgraph * Graph.node list *)
     (* val _ = print("Available Registers:
      * "^Assem.ourIntToString(Frame.K)^"\n"); *)
      val (cfg, fgNodes) = MakeGraph.instrs2graph(instrs)
      val _ = WL.initialize()

      (* Also builds the interference graph *)
      val (igraph as Liveness.IGRAPH{graph, tnode, gtemp, moves}) =
          Liveness.computeLivenessAndBuild(cfg)

       (*val _ = Liveness.show(TextIO.stdOut, igraph) *)

      fun copyAdjList(graph) =
          (foldr (fn ((n, adjSet), tab) =>
                     UGraph.Table.enter(tab, n, adjSet))
                 UGraph.Table.empty
                 (map (fn n => (n, UGraph.adjSet graph (n)))
                      (UGraph.nodeList(graph))))

      val originalAdjTab = copyAdjList(graph)
      val adjTab = ref originalAdjTab

      fun nodeMoves(n) =
          let
            val movesTab = (case UGraph.Table.look(moves, n)
                             of SOME(t) => t
                              | NONE => WL.E.empty)
          in
            WL.E.intersection(movesTab, WL.E.union(WL.getMoveSet(WL.ACTIVE),
                                                   WL.getMoveSet(WL.MOVES)))
          end

      fun moveRelated(n) = not(WL.E.isEmpty(nodeMoves(n)))

      fun ourDegree graph =
          (fn n =>
              if WL.isNin(WL.PRECOLORED, n)
              then Frame.K*List.length(UGraph.nodeList graph)
              else UGraph.degree graph n)

      fun makeWLs() =
          UGraph.S.app
            (fn n =>
                let
                  val t = gtemp(n)
                in
                  if List.exists (fn t' => t=t') Frame.physicalRegsT
                  then (WL.addNode(WL.PRECOLORED, n))
                  else if (ourDegree graph (n)) >= Frame.K
                  then WL.addNode(WL.TOSPILL, n)
                  else if moveRelated(n)
                  then WL.addNode(WL.FREEZE, n)
                  else WL.addNode(WL.SIMPLIFY, n)
                end)
            (UGraph.nodeSet(graph))

      val _ = makeWLs()

      val _ = WL.precolor(gtemp)

      (* This bridges differences between our Graph module invariants
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

      fun enableMoves(nodes) =
          WL.N.app (fn n => WL.E.app
                              (fn m =>
                                  if WL.isEin(WL.ACTIVE, m)
                                  then (WL.removeMove(WL.ACTIVE, m);
                                        WL.addMove(WL.MOVES, m))
                                  else ())
                              (nodeMoves(n))) nodes


      fun processNeighbours(n, []) = ()
        | processNeighbours(n, m::ms) =
          (UGraph.rmEdge graph (n,m);
           if ourDegree graph (m) = Frame.K-1
           then (
             enableMoves(WL.N.union(WL.N.singleton m, adjacent(m)));
             WL.removeNode(WL.TOSPILL, m);
             if moveRelated(m)
             then WL.addNode(WL.FREEZE, m)
             else WL.addNode(WL.SIMPLIFY, m))
           else ();
           processNeighbours(n, ms))

      fun simplify() =
          while WL.isNotNullN(WL.SIMPLIFY) do
                let
                  val n = WL.getAndRemoveNode(WL.SIMPLIFY)
                in
                  (WL.pushStack(n);
                   processNeighbours(n, WL.N.listItems(adjacent n)))
                end

      fun coalesce() =
          let
            fun OK(t, r) =
                (ourDegree graph t) < Frame.K
                orelse WL.isNin(WL.PRECOLORED, t)
                orelse UGraph.S.member(UGraph.lookUpNode(!adjTab, r), t)

            (* Count the number of elements in the nodes set which have degree
             * greater than K. If there are < K such elements, return true
             *)
            fun conservative(nodes) =
                WL.N.numItems(WL.N.filter
                                (fn n => (ourDegree graph n) >= Frame.K)
                                nodes) < Frame.K

            fun addWorkList(u) =
                if (not(WL.isNin(WL.PRECOLORED, u))
                    andalso not(moveRelated(u))
                    andalso (ourDegree graph u) < Frame.K)
                then ((case WL.whichNWL(u)
                       of SOME(WL.FREEZE) => ()
                        | _ => (print("WL before removal attempt: ");
                                WL.printNWL(u);
                                let exception notInFreezeWhenRemoving
                                in raise notInFreezeWhenRemoving end));
                      WL.removeNode(WL.FREEZE, u);
                      WL.addNode(WL.SIMPLIFY, u))
                else ()

            fun checkNewSpill(t) =
                if (ourDegree graph t) >= Frame.K then
                  let
                    exception shouldNotBeInStack
                    val wlt = (case WL.whichNWL(t)
                                of SOME(nwl) => nwl
                                 | NONE => raise shouldNotBeInStack)
                  in
                    if wlt <> WL.PRECOLORED
                    then (WL.removeNode(wlt, t);
                          WL.addNode(WL.TOSPILL, t))
                    else ()
                  end
                else ()

            fun combine(u, v) =
                (if WL.isNin(WL.FREEZE, v)
                 then WL.removeNode(WL.FREEZE, v)
                 else WL.removeNode(WL.TOSPILL, v);
                 WL.addNode(WL.COALESCED_N, v);
                 WL.setAlias(v, u);
                 UGraph.Table.enter(moves, u,
                                    WL.E.union(UGraph.lookUpNode(moves, u),
                                               UGraph.lookUpNode(moves, v)));
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
                                                               UGraph.S.add(tAdjSet, u)),
                                                u, UGraph.S.add(uAdjSet, t));

                                checkNewSpill(t); checkNewSpill(u))
                     end)
                     (adjacent v));
                 processNeighbours(v, WL.N.listItems(adjacent v));
                 if (ourDegree graph u) >= Frame.K
                    andalso WL.isNin(WL.FREEZE, u)
                 then (WL.removeNode(WL.FREEZE, u);
                       WL.addNode(WL.TOSPILL, u))
                 else ())

            fun coalesceGeorge(u, v) =
                not((WL.N.exists (fn t => not(OK(t, u))) (adjacent(v))))

            fun coalesceBriggs(u, v) =
                conservative(WL.N.union(adjacent(u), adjacent(v)))

          in
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
                    else if WL.isNin(WL.PRECOLORED, v)
                            orelse UGraph.S.member(UGraph.lookUpNode(!adjTab, u), v)
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


      fun freezeMoves(u:UGraph.node) =
          (WL.E.app (fn m =>
                        (let
                          val (x, y) = WL.getMoveContents(m)
                          val v = if WL.getAlias(y)=WL.getAlias(u)
                                  then WL.getAlias(x)
                                  else WL.getAlias(y)
                        in
                          WL.removeMove(WL.ACTIVE, m);
                          WL.addMove(WL.FROZEN, m);
                          if WL.E.isEmpty(nodeMoves(v))
                             andalso (ourDegree graph v) < Frame.K
                          then
                            (WL.removeNode(WL.FREEZE, v);
                             WL.addNode(WL.SIMPLIFY, v))
                          else ()
                        end))
                    (nodeMoves(u)))

      fun freeze() = let val u = WL.getAndRemoveNode(WL.FREEZE)
                     in (WL.addNode(WL.SIMPLIFY, u); freezeMoves(u)) end


      fun selectSpill() =
          let
            val m = WL.N.foldr (fn (n, max) =>
                                if (ourDegree graph n) > (ourDegree graph max)
                                then n else max)
                               (WL.getNode(WL.TOSPILL))
                               (WL.getNodeSet(WL.TOSPILL))
          in
            (WL.removeNode(WL.TOSPILL, m);
             WL.addNode(WL.SIMPLIFY, m);
             freezeMoves(m))
          end

      fun colorSpilled() =
          let

           (* val _ = print("spill-to-spill-edges\n")
            val _ =
                WL.N.app (fn n =>
                             UGraph.S.app
                               (fn m =>
                                   if WL.isNin(WL.SPILLED, m)
                                   then (print(Temp.makeString(gtemp n)^", "^
                                               Temp.makeString(gtemp m)^"\n"))
                                   else ()
                               ) (UGraph.lookUpNode(adjTab, n))
                         ) (WL.getNodeSet(WL.SPILLED)) *)
            (* Remove edges containing non-spilling nodes
             * and add edges containing spilling nodes
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
                                   * simply/coalescing phases *)
                                   (UGraph.lookUpNode(originalAdjTab, n)))
                  ) (UGraph.nodeSet graph)

            (* val _ = print("\nremaining-edges-in-the-new-graph\n"); *)
            (* val _ =
                (UGraph.S.app
                   (fn n =>
                       if not(UGraph.S.isEmpty((UGraph.adjSet graph n)))
                       then
                         (print(Temp.makeString(gtemp n)^": ");
                          WL.printNWL(n);
                          (UGraph.S.app (fn n =>
                                            print(Temp.makeString(gtemp n)^" "))
                                        (UGraph.adjSet graph n));
                          print("\n"))
                       else ())
                   (UGraph.nodeSet graph)) *)
            val spillAdjTab = ref (copyAdjList(graph))

            (* Drop move table entries unless it's for a SPILLED node *)
            val moves' =
                foldr (fn ((n, eSet), set) =>
                          if not(WL.isNin(WL.SPILLED, n))
                          then set
                          else WL.E.union(set, eSet))
                      WL.E.empty
                      (UGraph.Table.listItemsi(moves))

            (* Now remove any moves containing unspilled node on either side,
             * from remaining move edges; they are not eligible moves anymore.
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

            (* Not using the NONE mechanism to keep track of coalesced
             * vs. simplify anymore.
             *)
            val aliasTab =
                ref (WL.N.foldr (fn (n, tab) => UGraph.Table.enter(tab, n, n))
                                UGraph.Table.empty (WL.getNodeSet(WL.SPILLED)))
            fun getAlias(n) = case UGraph.Table.look(!aliasTab, n)
                               of SOME(m) => if n=m
                                             then n else getAlias(m)
                                | NONE => let exception unInitAlias
                                          in raise unInitAlias end
            fun setAlias(n, aliasN) =
                aliasTab := UGraph.Table.enter(!aliasTab, n, aliasN)

            (* Using an explicit coalescedSet now. Easier to debug. *)
            val coalescedSet = ref WL.N.empty
            fun combine(u, v) =
                (setAlias(v, u);
                 (* Add to the coalescedSet *)
                 coalescedSet := WL.N.add(!coalescedSet, v);
                 newMoves :=
                 (WL.E.foldr (fn (nSet, newESet) =>
                                 WL.E.add(newESet,
                                          (WL.N.map
                                             (fn x => if x=v then u else x)
                                             nSet)))
                        WL.E.empty (!newMoves));
                 (WL.N.app
                    (fn t =>
                      let
                        val tAdjSet = UGraph.lookUpNode(!spillAdjTab, t)
                        val uAdjSet = UGraph.lookUpNode(!spillAdjTab, u)
                      in
                        (UGraph.mkEdge graph (t, u);
                         UGraph.Table.enter(UGraph.Table.enter(!spillAdjTab, t,
                                                               UGraph.S.add(tAdjSet, u)),
                                            u, UGraph.S.add(uAdjSet, t));
                         UGraph.rmEdge graph (t, v))
                      end)
                     (UGraph.adjSet graph v)))

            val simplify = ref WL.N.empty
            val colorOrder = ref []
            fun doSimplify() =
                let
                  val n =       (* Pick min-degree node to simplify *)
                      (WL.N.foldr (fn (n, minDeg) =>
                                      if (UGraph.degree graph n) <
                                         (UGraph.degree graph minDeg)
                                      then n else minDeg)
                                  (WL.chooseN(!simplify)) (!simplify))
                in
                  simplify := WL.N.delete(!simplify, n);
                  colorOrder := n::(!colorOrder);
                  UGraph.S.app (fn t => UGraph.rmEdge graph (n, t))
                               (UGraph.adjSet graph n)
                end

            val accessTab = ref Temp.Table.empty

            fun color([], accessTab, graph) = accessTab
              | color(n::ns, accessTab, graph) =
                let
                  (* val _ = print("["^Temp.makeString(gtemp n)^"] ") *)
                  (* Table of used offsets, try to use these. *)
                  val okColors = (foldr (fn ((_, offset), set) =>
                                           WL.I.add(set, offset))
                                       WL.I.empty
                                       (Temp.Table.listItemsi accessTab))
                  (* val _ = print("okColors: ");
                  val _ = WL.I.app (fn i => print(Int.toString(i)^" "))
                                   okColors *)
(*
                  val _ = print(" available:\n")
                  val _ = print("Juadsfinaomids\n")
                  val _ =
                      UGraph.S.app
                        (fn m =>
                            (print(Temp.makeString(gtemp n)^", "^
                                   Temp.makeString(gtemp m)^"\n"))
                        ) (UGraph.lookUpNode(spillAdjTab, n))
                  val _ = print("edrtfiyhunjm\n")
 *)

                  (* Remove offsets of any neighbours *)
                  val availableColors =
                      (WL.N.foldr (fn (w, availableColors) =>
                                      ((*
                                      print("neighbour:"^Temp.makeString(w)^"<-");
                                        *)
                                      case Temp.Table.look(accessTab,
                                                           gtemp(getAlias(w)))
                                       of SOME(offset) =>
                                          ((* print(Int.toString(offset)^"\n");
                                              *)
                                           if WL.I.member(availableColors,
                                           offset)
                                           then
                                           WL.I.delete(availableColors, offset)
                                           else availableColors)
                                        | NONE => ((* print("\n"); *)
                                                   availableColors))
                                  ) okColors (UGraph.lookUpNode(!spillAdjTab, n)))
(*
                  val _ = WL.I.app (fn i => print(Int.toString(i)^" "))
                                   availableColors *)
                  (* Choose from availableColors, or ask for new stack slot *)
                  val chosenColor =
                      case (WL.I.find (fn _ => true) availableColors)
                       of SOME(offset) => offset
                        | NONE => Frame.getOffset(Frame.allocLocal
                                                    frame true)
                  (* val _ = print("\nchosen: "^Int.toString(chosenColor)^"\n")
                   * *)
                in
                  (* Recursively color the rest *)
                  color(ns, Temp.Table.enter(accessTab, gtemp n, chosenColor), graph)
                end
          in
            (* print("\nSPILLED\n");
            (WL.N.app (fn n => print(Temp.makeString(gtemp n)^" "))
                      (WL.getNodeSet(WL.SPILLED))); *)
            (* The coalescing loop *)
            while not(WL.E.isEmpty(!newMoves)) do
                  let
                    val m = case WL.E.find (fn _ => true) (!newMoves)
                             of SOME(m') => m'
                              | NONE => let exception WontHappen
                                        in raise WontHappen end
                    val _ = (newMoves := WL.E.delete(!newMoves, m))
                    val (x', y') = WL.getMoveContents(m)
                    (*
                    val _ = print("x':"^Temp.makeString(gtemp x')^", "^
                                  "y':"^Temp.makeString(gtemp y')^"\n"); *)
                    val u = getAlias(x')
                    and v = getAlias(y')
                      (*
                    val _ = print("u:"^Temp.makeString(gtemp u)^", "^
                                  "v:"^Temp.makeString(gtemp v)^"\n");
                       *)
                  in
                    if u=v
                    then ((* print("should you reach here?\n"); *)
                          coalescedSet := WL.N.add(!coalescedSet, v))
                    else if not(UGraph.S.member(UGraph.adjSet graph u, v))
                    then combine(u, v) else ()
                  end;
            (* Get set of nodes to be simplified  *)
            simplify := WL.N.filter (fn n => if WL.N.exists (fn m => m=n)
                                                            (!coalescedSet)
                                             then false else true)
                                    (WL.getNodeSet(WL.SPILLED));
          (*  print("\nSimplifyWL\n");
            (WL.N.app (fn n => print(Temp.makeString(gtemp n)^" "))
                      (!simplify));
            print("\nCoalescedWL\n");
            (WL.N.app
               (fn n => print(Temp.makeString(gtemp n)^"->"^
                              Temp.makeString(gtemp(getAlias n))^"\n"))
               (!coalescedSet)); *)

            while not(WL.N.isEmpty(!simplify)) do doSimplify();

            accessTab := color(!colorOrder, !accessTab, graph);

            (* print("Colored\n");
            (app
               (fn (t, offset) =>
                   print(Temp.makeString(t)^":"^Assem.ourIntToString(offset)^"\n"))
               (Temp.Table.listItemsi(!accessTab)));
            print("%%%%\n"); *)
            (WL.N.app (fn n =>
                          let
                            val aliasN = getAlias(n)
                           (* val _ = print(Temp.makeString(gtemp n)^": "^
                                          Temp.makeString(gtemp aliasN)^"\n")
                            val _ = if n = aliasN
                                    then let exception aliasIsSelf
                                         in raise aliasIsSelf end
                                    else () *)
                            val aliasColor =
                                case Temp.Table.look(!accessTab, gtemp aliasN)
                                 of SOME(i) => i
                                  | NONE => let exception uncoloredAlias
                                            in raise uncoloredAlias end
                          in
                            accessTab := Temp.Table.enter(!accessTab,
                                                          gtemp n, aliasColor)
                          end)
                      (!coalescedSet));
            (* print("****\n"); *)
            !accessTab
          end

      fun rewriteProgram() =
          let
            (* val accessTab = colorSpilled() *)
            val accessTab = (WL.N.foldr
            (fn (t, tab) =>
            Temp.Table.enter(tab, t,
            Frame.getOffset(Frame.allocLocal frame true)))
            Temp.Table.empty
            (WL.N.map gtemp (WL.getNodeSet(WL.SPILLED))))
            (* val _ = print("~~~~\nSpilling temp, offset\n") *)
            (* val _ = (app (fn (t, nt) =>
                             print("("^Temp.makeString(t)^", "
                                   ^Assem.ourIntToString(nt)^") "))
                         (Temp.Table.listItemsi(accessTab)))
            val _ = print("\n~~~~\n")
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

                  (* Go through either dsts or srcs and replace spillers
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
                   * Does it even spill?
                   *)
                  val (newDst, dstTab, dstSpills) =
                      replaceTemp(dst, [], Temp.Table.empty, false)

                  (* replaceTemp on srcs
                   * newSrcs should be used from now.
                   * srcTab has the mapping from spilled to new temps
                   * Does it even spill?
                   *)
                  val (newSrc, srcTab, srcSpills) =
                      replaceTemp(src, [], Temp.Table.empty, false)


                  (* Return list of instrs that put "FP" into a temp
                   * so that we can access things in the frame.
                   *)
                  fun putFPintoTemp(fpTemp) =
                      let
                        val t1 = Temp.newTemp()
                        and t2 = Temp.newTemp()
                      in
                        [Assem.OPER{assem="la 'd0 "^
                                          Symbol.name(Frame.name(frame))
                                          ^"_framesize\n",
                                    src=[], dst=[fpTemp], jump=NONE},
                         Assem.OPER{assem="lw 'd0 0('s0)\n",
                                    src=[fpTemp], dst=[fpTemp], jump=NONE},
                         Assem.OPER{assem="add 'd0, 's0, 's1\n",
                                    src=[Frame.SP, fpTemp], dst=[fpTemp],
                                    jump=NONE}]
                      end

                  (* findVarOffset: Temp.temp -> string
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
                          @(map (fn (t, newT) =>
                                    Assem.OPER({assem="lw 'd0 "^
                                                      findVarOffset(t)
                                                      ^"('s0)\n",
                                                src=[fpTemp], jump=NONE,
                                                dst=[newT]}))
                                (Temp.Table.listItemsi(srcTab)))
                        end
                      else [] (* [Assem.OPER({assem="EMPTY-FOO--\n", *)
                  (*                      src=[], jump=NONE, *)
                  (*                      dst=[]})] *)

                  (* Add instrs to handle spilling temps in dsts *)
                  val nextInstr =
                      if dstSpills
                      then
                        let val fpTemp = Temp.newTemp() in
                          putFPintoTemp(fpTemp)
                          @(map (fn (t, newT) =>
                                    Assem.OPER({assem="sw 's0 "^
                                                      findVarOffset(t)^"('s1)\n",
                                                src=[newT, fpTemp], jump=NONE,
                                                dst=[]}))
                                (Temp.Table.listItemsi(dstTab)))
                        end
                      else [] (* [Assem.OPER({assem="EMPTY-BAR--\n", *)
                  (*                      src=[], jump=NONE, *)
                  (*                      dst=[]})] *)

                  (* Remember to reverse newXXX temp lists *)
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

            fun rewriteAll([], acc) = rev(acc)
              | rewriteAll(instr::instrs, acc) =
                rewriteAll(instrs, (rewriteInstr(instr))@acc)

          in
            rewriteAll(instrs, [])
          end

      fun removeRedundantMoves(colorMap:WL.allocation, comment, instrs, newInstrs) =
          let
            fun actual(t) = case Temp.Table.look(colorMap, t)
                             of SOME(t') => t'
                              | NONE => let exception uncoloredTemp
                                        in raise uncoloredTemp end

            (* Actual removal happens here. *)
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
             (*
      if WL.isNotNullE(WL.FROZEN) then print("froze some move\n") else ();
              *)
      let
        val colorAlloc = Color.color{interference=igraph, adjTab=(!adjTab)}
      in
        if WL.isNotNullN(WL.SPILLED)
        then ((* print("rewriting\n"); *) alloc(rewriteProgram(), frame))
        else
          (* Set 2nd argument to true to keep redundant moves (commented out)
           * false will remove them from the list of instrs.
           * eventually the second argument to this can be removed.
           *)
          (removeRedundantMoves(colorAlloc, false, instrs, []), colorAlloc)
      end
    end

end
