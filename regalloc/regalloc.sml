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
      val (cfg, fgNodes) = MakeGraph.instrs2graph(instrs)

      val _ = WL.initialize()

      (* Also builds the interference graph *)
      val (igraph as Liveness.IGRAPH{graph, tnode, gtemp, moves}) =
          Liveness.computeLivenessAndBuild(cfg)
      (* val _ = Liveness.show(TextIO.stdOut, igraph) *)

      fun copyAdjList(Liveness.IGRAPH{graph, tnode, gtemp, moves}) =
          (foldr (fn ((n, adjSet), tab) =>
                     UGraph.Table.enter(tab, n, adjSet))
                 UGraph.Table.empty
                 (map (fn n => (n, UGraph.adjSet graph (n)))
                      (UGraph.nodeList(graph))))

      val adjTab = copyAdjList(igraph)
                              
      fun nodeMoves(n) =
          let
            val movesTab = (case UGraph.Table.look(moves, n)
                             of SOME(t) => t
                              | NONE => WL.E.empty)
          in
            WL.E.intersection(movesTab, WL.E.union(WL.getMoveSet(WL.ACTIVE),
                                                   WL.getMoveSet(WL.MOVES)))
          end
            
      fun moveRelated(n) = WL.E.isEmpty(nodeMoves(n))
                                       
      fun makeWLs() =
          let
            val nodes = UGraph.nodeSet(graph)
          in
            UGraph.S.app
              (fn n => 
                  let
                    val t = gtemp(n)
                  in
                    if List.exists (fn t' => t=t') Frame.physicalRegsT
                    then (WL.addNode(WL.PRECOLORED, n))
                    else if (UGraph.degree graph (n)) >= Frame.K
                    then WL.addNode(WL.TOSPILL, n)
                                   (* else if moveRelated(n) *)
                                   (* then WL.addNode(WL.FREEZE, n) *)
                    else WL.addNode(WL.SIMPLIFY, n)
                  end)
              nodes
          end

      val _ = print("uncomment-for-freeze (regalloc(makeWL))\n")
      val _ = makeWLs()
      val _ = WL.precolor(gtemp)
                         
      fun adjacent(n) =
          WL.N.difference(UGraph.adjSet graph n,
                          WL.N.union(WL.getStackSet(),
                                     WL.getNodeSet(WL.COALESCED_N)))


      fun enableMoves(nodes) =
          WL.N.app
            (fn n =>
                WL.E.app
                  (fn m =>
                      if WL.isEin(WL.ACTIVE, m)
                      then (WL.removeMove(WL.ACTIVE, m);
                            WL.addMove(WL.MOVES, m))
                      else ())
                  (nodeMoves(n)))
            nodes
            
            
      fun processNeighbours(n, []) = ()
        | processNeighbours(n, m::ms) =
          (UGraph.rmEdge graph (n,m);
           if UGraph.degree graph (m) = Frame.K-1
           then (
             (* enableMoves(WL.N.union(WL.N.singleton m, adjacent(m))); *)
             (* WL.removeNode(WL.TOSPILL, m); *)
             (* if moveRelated(m) *)
             (* then WL.addNode(WL.FREEZE, m) *)
             (* else *)
             WL.addNode(WL.SIMPLIFY, m))
           else ();
           processNeighbours(n, ms)
          )
            
      fun simplify() =
          while WL.isNotNullN(WL.SIMPLIFY) do
                let
                  val n = WL.getAndRemoveNode(WL.SIMPLIFY)
                in
                  (WL.pushStack(n);
                   processNeighbours(n, UGraph.adjList graph (n)))
                end
                
      fun coalesce() = ()
      fun freeze() = ()

      fun freezeMoves(m:UGraph.node) = ()

      fun selectSpill() =
          let
            val m = WL.getAndRemoveNode(WL.TOSPILL)
          in
            (WL.addNode(WL.SIMPLIFY, m);
             (* TODO: Implement freezeMoves *)
             freezeMoves(m))
          end
            
      fun rewriteProgram() =
          let
            val accessTab =
                (WL.N.foldr
                   (fn (t, tab) =>
                       Temp.Table.enter(tab, t,
                                        Frame.getOffset(Frame.allocLocal frame
                                                                         true)))
                   Temp.Table.empty
                   (WL.N.map gtemp (WL.getNodeSet(WL.SPILLED))))

            val _ = print("~~~~\nSpilling temp, offset\n")
            val _ = (app (fn (t, nt) =>
                             print("("^Temp.makeString(t)^", "
                                   ^Int.toString(nt)^") "))
                         (Temp.Table.listItemsi(accessTab)))
            val _ = print("\n~~~~\n")
                  
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
                      (newTempList, tempMap, replaced)
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
                   * newDsts should be used from now. (AFTER REVERSING)
                   * dstTab has the mapping from spilled to new temps
                   * Does it even spill?
                   *)
                  val (newDst, dstTab, dstSpills) =
                      replaceTemp(dst, [], Temp.Table.empty, false)

                  (* replaceTemp on srcs
                   * newSrcs should be used from now. (AFTER REVERSING)
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
                      [Assem.OPER{assem="FP--| la 'd0 "^
                                        Symbol.name(Frame.name(frame))
                                        ^"_framesize\n",
                                  src=[], dst=[fpTemp], jump=NONE},
                       Assem.OPER{assem="FP--| lw 'd0 0('s0)\n",
                                  src=[fpTemp], dst=[fpTemp], jump=NONE},
                       Assem.OPER{assem="FP--| add 'd0, 's0, 's1\n",
                                  src=[Frame.SP, fpTemp], dst=[fpTemp],
                                  jump=NONE}]
                      end
                        
                  (* findVarOffset: Temp.temp -> string
                   * Find the offset from FP, given a spilling temp *)
                  fun findVarOffset(t) =
                      case Temp.Table.look(accessTab, t)
                       of SOME(offset) => Int.toString(offset)
                        | NONE => let exception accessNotFound
                                  in raise accessNotFound end

                  (* Add instrs to handle spilling temps in srcs *)
                  val prevInstr =
                      if srcSpills
                      then
                        let val fpTemp = Temp.newTemp() in
                          putFPintoTemp(fpTemp)
                          @(map (fn (t, newT) =>
                                    Assem.OPER({assem="USE-| lw 'd0 "^
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
                                    Assem.OPER({assem="DEF-| sw 's0 "^
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
                      then [Assem.OPER{assem="op--| "^assem, jump=jump,
                                       dst=rev(newDst), src=rev(newSrc)}]
                      else [Assem.MOVE{assem="mo--| "^assem,
                                       dst=hd(rev(newDst)), src=hd(rev(newSrc))}]
                in
                  (* Replaced list of instructions 
                   * This needs to be reversed, because when the overall rewrite
                   * is reversed, we want this whole chunk to be considered as 
                   * one instr, conceptually. Hence, we pre-reverse it to balance
                   * the parity of reverses.
                   *)
                  rev(prevInstr@currInstr@nextInstr)
                end

            fun rewriteAll([], acc) = acc
              | rewriteAll(instr::instrs, acc) =
                rewriteAll(instrs, (rewriteInstr(instr))@acc)

          in
            rev(rewriteAll(instrs, []))
          end

    in
      while not(WL.isNullN(WL.SIMPLIFY) andalso
                WL.isNullN(WL.TOSPILL)) do
            (* while not(WL.isNullN(WL.SIMPLIFY) andalso *)
            (*           WL.isNullN(WL.FREEZE) andalso *)
            (*           WL.isNullN(WL.TOSPILL) andalso *)
            (*           WL.isNullE(WL.MOVES)) do *)
            (if WL.isNotNullN(WL.SIMPLIFY)
             then (simplify();print(""))
             else if WL.isNotNullE(WL.MOVES)
             then (print("coalesce\n");coalesce())
             else if WL.isNotNullN(WL.FREEZE)
             then (print("freeze\n");freeze())
             else if WL.isNotNullN(WL.TOSPILL)
             then (selectSpill();print(""))
             else ());
      let
        (* val _ = print("starting color\n") *)
        val colorAlloc =
            (Color.color{interference=igraph,
                         adjTab=adjTab,
                         spillCost=
                         (fn n:UGraph.node => ~(UGraph.degree graph (n))),
                         registers=Frame.registers})
              (* val _ = print("ending color\n") *)
              
      in
        if WL.isNotNullN(WL.SPILLED)
        then
          let
            (* val _ = (print("Spillers\n"); *)
            (*          (WL.N.app (fn n => print(Temp.makeString(gtemp n)^" ")) *)
            (*                    (WL.getNodeSet(WL.SPILLED))); *)
            (*          print("\n****\n")) *)
            val newInstrs = rewriteProgram()

            (* val _ = print(Int.toString(Frame.K)^"\n") *)
            (* val _ = print((Frame.tempToString Frame.tempMap 101)^"\n") *)
            (* val _ = print((Frame.tempToString Frame.tempMap 102)^"\n") *)
            (* val _ = print((Frame.tempToString Frame.tempMap 108)^"\n") *)
            (* val _ = print((Frame.tempToString Frame.tempMap 109)^"\n") *)
            (* val _ = print((Frame.tempToString Frame.tempMap 117)^"\n") *)
            (* val _ = print((Frame.tempToString Frame.tempMap 126)^"\n") *)

            (* val _ = print("Old graph: "^ *)
            (*               Int.toString(List.length(UGraph.nodeList graph)) *)
            (*               ^"\n") *)
            (* val _ = (app (fn (n, adjN) => *)
            (*                  print(Temp.makeString(gtemp n)^": "^ *)
            (*                        Int.toString(UGraph.S.numItems(adjN)) *)
            (*                        ^"\n")) *)
            (*              (UGraph.Table.listItemsi(adjTab))) *)


            val (newcfg, newfgNodes) = MakeGraph.instrs2graph(newInstrs)
            val Liveness.IGRAPH{graph=newGraph,
                                tnode=newtnode, gtemp=newgtemp, moves=newmoves} =
                Liveness.computeLivenessAndBuild(newcfg)
            (* val _ = print("New graph: "^ *)
            (*               Int.toString(List.length(UGraph.nodeList newGraph)) *)
            (*               ^"\n") *)
            (* val _ = (app (fn n => print(Temp.makeString(newgtemp n)^": "^ *)
            (*                             Int.toString(UGraph.degree newGraph n)^"\n")) *)
            (*              (UGraph.nodeList(newGraph))) *)

                      
            (* val _ = *)
            (*     (print("****old****\n"); *)
            (*      (app (fn i => print(Assem.format(Frame.tempToString Frame.tempMap) i)) *)
            (*          instrs)) *)
            (* val _ = *)
            (*     (print("****new****\n"); *)
            (*      (app (fn i => print(Assem.format(Frame.tempToString Frame.tempMap) i)) *)
            (*          newInstrs)) *)

          in
            (* (instrs, Temp.Table.empty) *)
            alloc(newInstrs, frame)
          end
        else (instrs, colorAlloc)
      end
    end

end
