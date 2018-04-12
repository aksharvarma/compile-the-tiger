signature COLOR =
sig

  val color: {interference: Liveness.igraph,
              spillCost: UGraph.node -> int,
              adjTab: UGraph.S.set UGraph.Table.table,
              registers: Frame.register list}
             -> WL.allocation
end

structure Color: COLOR =
struct

fun color{interference=(Liveness.IGRAPH{graph, tnode, gtemp, moves}),
          adjTab, spillCost, registers} =
    (while not(WL.stackNull()) do
           let
             val n = WL.popStack()
             (* val _ = print((Frame.tempToString Frame.tempMap (gtemp n))^": "); *)
             val adjN = UGraph.lookUpNode(adjTab, n)
           in
             (WL.initOkColors();
              (UGraph.S.app
                 (fn w => 
                     if WL.N.member(WL.N.union(WL.getNodeSet(WL.COLORED),
                                               WL.getNodeSet(WL.PRECOLORED)),
                                    WL.getAlias(w))
                     then WL.removeColors(WL.C.singleton(WL.getColor(gtemp(WL.getAlias(w)))))
                     else ())
                 adjN);
              if not(WL.hasFreeColor())
              then (print("need-spilling.\n");WL.addNode(WL.SPILLED, n))
              else
               (let
                  val chosenColor = WL.getAvailableColor()
               in
                  ((* print("coloring with "^chosenColor^"\n"); *)
                    WL.setColor(gtemp(n), chosenColor);
                    WL.addNode(WL.COLORED, n))
               end))
           end;
     (WL.N.app
        (fn m => WL.setColor(gtemp(m), WL.getColor(gtemp(WL.getAlias(m)))))
        (WL.getNodeSet(WL.COALESCED_N)));
     WL.updateSafeColors();
     (* print("~~~~Table printing~~~~\n"); *)
     (* (app (fn (t, s) => print((Frame.tempToString Frame.tempMap t)^": "^s^"\n")) *)
     (*      (Temp.Table.listItemsi(WL.getAllocation()))); *)
     (* print("~~~~Table done~~~~\n"); *)
     (WL.getAllocation()))


end
