signature COLOR =
sig

  val color: {interference: Liveness.igraph,
              spillCost: UGraph.node -> int,
              registers: Frame.register list}
               -> WL.allocation * Temp.temp list
end

structure Color: COLOR =
struct

  (* fun color{interference, initial, spillCost, registers} = (Temp.Table.empty, []) *)
fun color{interference=(Liveness.IGRAPH{graph, tnode, gtemp, moves}),
          spillCost, registers} =
      (while not(WL.stackNull()) do
             let
               val n = WL.popStack()
               val adjN = UGraph.adjSet graph (n)
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
                else (WL.setColor(gtemp(n), WL.getAvailableColor());
                      WL.addNode(WL.COLORED, n)))
             end;
       (WL.N.app
          (fn m => WL.setColor(gtemp(m), WL.getColor(gtemp(WL.getAlias(m)))))
          (WL.getNodeSet(WL.COALESCED_N)));
       WL.updateSafeColors();
       (WL.getAllocation(), []))


end
