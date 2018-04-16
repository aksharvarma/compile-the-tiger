(* Contains logic for coloring an interference graph, assuming register
 * allocation has correctly filled out the stack.
 * Supports spilling if a node cannot be colored.
 *)
signature COLOR =
sig
  (* Colors the given interference graph and returns a valid allocation if
   * possible. If not, colors as much as possible and adds uncolored nodes to
   * the SPILLED worklist.
   *)
  val color: {interference: Liveness.igraph,
              adjTab: UGraph.S.set UGraph.Table.table}
             -> WL.allocation
end

structure Color: COLOR =
struct
(* Colors the given interference graph and returns a valid allocation if
 * possible. If not, colors as much as possible and adds uncolored nodes to
 * the SPILLED worklist.
 * Note: assumes that the stack has been filled out by register allocation.
 *
 * interference :  the interference graph to color.
 *                 Note: some of the edges may have been removed during
 *                       register allocation. Use adjTab instead for
 *                       edge information.
 * adjTab :        a copy of the original adjacency sets of the interference graph.
 *                 Maps a UGraph.node to a set of adjacent UGraph.nodes
 *)
fun color{interference=(Liveness.IGRAPH{graph, tnode, gtemp, moves}), adjTab} =
    (while not(WL.stackNull()) do
        let
          val n = WL.popStack()
        in
          (* Initialize okColors with all of the potential colors *)
          (WL.initOkColors();
          (* For all nodes w that interfere with n,
           * If the alias of w is in either the already colored set, or the
           * precolored set, then remove that node's color from okColors *)
           (UGraph.S.app
              (fn w =>
                  if WL.N.member(WL.N.union(WL.getNodeSet(WL.COLORED),
                                            WL.getNodeSet(WL.PRECOLORED)),
                                 WL.getAlias(w))
                  then
                    WL.removeColors(WL.C.singleton(WL.getColor(gtemp(WL.getAlias w))))
                  else ())
              (UGraph.lookUpNode(adjTab, n)));
           (* If there are no available colors left, then add n to the SPILLED
            * worklist *)
           if not(WL.hasFreeColor())
           then WL.addNode(WL.SPILLED, n)
           (* If there is a free color left, color n with an available color and
            * add n to the COLORED worklist *)
           else (WL.setColor(gtemp(n), WL.getAvailableColor());
                 WL.addNode(WL.COLORED, n)))
        end;
     (* For all nodes m that have been coalesced,
      * set the color of m to the color of its alias *)
     (WL.N.app
        (fn m => WL.setColor(gtemp(m), WL.getColor(gtemp(WL.getAlias(m)))))
        (WL.getNodeSet(WL.COALESCED_N)));
     (* Return the current allocation after coloring *)
     WL.getAllocation())
end
