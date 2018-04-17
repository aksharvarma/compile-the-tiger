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
  val colorRegs: {interference: Liveness.igraph,
                  adjTab: UGraph.S.set UGraph.Table.table}
                 -> WL.allocation

  val colorStack: {frame: Frame.frame,
                   colorOrder: UGraph.node list,
                   gtemp: Liveness.gtempFn,
                   getAlias: UGraph.node -> UGraph.node,
                   spillAdjTab: UGraph.S.set UGraph.Table.table}
                  -> WL.I.item Temp.Table.table
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
fun colorRegs{interference=(Liveness.IGRAPH{graph, tnode, gtemp, moves}),
              adjTab} =
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

fun colorStack{frame, colorOrder, gtemp, getAlias, spillAdjTab} =
    let
      fun color([], accessTab) = accessTab
        | color(n::ns, accessTab) =
          let
            (* Table of used offsets, try to use these. *)
            val okColors = (foldr (fn ((_, offset), set) =>
                                      WL.I.add(set, offset))
                                  WL.I.empty
                                  (Temp.Table.listItemsi accessTab))

            (* Remove offsets of any neighbours *)
            val availableColors =
                (WL.N.foldr (fn (w, availableColors) =>
                                case Temp.Table.look(accessTab,
                                                     gtemp(getAlias(w)))
                                 of SOME(offset) =>
                                    (if WL.I.member(availableColors,
                                                    offset)
                                     then
                                       WL.I.delete(availableColors, offset)
                                     else availableColors)
                                  | NONE => ((* print("\n"); *)
                                    availableColors)
                            ) okColors (UGraph.lookUpNode(spillAdjTab, n)))

            (* Choose from availableColors, or ask for new stack slot *)
            val chosenColor =
                case (WL.I.find (fn _ => true) availableColors)
                 of SOME(offset) => offset
                  | NONE => Frame.getOffset(Frame.allocLocal
                                              frame true)
          in
            (* Recursively color the rest *)
            color(ns, Temp.Table.enter(accessTab, gtemp n, chosenColor))
          end
    in
      color(colorOrder, Temp.Table.empty)
    end

end
