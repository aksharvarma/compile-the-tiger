signature WL =
sig
  datatype wlType = SIMPLIFY | FREEZE | SPILL | MOVES

  val reset: unit -> unit
  val initialize: unit -> unit
  (* val initializeWL: wlType -> unit *)
  val makeWLs: Liveness.igraph -> unit
  val isNull: wlType -> bool
  val isNotNull: wlType -> bool
end

structure WL:WL =
struct
  datatype wlType = SIMPLIFY | FREEZE | SPILL | MOVES

  (* Set of nodes *)
  structure N = BinarySetFn(type ord_key = UGraph.node
                            fun compare(a,b) = UGraph.compare(a,b))
                                                  
  val precolored: N.set ref = ref N.empty
  val initial: N.set ref = ref N.empty
  val simplifyWL: N.set ref = ref N.empty
  val freezeWL: N.set ref = ref N.empty
  val spillWL: N.set ref = ref N.empty
  val spilledNodes: N.set ref = ref N.empty
  val coalescedNodes: N.set ref = ref N.empty
  val coloredNodes: N.set ref = ref N.empty
  val selectStack: UGraph.node list ref = ref []

  (* Set of edges *)
  structure E = BinarySetFn(type ord_key = UGraph.node * UGraph.node
                            fun compare(e1, e2) = UGraph.pairCompare(e1, e2))
                                              
                           
  val coalescedMoves: E.set ref = ref E.empty
  val constrainedMoves: E.set ref = ref E.empty
  val frozenMoves: E.set ref = ref E.empty
  val movesWL: E.set ref = ref E.empty
  val activeMoves: E.set ref = ref E.empty

  val alias: UGraph.node UGraph.Table.table ref = ref UGraph.Table.empty
  val color: string UGraph.Table.table ref = ref UGraph.Table.empty
                                  
  fun reset() = ()
  fun initialize() = (reset();())
  fun initializeWL(wl) = ()
  fun makeWLs(igraph) = ()
  fun isNull(wl) = true
  fun isNotNull(wl) = not(isNull(wl))
end
