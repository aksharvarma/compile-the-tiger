signature WL =
sig
  datatype nodeWL = PRECOLORED | INITIAL | SIMPLIFY | FREEZE | TOSPILL | SPILLED
                    | COALESCED_N | COLORED

  datatype moveWL = COALESCED_E | CONSTRAINED | FROZEN | MOVES | ACTIVE

  structure N : ORD_SET
  structure E : ORD_SET                                                      

  val reset: unit -> unit
  val initialize: unit -> unit
  (* val initializeWL: wlType -> unit *)
  (* val makeWLs: Liveness.igraph -> unit *)
  val isNullN: nodeWL -> bool
  val isNotNullN: nodeWL -> bool
  val isNullE: moveWL -> bool
  val isNotNullE: moveWL -> bool

  val addNode: nodeWL * UGraph.node -> unit
  val addMove: moveWL * (UGraph.node * UGraph.node) -> unit

  val removeNode: nodeWL * UGraph.node -> unit
  val removeMove: moveWL * (UGraph.node * UGraph.node) -> unit

  val chooseN: N.set -> N.item
  val chooseE: E.set -> E.item

  val getNode: nodeWL -> UGraph.node
  val getMove: moveWL -> UGraph.node * UGraph.node

  val isNin: nodeWL * UGraph.node -> bool
  val isEin: moveWL * (UGraph.node * UGraph.node) -> bool
                                       
  val getNodeSet: nodeWL -> N.set
  val getMoveSet: moveWL -> E.set


  val pushStack: UGraph.node -> unit
  val popStack: unit -> UGraph.node
  val stackNull: unit -> bool                         
  val getStackSet: unit -> N.set
                           (* TODO: DO the rest *)
end

structure WL:WL =
struct
datatype nodeWL = PRECOLORED | INITIAL | SIMPLIFY | FREEZE | TOSPILL | SPILLED
                  | COALESCED_N | COLORED

datatype moveWL = COALESCED_E | CONSTRAINED | FROZEN | MOVES | ACTIVE
                                                                 
(* Set of nodes *)
structure N = UGraph.S
(* BinarySetFn(type ord_key = UGraph.node *)
(*         fun compare(a,b) = UGraph.compare(a,b)) *)
                
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

fun getNRef(PRECOLORED) = (precolored)
  | getNRef(INITIAL) = (initial)
  | getNRef(SIMPLIFY) = (simplifyWL)
  | getNRef(FREEZE) = (freezeWL)
  | getNRef(TOSPILL) = (spillWL)
  | getNRef(SPILLED) = (spilledNodes)
  | getNRef(COALESCED_N) = (coalescedNodes)
  | getNRef(COLORED) = (coloredNodes)

fun getNWL(wl) = !(getNRef(wl))

fun getERef(COALESCED_E) = (coalescedMoves)
  | getERef(CONSTRAINED) = (constrainedMoves)
  | getERef(FROZEN) = (frozenMoves)
  | getERef(MOVES) = (movesWL)
  | getERef(ACTIVE) = (activeMoves)

fun getEWL(wl) = !(getERef(wl))
                  
fun reset() = ()
fun initialize() = (reset();())
fun initializeWL(wl) = ()
(* fun makeWLs(igraph) = () *)

fun isNullN(wl) = N.isEmpty(getNWL(wl))
fun isNullE(wl) = E.isEmpty(getEWL(wl))
fun isNotNullN(wl) = not(isNullN(wl))
fun isNotNullE(wl) = not(isNullE(wl))
                        

fun addNode(wl, item) = getNRef(wl) := N.add(getNWL(wl), item)

fun addMove(wl, item) = getERef(wl) := E.add(getEWL(wl), item)

fun removeNode(wl, item) = getNRef(wl) := N.delete(getNWL(wl), item)

fun removeMove(wl, item) = getERef(wl) := E.delete(getEWL(wl), item)

fun chooseN(wl) =
    case N.find (fn _ => true) (wl)
     of SOME(n) => n
      | NONE => let exception emptyNodeWL
                in raise emptyNodeWL end

fun chooseE(wl) =
    case E.find (fn _ => true) (wl)
     of SOME(n) => n
      | NONE => let exception emptyMoveWL
                in raise emptyMoveWL end
                  
fun getNode(wl) =
    let
      val n = chooseN(getNWL(wl))
    in
      removeNode(wl, n);
      n
    end

fun getMove(wl) =
    let
      val m = chooseE(getEWL(wl))
    in
      removeMove(wl, m);
      m
    end
      
fun isNin(wl, n) = N.member(getNWL(wl), n)
fun isEin(wl, e) = E.member(getEWL(wl), e)


      
fun getNodeSet(wl) = getNWL(wl)

fun getMoveSet(wl) = getEWL(wl)
    

fun pushStack(n) = selectStack := n::(!selectStack)
fun popStack() =
    let
      val n = hd(!selectStack)
    in
      selectStack := tl(!selectStack);
      n
    end 
fun stackNull() = List.null(!selectStack)

fun getStackSet() = N.addList(N.empty, !selectStack)
                             
end
