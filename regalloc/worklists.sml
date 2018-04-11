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
  val getAndRemoveNode: nodeWL -> UGraph.node
  val getAndRemoveMove: moveWL -> UGraph.node * UGraph.node

  val isNin: nodeWL * UGraph.node -> bool
  val isEin: moveWL * (UGraph.node * UGraph.node) -> bool
                                                       
  val getNodeSet: nodeWL -> N.set
  val getMoveSet: moveWL -> E.set


  val pushStack: UGraph.node -> unit
  val popStack: unit -> UGraph.node
  val stackNull: unit -> bool                         
  val getStackSet: unit -> N.set
  (* TODO: DO the rest *)

  (* Set of (register) strings *)
  structure C: ORD_SET
  (* OkColors *)
  val initOkColors: unit -> unit
  val hasFreeColor: unit -> bool
  val getAvailableColor: unit -> string
  val removeColors: C.set -> unit

  val getAlias: UGraph.node -> UGraph.node
                                 
  type allocation = Frame.register Temp.Table.table
  val precolor: (UGraph.node -> Temp.temp) -> unit
  val getColor: Temp.temp -> Frame.register
  val setColor: Temp.temp * Frame.register -> unit
  val updateSafeColors: unit -> unit
  val getAllocation: unit -> allocation
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

(* Set of edges *)
structure E = BinarySetFn(type ord_key = UGraph.node * UGraph.node
                          fun compare(e1, e2) = UGraph.pairCompare(e1, e2))
                         
                         
val coalescedMoves: E.set ref = ref E.empty
val constrainedMoves: E.set ref = ref E.empty
val frozenMoves: E.set ref = ref E.empty
val movesWL: E.set ref = ref E.empty
val activeMoves: E.set ref = ref E.empty

                                 
structure C = BinarySetFn(type ord_key = string
                          fun compare(s1, s2) = String.compare(s1, s2))


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
                  
fun getAndRemoveNode(wl) =
    let
      val n = chooseN(getNWL(wl))
    in
      removeNode(wl, n);
      n
    end

fun getAndRemoveMove(wl) =
    let
      val m = chooseE(getEWL(wl))
    in
      removeMove(wl, m);
      m
    end

fun getNode(wl) = chooseN(getNWL(wl))
fun getMove(wl) = chooseE(getEWL(wl))

fun isNin(wl, n) = N.member(getNWL(wl), n)
fun isEin(wl, e) = E.member(getEWL(wl), e)


                           
fun getNodeSet(wl) = getNWL(wl)

fun getMoveSet(wl) = getEWL(wl)
                           

(* Stack *)
val selectStack: UGraph.node list ref = ref []
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

(* Ok Colors *)
val okColors: C.set ref = ref C.empty

fun initOkColors() =
    okColors := C.addList(C.empty, Frame.registers)
                         
fun hasFreeColor() = not(C.isEmpty(!okColors))

fun getAvailableColor() =
    case C.find (fn _ => true) (!okColors)
     of SOME(n) => n
      | NONE => let exception emptyMoveWL
                in raise emptyMoveWL end

fun removeColors(badColors) = okColors := C.difference(!okColors, badColors)


val alias: UGraph.node UGraph.Table.table ref = ref UGraph.Table.empty
fun getAlias(n) =
    if isNin(COALESCED_N, n)
    then case UGraph.Table.look(!alias, n)
          of SOME(n') => getAlias(n')
           | NONE => let exception ourCodeHasABug
                     in raise ourCodeHasABug end
    else n


(* Coloring related *)
type allocation = Frame.register Temp.Table.table

val color: allocation ref = ref Temp.Table.empty
val safeColor: allocation ref = ref Temp.Table.empty

fun updateSafeColors() = safeColor := !color
                                        
(* Call only after precolored list has been initialized properly *)
fun precolor(gtemp) =
    (color :=
     (N.foldr
        (fn (n, tab) =>
            let
              val t = gtemp(n)
              val regStr = (case Temp.Table.look(Frame.tempMap, t)
                             of SOME(s) => s
                              | NONE => let exception unknownTemp
                                        in raise unknownTemp end)
            in
              Temp.Table.enter(tab, t, regStr)
            end)
        Temp.Table.empty
        (getNWL(PRECOLORED)));
     updateSafeColors())
      
      
fun getColor(t) =
    case Temp.Table.look(!color, t)
     of SOME(s) => s
      | NONE => let exception unColoredNode
                in raise unColoredNode end
                  
fun setColor(t, s) = color := Temp.Table.enter(!color, t, s)

fun getAllocation() = !safeColor



fun reset() =
    (precolored := N.empty;
     initial := N.empty;
     simplifyWL := N.empty;
     freezeWL := N.empty;
     spillWL := N.empty;
     spilledNodes := N.empty;
     coalescedNodes := N.empty;
     coloredNodes := N.empty;
     coalescedMoves := E.empty;
     constrainedMoves := E.empty;
     frozenMoves := E.empty;
     movesWL := E.empty;
     activeMoves := E.empty;
     selectStack := [];
     okColors := C.empty;
     alias := UGraph.Table.empty;
     color := Temp.Table.empty;
     safeColor := Temp.Table.empty
    )
fun initialize() = (reset();())
fun initializeWL(wl) = ()
                         (* fun makeWLs(igraph) = () *)

end
