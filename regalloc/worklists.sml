(* Contains all logic and data structures related to the various worklists
 * involved in register allocation and coloring
 *)
structure WL:WL =
struct
(* Node worklists:
 * - PRECOLORED:  machine registers, preassigned a color
 * - SIMPLIFY:    low-degree non-move-related nodes
 * - FREEZE:      low-degree move-related nodes
 * - TOSPILL:     high-degree nodes
 * - SPILLED:     nodes marked for spilling during this round
 * - COALESCED_N: nodes that have been coalesced. When (u,v) is coalesced, v
 *                is added to this set and u is put back on some worklist.
 * - COLORED:     nodes successfully colored
 *
 * Note: every node is always in exactly one of the above sets or in the stack
 * to be colored.
 *)
datatype nodeWL = PRECOLORED | SIMPLIFY | FREEZE | TOSPILL | SPILLED
                  | COALESCED_N | COLORED

(* Move worklists:
 * - COALESCED_E: moves that have been coalesced
 * - CONSTRAINED: moves whose source and target interfere
 * - FROZEN:      moves that will no longer be considered for coalescing
 * - MOVES:       moves enabled for possible coalescing
 * - ACTIVE:      moves not yet ready for coalescing
 *
 * Note: every move is in exactly one of the above sets
 *)
datatype moveWL = COALESCED_E | CONSTRAINED | FROZEN | MOVES | ACTIVE

(* Set of nodes *)
structure N = UGraph.S

(* Define initial refs for all node worklists *)
val precolored: N.set ref = ref N.empty
val simplifyWL: N.set ref = ref N.empty
val freezeWL: N.set ref = ref N.empty
val spillWL: N.set ref = ref N.empty
val spilledNodes: N.set ref = ref N.empty
val coalescedNodes: N.set ref = ref N.empty
val coloredNodes: N.set ref = ref N.empty

(* Set of edges *)
structure E = BinarySetFn(type ord_key = N.set
                          fun compare(e1, e2) = N.compare(e1, e2))

(* Define initial refs for all move worklists *)
val coalescedMoves: E.set ref = ref E.empty
val constrainedMoves: E.set ref = ref E.empty
val frozenMoves: E.set ref = ref E.empty
val movesWL: E.set ref = ref E.empty
val activeMoves: E.set ref = ref E.empty

(* getNRef : nodeWL -> N.set ref
 * Gets the ref version of the given node worklist
 *)
fun getNRef(PRECOLORED) = (precolored)
  | getNRef(SIMPLIFY) = (simplifyWL)
  | getNRef(FREEZE) = (freezeWL)
  | getNRef(TOSPILL) = (spillWL)
  | getNRef(SPILLED) = (spilledNodes)
  | getNRef(COALESCED_N) = (coalescedNodes)
  | getNRef(COLORED) = (coloredNodes)

(* getNWL : nodeWL -> N.set
 * Extracts the set from the given node worklist
 *)
fun getNWL(wl) = !(getNRef(wl))

(* getERef : moveWL -> E.set ref
 * Gets the ref version of the given move worklist
 *)
fun getERef(COALESCED_E) = (coalescedMoves)
  | getERef(CONSTRAINED) = (constrainedMoves)
  | getERef(FROZEN) = (frozenMoves)
  | getERef(MOVES) = (movesWL)
  | getERef(ACTIVE) = (activeMoves)

(* getEWL : moveWL -> E.set
 * Extracts the set from the given move worklist
 *)
fun getEWL(wl) = !(getERef(wl))

(* isNullN : nodeWL -> bool
 * Determines whether the given node worklist is null
 *)
fun isNullN(wl) = N.isEmpty(getNWL(wl))

(* isNullE : moveWL -> bool
 * Determines whether the given move worklist is null
 *)
fun isNullE(wl) = E.isEmpty(getEWL(wl))

(* isNotNullN : nodeWL -> bool
 * Determines whether the given node worklist is not null
 *)
fun isNotNullN(wl) = not(isNullN(wl))

(* isNotNullE : moveWL -> bool
 * Determines whether the given move worklist is not null
 *)
fun isNotNullE(wl) = not(isNullE(wl))

(* addNode : nodeWL * UGraph.node -> unit
 * Adds the given node to the given node worklist
 *)
fun addNode(wl, item) = getNRef(wl) := N.add(getNWL(wl), item)

(* addMove : moveWL * E.item -> unit
 * Adds the given move to the given move worklist
 *)
fun addMove(wl, item) = getERef(wl) := E.add(getEWL(wl), item)

(* removeNode : nodeWL * UGraph.node -> unit
 * Removes the given node from the given node worklist
 *)
fun removeNode(wl, item) = getNRef(wl) := N.delete(getNWL(wl), item)

(* removeMove : moveWL * E.item -> unit
 * Removes the given move from the given move worklist
 *)
fun removeMove(wl, item) = getERef(wl) := E.delete(getEWL(wl), item)

(* chooseN : N.set -> N.item
 * Select an element from the given node set.
 * Throws an exception if the given set is empty.
 *)
fun chooseN(wl) =
    case N.find (fn _ => true) (wl)
     of SOME(n) => n
      | NONE => let exception emptyNodeWL
                in raise emptyNodeWL end

(* chooseE : E.set -> E.item
 * Select an element from the given move set.
 * Throws an exception if the given set is empty.
 *)
fun chooseE(wl) =
    case E.find (fn _ => true) (wl)
     of SOME(nSet) => nSet
      | NONE => let exception emptyMoveWL
                in raise emptyMoveWL end

(* getMoveContents : E.item -> UGraph.node * UGraph.node
 *
 * Get the pair of nodes associated with the given move item.
 * Note: Assumes the invariant that there either exactly one or
 *       two items in a given move item.
 *       If there are two, return two nodes.
 *       If there is only one, then this is a move edge from a node
 *       to itself, so return the single node twice.
 *)
fun getMoveContents(m) =
    let
      val mList = N.listItems(m)
      val nItems = List.length(mList)
    in
      if nItems = 2
      then ((hd mList), hd(tl mList))
      else if nItems = 1
      then  ((hd mList), (hd mList))
      else
        let exception edgeInvariantViolated
        in raise edgeInvariantViolated end
    end

(* getNode: nodeWL -> UGraph.node
 * Get a node from the given node worklist
 *)
fun getNode(wl) = chooseN(getNWL(wl))

(* getAndRemoveNode : nodeWL -> UGraph.node
 * Remove a node from the given node worklist and return the removed node
 *)
fun getAndRemoveNode(wl) =
    let
      val n = chooseN(getNWL(wl))
    in
      removeNode(wl, n);
      n
    end

(* getAndRemoveMove : moveWL -> E.item
 * Remove a move from the given move worklist and return the removed move item
 *)
fun getAndRemoveMove(wl) =
    let
      val m = chooseE(getEWL(wl))
    in
      removeMove(wl, m);
      m
    end

(* isNin : nodeWL -> bool
 * Determines whether the given node is in the given node worklist
 *)
fun isNin(wl, n) = N.member(getNWL(wl), n)

(* isEin : moveWL -> bool
 * Determines whether the given move is in the given move worklist
 *)
fun isEin(wl, e) = E.member(getEWL(wl), e)

(* getNodeSet : nodeWL -> N.set
 * Extract the set of nodes from the given node worklist *)
fun getNodeSet(wl) = getNWL(wl)

(* getMoveSet : moveWL -> E.set
 * Extract the set of moves from the given move worklist *)
fun getMoveSet(wl) = getEWL(wl)

(* Stack *)
val selectStack: UGraph.node list ref = ref []

(* pushStack : UGraph.node -> unit
 * Push the given node onto the stack
 *)
fun pushStack(n) = selectStack := n::(!selectStack)

(* popStack : unit -> UGraph.node
 * Pop a node off of the stack
 *)
fun popStack() =
    let
      val n = hd(!selectStack)
    in
      selectStack := tl(!selectStack);
      n
    end
fun stackNull() = List.null(!selectStack)

(* getStackSet : unit -> N.set
 * Extract the set of nodes currently on the stack
 *)
fun getStackSet() = N.addList(N.empty, !selectStack)

(* whichNWL : UGraph.node -> nodeWL option
 *
 * Determine which node worklist the given node is in.
 * If it is in worklist wl, returns SOME(wl),
 * if it is in the stack, returns NONE
 * Throws exception if invariants are violated and neither case is satisfied.
 *)
fun whichNWL(n) =
    if isNin(PRECOLORED, n) then SOME(PRECOLORED)
    else if isNin(SIMPLIFY, n) then SOME(SIMPLIFY)
    else if isNin(FREEZE, n) then SOME(FREEZE)
    else if isNin(TOSPILL, n) then SOME(TOSPILL)
    else if isNin(SPILLED, n) then SOME(SPILLED)
    else if isNin(COALESCED_N, n) then SOME(COALESCED_N)
    else if isNin(COLORED, n) then SOME(COLORED)
    else if List.exists (fn stackN => n=stackN) (!selectStack) then NONE
    else let exception NotInAnyNWL in raise NotInAnyNWL end

(* printNWL : UGraph.node -> unit
 * Print the name of the worklist that the given node is in *)
fun printNWL(n) =
    if isNin(PRECOLORED, n) then print("PRECOLORED\n")
    else if isNin(SIMPLIFY, n) then print("SIMPLIFY\n")
    else if isNin(FREEZE, n) then print("FREEZE\n")
    else if isNin(TOSPILL, n) then print("TOSPILL\n")
    else if isNin(SPILLED, n) then print("SPILLED\n")
    else if isNin(COALESCED_N, n) then print("COALESCED_N\n")
    else if isNin(COLORED, n) then print("COLORED\n")
    else if List.exists (fn stackN => n=stackN) (!selectStack)
    then print("STACK\n")
    else let exception NotInAnyNWL in raise NotInAnyNWL end

(* Set of (register) strings
 * Note: ordered with a special ordering function: Frame.registerCompare
 *       to prefer choosing certain registers if possible
 *)
structure C = BinarySetFn(type ord_key = string
                          fun compare(s1, s2) = Frame.registerCompare() (s1, s2))

(* Ok Colors *)
val okColors: C.set ref = ref C.empty

(* initOkColors : unit -> unit
 * Initializes okColors to include all physical registers
 *)
fun initOkColors() =
    okColors := C.addList(C.empty, Frame.registers)

(* hasFreeColor : unit -> bool
 * Determines whether there is currently a free color available
 *)
fun hasFreeColor() = not(C.isEmpty(!okColors))

(* getAvailableColor: unit -> string
 * Gets an available color. Throws an exception if there isn't one.
 *)
fun getAvailableColor() =
    case C.find (fn _ => true) (!okColors)
     of SOME(n) => n
      | NONE => let exception noColorAvailable
                in raise noColorAvailable end

(* Remove the given set of colors from the okColors set *)
fun removeColors(badColors) = okColors := C.difference(!okColors, badColors)

(* Alias table *)
val alias: UGraph.node UGraph.Table.table ref = ref UGraph.Table.empty

(* getAlias : UGraph.node -> UGraph.node
 * Get the alias of teh given node if it has been coalesced.
 * If not, the node is its own alias
 *)
fun getAlias(n) =
    if isNin(COALESCED_N, n)
    then case UGraph.Table.look(!alias, n)
          of SOME(n') => getAlias(n')
           | NONE => let exception coalescedWithoutAlias
                     in raise coalescedWithoutAlias end
    else n

(* setAlias : UGraph.node * UGraph.node -> unit
 * Set the alias of the given node
 *)
fun setAlias(n, aliasN) =
    alias := UGraph.Table.enter(!alias, n, aliasN)

(* A mapping from temps to the registers (strings) they have been colored as *)
type allocation = Frame.register Temp.Table.table

(* The current color assignments *)
val color: allocation ref = ref Temp.Table.empty

(* precolor : (UGraph.node -> Temp.temp) -> unit
 *
 * Color all of the physical registers with their appropriate colors.
 * Note: call only after precolored list has been initialized properly
 *)
fun precolor(gtemp) =
    color :=
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
       (getNWL(PRECOLORED)))

(* getColor : Temp.temp -> Frame.register
 * Get the color assigned to the given temp.
 * Throws exception if the temp has not yet been colored.
 *)
fun getColor(t) =
    case Temp.Table.look(!color, t)
     of SOME(s) => s
      | NONE => let exception unColoredNode
                in raise unColoredNode end
(* setColor : Temp.temp * Frame.register -> unit
 * Set the color of the given temp to the given register
 *)
fun setColor(t, s) = color := Temp.Table.enter(!color, t, s)

(* getAllocation : unit -> allocation
 * Extract the current mapping from temps to registers
 *)
fun getAllocation() = !color

(* initialize : unit -> unit
 * Resets all lists/set refs to be empty
 *)
fun initialize() =
    (precolored := N.empty;
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
     color := Temp.Table.empty)

(* A set of integers *)
structure I = BinarySetFn(type ord_key = int
                          fun compare(e1, e2) = Int.compare(e1, e2))
end
