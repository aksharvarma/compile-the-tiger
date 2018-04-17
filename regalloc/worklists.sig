(* Contains all logic and data structures related to the various worklists
 * involved in register allocation and coloring
 *)
signature WL =
sig
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
  structure N : ORD_SET
  (* Set of moves (edges) *)
  structure E : ORD_SET

  (* Initializes/resets all of the worklists to be empty *)
  val initialize: unit -> unit

  (* Determines whether the given node worklist is null *)
  val isNullN: nodeWL -> bool
  (* Determines whether the given node worklist is not null *)
  val isNotNullN: nodeWL -> bool
  (* Determines whether the given move worklist is null *)
  val isNullE: moveWL -> bool
  (* Determines whether the given move worklist is not null *)
  val isNotNullE: moveWL -> bool

  (* Adds the given node to the given node worklist *)
  val addNode: nodeWL * UGraph.node -> unit
  (* Adds the given move to the given move worklist *)
  val addMove: moveWL * E.item -> unit

  (* Removes the given node from the given node worklist *)
  val removeNode: nodeWL * UGraph.node -> unit
  (* Removes the given move from the given move worklist *)
  val removeMove: moveWL * E.item -> unit

  (* Select an element from the given node set *)
  val chooseN: N.set -> N.item
  (* Get the pair of nodes associated with the given move item *)
  val getMoveContents: E.item -> UGraph.node * UGraph.node

  (* Get a node from the given node worklist *)
  val getNode: nodeWL -> UGraph.node
  (* Remove a node from the given node worklist and return the removed node *)
  val getAndRemoveNode: nodeWL -> UGraph.node
  (* Remove a move from the given move worklist and return the removed move *)
  val getAndRemoveMove: moveWL -> E.item

  (* Determines whether the given node is in the given node worklist *)
  val isNin: nodeWL * UGraph.node -> bool
  (* Determines whether the given move is in the given move worklist *)
  val isEin: moveWL * E.item -> bool
  (* Determine which node worklist the given node is in.
   * If it is in worklist wl, returns SOME(wl),
   * if it is in the stack, returns NONE
   * Throws exception if invariants are violated and neither case is satisfied.
   *)
  val whichNWL: UGraph.node -> nodeWL option
  (* Print the name of the worklist that the given node is in *)
  val printNWL: UGraph.node -> unit

  (* Extract the set of nodes from the given node worklist *)
  val getNodeSet: nodeWL -> N.set
  (* Extract the set of moves from the given move worklist *)
  val getMoveSet: moveWL -> E.set

  (* Push the given node onto the stack *)
  val pushStack: UGraph.node -> unit
  (* Pop a node off of the stack *)
  val popStack: unit -> UGraph.node
  (* Determines whether the stack is currently null *)
  val stackNull: unit -> bool
  (* Extract the set of nodes currently on the stack*)
  val getStackSet: unit -> N.set

  (* Set of (register) strings *)
  structure C: ORD_SET
  (* Initializes the okColors set to include all physical registers *)
  val initOkColors: unit -> unit
  (* Determines whether there is currently a free color available *)
  val hasFreeColor: unit -> bool
  (* Gets an available color. Throws an exception if there isn't one *)
  val getAvailableColor: unit -> string
  (* Remove the given set of colors from the okColors set *)
  val removeColors: C.set -> unit

  (* Get the alias of the given node if it has been coalesced.
   * If not, the node is its own alias *)
  val getAlias: UGraph.node -> UGraph.node
  (* Set the alias of the given node *)
  val setAlias: UGraph.node * UGraph.node -> unit

  (* A mapping from temps to the registers (strings) they have been colored as *)
  type allocation = Frame.register Temp.Table.table
  (* Color all of the physical registers with their appropriate colors *)
  val precolor: (UGraph.node -> Temp.temp) -> unit
  (* Get the color that the given temp has been assigned *)
  val getColor: Temp.temp -> Frame.register
  (* Set the color of the given temp to the given register *)
  val setColor: Temp.temp * Frame.register -> unit
  (* Extract the current mapping from temps to registers *)
  val getAllocation: unit -> allocation

  (* A set of integers *)
  structure I: ORD_SET
end
