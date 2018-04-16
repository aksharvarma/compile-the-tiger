(* Graph module provided in the starter files.
 * Used only as a directed graph.
 * Modified to not add duplicate edges
 *)
signature GRAPH =
sig
    type graph
    type node

    val nodes: graph -> node list
    val succ: node -> node list
    val pred: node -> node list
    val adj: node -> node list   (* succ+pred *)
    val eq: node*node -> bool

    val newGraph: unit -> graph
    val newNode : graph -> node
    exception GraphEdge
    val mk_edge: {from: node, to: node} -> unit
    val rm_edge: {from: node, to: node} -> unit

    structure Table : TABLE
    sharing type Table.key = node
    val lookUpNode: 'a Table.table * node -> 'a

    val nodename: node->string  (* for debugging only *)
end
