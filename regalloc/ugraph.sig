(* Adapted from:
 * http://www.cs.princeton.edu/courses/archive/spring12/cos320/homeworks/hw8/
 * Used as an undirected graph.
 *)
signature UGRAPH = sig
  structure S : ORD_SET
  type node
  type graph

  val adjSet: graph -> node -> S.set
  val adjList: graph -> node -> node list

  val newGraph: unit -> graph
  val newNode: graph -> node

  val mkEdge: graph -> node * node -> unit
  val rmEdge: graph -> node * node -> unit

  val nodeSet: graph -> S.set
  val nodeList: graph -> node list
  val nodeName: node->string

  structure Table : TABLE
  sharing type Table.key = node

  val lookUpNode: 'a Table.table * node -> 'a
  val degree: graph -> node -> int
end
