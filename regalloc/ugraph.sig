signature UGRAPH = sig
  structure S : ORD_SET

  type node
  
  type graph

  val adjSet: graph -> node -> S.set  (*    *)
  val adjList: graph -> node -> node list  (*    *)
    
  val newGraph: unit -> graph  (* makes a new, empty graph *)
  val newNode: graph -> node
                          
  val mkEdge: graph -> node * node -> unit

  val rmEdge: graph -> node * node -> unit

  val nodeSet: graph -> S.set
  val nodeList: graph -> node list

  structure Table : TABLE
  sharing type Table.key = node

  val lookUpNode: 'a Table.table * node -> 'a
  val compare: node * node -> order
  val pairCompare: (node * node) * (node * node) -> order

  val nodeName: node->string
end
