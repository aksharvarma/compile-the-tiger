structure UGraph : UGRAPH =
struct
structure S = BinarySetFn(type ord_key = int
                          fun compare(a,b) = Int.compare(a,b))

  type node = S.Key.ord_key
  datatype noderep = NODE of {adj: S.set}

  val emptyNode = NODE{adj=S.empty}

  structure H = RedBlackMapFn(S.Key)
  type graph = noderep H.map ref

  val nodeCount = ref 0
  fun incNodeCount() = nodeCount:= !nodeCount+1

  fun newGraph() = ref H.empty

  fun put(g,i,x) = g := H.insert(!g, i, x)
  fun get(g,i) = case H.find(!g,i) 
                 of SOME x => x 
                  | NONE => let exception noNodeFound
                            in raise noNodeFound end
                              (* (put(g,i,emptyNode); emptyNode) *)

  fun newNode(g) = (incNodeCount();
                    put(g, !nodeCount, emptyNode);
                    !nodeCount)
      

  (* fun succ g i = let val NODE{succ=x,...} = get(g,i) in x end *)
  (* fun pred g i = let val NODE{pred=x,...} = get(g,i) in x end *)
  fun adjSet g i =  let val NODE{adj=a} = get(g,i) in a end
  fun adjList g i = S.listItems(adjSet g i)

  fun mkEdge g (n1, n2) =
    let val NODE{adj=a1} = get (g,n1)
        val NODE{adj=a2} = get (g,n2)
     in put (g, n1, NODE{adj=S.union(a1, S.singleton n2)});
        put (g, n2, NODE{adj=S.union(a2, S.singleton n1)})
    end

  fun delete'(s,x) = S.delete(s,x) handle NotFound => s
  fun rmEdge g (n1, n2) =
    let val NODE{adj=a1} = get (g,n1)
        val NODE{adj=a2} = get (g,n2)
     in put(g, n1, NODE{adj=delete'(a1, n2)});
        put(g, n2, NODE{adj=delete'(a2, n1)})
    end

  fun nodeSet g = H.foldli (fn(i,_,s) => S.add(s,i)) S.empty (!g)
  fun nodeList g = S.listItems(nodeSet g)

                              
  fun isEdge g (u, v) = S.member((adjSet g u), v)

  fun degree g i = S.numItems((adjSet g i))

  structure Table = IntMapTable(type key = node
                                fun getInt(n) = n)

                               
  fun lookUpNode(map, node) =
      (case Table.look(map, node)
        of SOME(t) => t
         | NONE => let exception NodeNotFound
                   in raise NodeNotFound end)

                             
  fun compare(a, b) = Int.compare(a, b)

  fun pairCompare((a, b), (c, d)) =
      let
        fun twoOrderToOne(EQUAL, EQUAL) = EQUAL
          | twoOrderToOne(LESS, _) = LESS
          | twoOrderToOne(GREATER, _) = GREATER
          | twoOrderToOne(_, LESS) = LESS
          | twoOrderToOne(_, GREATER) = GREATER
      in
        twoOrderToOne(compare(a,c), compare(b,d))
      end

  fun nodeName(n) = "n" ^ Int.toString(n)
                                      
end

