# Assignment 8
## Team members
1. Caitlin Matuszak
2. Akshar Varma

Our register allocator does spilling, coalescing as well as coloring of stack frame slots (all the levels).

The algorithm and logic for simplify, coalescing, spilling, and stack slot allocation of spilled temps are written in the `regalloc.sml` file, while the `color.sml` file does the actual coloring part of the algorithms, both for temps and stack slots. We have a worklists module which we use to keep track of all of the necessary data structures that are mentioned in the book.

# ugraph.sml
While we stick to the given starter graph module for control flow graphs, we use the graph module from Appel's Compilers course from 2012 (which he claims is a better version) for creating interference graphs. We make certain modifications to his module to make it an undirected graph module (and rename it UGraph). We also define additional functions to easily compute the adjacency list and degree of a given node.

# worklists.sml
This module allows us to abstract out details related to the dozen or so worklists that are needed for using the algorithm from the book.

There are functions that are visible to other modules that allow
* detemining if a worklist is empty or not,
* adding/removing specific elements to/from worklists, 
* getting an arbitrary element (and potentially removing it) from a worklist,
* determining if an element is in a particular worklist or not,
* and getting the full set of elements in a worklist.

This module also contains the logic for manipulating available colors for a temp, as well as managing aliases for temps. Lastly, this is the module which keeps track of the coloring/allocation that is given to a temp. 

(Note: we deviate from the given API by moving the `allocation` type to the worklists module from the color module since all other such data structure related content is in this module helping in keeping a cleaner abstraction.)

## datatype nodeWL
This is a datatype that keeps track of the various worklists that nodes/temps can be in at a time. These include most things mentioned in the "Node worklists, sets and stacks" subsection on Pg. 242. 

Since we do not have a need to keep track of an explicit initial worklist, we get rid of it and use all the nodes of the graph as our initial worklist. 

Also, we keep track of the stack separately from this datatype, as the others in the list are all better represented as sets, while the stack is naturally represented as an SML list.

## datatype moveWL
The five worklists mentioned in the "Move sets" subsection on Pg. 243 are included in this datatype.

## Use of sets
We use the BinarySetFn functor to create sets for various purposes. The main sets we use are node sets, move/edge sets, and set of colors (string representations of registers), sets of stack frame slots (interger sets representing offsets from the frame pointer).

`structure N: ORD_SET`
Node sets are simply sets of nodes (they are in fact the same as UGraph.S sets, which is used internally by the UGraph module).

`structure E: ORD_SET`
Move sets are sets having node sets as its elements; keeping track of move edges. We use node sets instead of tuples to better represent the undirected nature of edges in the interference graph.

`structure C: ORD_SET`
We use a set of strings as the available colors for a temp, using the string representations of physical registers as the colors. We also make use of the internal structure of this set to be enforce a preference order on the choice of registers as follows:
To get an arbitrary color/physical register, we use a call of the form:
    `C.find (fn _ => true) (availableColors)`
This goes through the list of elements in `availableColors` and returns the first one. The BinarySetFn functor implements a balanced binary tree, and based on a few tests we observed that operations such as find, listItems (and app, map, etc.) all use an inorder traversal of the elements as the order for traversing through the set. By providing the right kind of compare function to the functor, we control this traversal and hence the order of choosing registers. We define and use a registerCompare function inside `mipsframe.sml` to get the preference order of our choosing.

# regalloc.sml
At a high level, our register allocation algorithm is pretty much exactly what is present in the book. However, due to differences in our representation of graphs and worklists, there are things that are done slightly differently to get the same effect.

## effectiveDegree
We want the degree of precolored nodes to be very high all the time. And since we use our graph module to find the degree, we get the current degree (possibly with some edges removed). However, there are places where the book's algorithm assumes (as an invariant) that precolored nodes have higher degree than everything else. 

To avoid any potential issues, we write a wrapper around our degree function from the UGraph module, to return an impossibly high value (number of registers * number of nodes) as the degree of precolored nodes.

## originalAdjTab/adjTab
During the algorithm's simplify/coalesce phases, the interference graph is modified, with some edges being deleted and other being added. But we need the edges of the graph for coloring. We store the original adjacency list in a table indexed using the nodes of the graph.

- originalAdjTab contains the original interference graph.
- adjTab contains the interference graph including coalesced edges.

## processNeighbours
We modify the decrementDegree procedure since we don't need to actually decrementDegree, only to remove the edge from the graph and let our UGraph module handles degrees. So, processNeighbours only performs the worklist manipulations that are present in decrementDegree and hence looks different and is called slightly differently (over all nodes adjacent to a node instead of for one neighbour at a time).

## checkNewSpill
When we combine two nodes u and v while coalescing them (v becomes u), we need to add edges between u and v's neighbours. However, this might increase the degree of u and that might cause it to have to be moved to the TOSPILL worklist. The book doesn't seem to consider this and we think that it is a bug. We fix it by adding a check after adding these edges to see if we need to change u's worklist.

## removeRedundantMoves
This function simply removes a move from the list of instructions if the two temps involved are actually the same.

## Part 4: allocateStackSlots (choosing stack slots for spilled nodes)
We follow the given algorithm for this, and again what minor differences we have are due to the different organization our data structures in comparison to the book.

We adjust the existing interference graph to get the correct interference for the spilled temps. We also make a `spilledAdjTab` similar to the `origingalAdjTab` that was created for the main coloring.

# color.sml
This module does the actual coloring using the worklist module pretty much exactly as the AssignColors() procedure from Pg.249 of the book. Also has the function that does the coloring of stack slots once the algorithm has done simplify/coalesce.

Note: The signature that we have is much leaner than what is mentioned in the book because we directly have access to the initial allocation via the worklist module. Further, there is no need for a spillCost function as we are not spilling inside the color module at all. We add the adjTab since we otherwise destroy the edges of the graph during the algorithm.

# Changes to previous code/design
## Interference Graphs
We changed earlier code to use the UGraph module for creating interefernce graphs, instead of a directed graph. We also added the interference between the physical registers so that register allocation works correctly. 

## Frame
We add the necessary prolog code for moving the callee saves registers to temporary temps and epilog code for moving them back in `procEntryExit1`.

We also decide that argument registers are considered caller saves and hence go into the trashedByCall set which is set as defs for a `jal funLabel` function call. 
