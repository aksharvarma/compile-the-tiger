# Assignment 7
## Team members
1. Caitlin Matuszak
2. Akshar Varma

# Changes for final submission
* All changes made for the resubmission of instruction selection are also propagated here.
* Adds the UGraph module (ugraph.sig and ugraph.sml) which implements an undirected graph.
* Inteference graphs are modified to use the UGraph module (undirected) rather than the previous directed version presented by the Graph module. Flowgraphs remain directed, using the Graph module.
* Edges are added between all physical registers in the interference graph to prepare for register allocation.
* Corrects a few bugs with the liveness algorithm in livenessWL


## MakeGraph
One thing to note here is that we may come to know about the existence of a label before we actually see the instruction corresponding to the label. We handle this by keeping a mapping between labels and nodes of the CFG. If we see a label first, we store the node it was assigned, if we see a reference to the label first, we create a node, add the required edge, and remember that that particular label already has a node.

Other than this, the algorithm is straightforward.

## Liveness and Interference Graphs
For liveness, we use the worklist algorithm discussed in class.
Note: The order of the In and Out updates as mentioned in class needs to be switched for it to work. This is because LiveIn depends on LiveOut, and you need to update LiveOut before LiveIn.

For interference graphs, we use the algorithm from the book (Pg 221-222).
Note that the edges between the hardware registers are not yet complete. During register allocation, we will need to add edges between all hardware registers to indicate that they will always be separate registers.

## Miscellaneous changes to previous code/design
* Changes were made in the mipsgen module to correct the creation of Assem.OPER and Assem.MOVE instructions. Now, Assem.MOVE instructions are only created for instructions that involve only a transfer of data from one temp to another without any modification. This only occurs when we use the MIPS "move" instruction.
* The manner in which we handle index bound checks for arrays and dereferencing nil records was changed to make their corresponding code smaller and cleaner. This was also helpful in keeping our CFG makeGraph function simple.
* The exitTigMain function and hence derefNils and OutofBounds labels/basic blocks are sinks and we never come out of them. However, the canonicalizer adds a jump back from these labels to the place from which we make a call to these things. This jump is never taken, but there is nothing we can do about it without changing the canonicalizer. Plus, it doesn't affect correctness, so leaving the spurious jump is fine. Also note that we ensure that the main proc will be the final proc printed in the assembly file, so we don't need to worry about jumping to the end of the file when we finish the main body.
