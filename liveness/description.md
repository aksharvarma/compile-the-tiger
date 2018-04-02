# Assignment 7
## Team members
1. Caitlin Matuszak
2. Akshar Varma

## MakeGraph
* MakeGraph.instrs2graph creates the control flow graph for the given list of instructions. Each node corrresponds to a single assembly instruction. The def and use sets correspond to the dst and src lists in the assem instructions, respectively. Each time a node is created in the graph, the def, use, and ismove tables in the flowgraph are updated accordingly with the information associated with the new node and its assembly instruction. Note that once we add this information to these three tables in the flowgraph, we no longer need the associated assembly instruction to compute liveness or to accomplish register allocation. All relevant data for the remaining steps is stored in the flowgraph itself.

##Liveness

## Miscellaneous
* Changes were made in the mipsgen module to correct the creation of Assem.OPER and Assem.MOVE instructions. Now, Assem.MOVE instructions are only created for instructions that involve only a transfer of data from one temp to another without any modification. This only occurs when we use the MIPS "move" instruction.
