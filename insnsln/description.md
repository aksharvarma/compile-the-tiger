# Assignments 6
## Team members
1. Caitlin Matuszak
2. Akshar Varma

Takes the IR Tree, canonicalizes it and performs instruction selection.

## Change of signatures
There is now a globally visible Frame structure which has the signature FRAME and is set equal to MipsFrame in our case. This means that no other module has a internal variable which defines the Frame structure that it uses. This improves the modularity due to the following changes:
    * Semant does not even know that there is anything called Frame
    * Semant primarily does type-checking and calls Translate for the translation to IR work.
    * Translate stores the final frag list and a driver module extracts it out. Semant is free from having to do this.
    * This Frame can be accessed in MipsGen without any weird SML type errors because it doesn't know if frag is a Translate.Frame.frag or a MipsGen.Frame.frag.


## Stores
There are only three stores (cases of T.MOVE with a MEM on the left) because we only have the store word instruction.
* MEM on the left 
* The two symmetric cases where the MEM on the left has a constant offset addition inside it. (T.MOVE(addr+i, reg) and T.MOVE(i+addr, reg))

## Loads
There are 4 cases for the load because of the possibility of load immediate.
* We are loading in an immediate into a register.
* The address from which we're loading can have an offset. (2 symmetric cases)
* The MEM on the right and a TEMP on the left. However, we use the catchall mechanism to get at this since the TEMP will come from munchExp(e) for some e, which also catches everything. So both those cases are equivalent.

## munchArgs
