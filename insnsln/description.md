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

## Canonicalizer and false branches falling through
The canonicalizer always sets it up so that the false branch is always fallthrough. So we assume the we can jump to the true label and depend on the canonicalizer to put the false label below. 

## munchArgs


## RV and liveness
We make a conservative estimate and assume that the RV is always live at the end of a procedure call for liveness analysis even if we are actually dealing with a procedure and not a function.

## Assumption about FP
We assume the following two things about the FP:
- We never add constants to the FP [TODO. Isn't it the exact opposite?]
- We never subtract from the FP

## SL register convention used [Change if we change]
We use the $v1 register as a dedicated SL register and keep all the actual arguments of the function in the $a0--$a3 registers.

## Not handling MEM(CONST i))
Why were we doing this again?


## framesize instead of FP
We use framesize instead of the FP because our stack cannot be dynamically allocated to.
