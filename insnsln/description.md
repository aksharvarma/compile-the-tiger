# Assignments 6
## Team members
1. Caitlin Matuszak
2. Akshar Varma

Changes and bug fixes added on the last submission are detailed at the end of the file.

Takes the IR Tree, canonicalizes it and performs instruction selection.

1. The first part of this file mentions some design changes from last time.
2. The second part mentions design choices for instruction selection.
3. The test4.tig.s file contains the assembler listing for a factorial function.

# Design changes from last time
We had made certain assumptions during the translation to IR tree phase of the compiler. Some of those have changed based on new information from the instruction selection phase.

## Change of signatures
There is now a globally visible Frame structure which has the signature FRAME and is set equal to MipsFrame in our case. This means that no other module has an internal variable which defines the Frame structure that it uses. This improves the modularity due to the following changes/refinements:
    * Semant does not even know that there is anything called Frame
    * Semant primarily does type-checking and calls Translate for the translation to IR work.
    * Translate stores the final frag list and a driver module extracts it out. Semant is free from having to do this.
    * This Frame can be accessed in MipsGen without any weird SML type errors because it doesn't know if frag is a Translate.Frame.frag or a MipsGen.Frame.frag (which would occur if Frame was an internal variable in different modules).

## Labels for fragment
During the IR generation, we had created fragments for each function, but we had assumed that they would be brought together in a nice way. One particular thing we had not done earlier in our translation was to add a label at the start of the function. We rectify that now.

## procEntryExit1
We had assumed that procEntryExit1 would create the prolog and epilog for functions, but that is to be done by the other procEntryExit functions. This is rectified.

## Error handling
Earlier we had a IR Tree with labels that we jumped to in case of errors, printed errors and jumped to a DONE label which was supposed to indicate the end of the program. However, the canonicalizer jumbles up orders so that DONE is not necessarily at the end anymore.

We rectify this by assuming that there'll be a function in the runtime.c which we call with an argument specifying whether we are exiting the program due to an error (and if so, which error) or if it is a good, normal exit.

The function will look something like this:
  exit_TigMain(errno){
    int badDerefExit=-1, outOfBoundsExit=-2;
    if (errno=badDerefExit){printf("Array-out-of-bound-error.\n");
    if (errno=outOfBoundsExit){printf("Tried to dereference Nil.\n");
    exit(errno);
  }

Note: We assume that these functions that are added to the runtime by the compiler can be added to runtime.c and recompiled to get assembly code or that we will manually add the required assembly for the functions ourselves.



# Choices related to Instruction Selection
## Stores
There are only three stores (cases of T.MOVE with a MEM on the left) because we only have the store word instruction.
* MEM on the left
* The two symmetric cases where the MEM on the left has a constant offset addition inside it. (T.MOVE(addr+i, reg) and T.MOVE(i+addr, reg))

## Loads
There are 4 cases for the load because of the possibility of load immediate.
* We are loading in an immediate into a register.
* The address from which we're loading can have an offset. (2 symmetric cases)
* The MEM on the right and a TEMP on the left. However, we use the catchall mechanism to get at this since the TEMP will come from munchExp(e) for some e, which also catches everything. So both those cases are equivalent.

Note: There is one case where we move a temp to a temp, we do not count that in either of the above counts for store/load.

## Canonicalizer and false branches falling through
The canonicalizer always sets it up so that the false branch is always fallthrough. So we assume the we can jump to the true label and depend on the canonicalizer to put the false label below.

## munchArgs
munchArgs needs to call munchExp on the arguments to a function. It also returns the temps from the munchExp call.

* For register args (# of args<4), we simply move args to the corresponding argument register and returns the corresponding argument register as a destination register.
* If there are more than 4 arguments, then arguments 5 onwards are moved to their appropriate place in the frame and there is no destination register.

## RV and liveness
We make a conservative estimate and assume that the RV is always live at the end of a procedure call for liveness analysis even if we are actually dealing with a procedure and not a function.

## Assumption about FP
We assume the following two things about the FP:
- We only add constants to the FP
- We never subtract from the FP
This allows changing FP to SP+framesize using very few changes to MipsGen.

## SL register convention used
We do not consider the SL argument as being different from the other arguments. Thus, we use the $a0 register for the SL.

## Extra callee-saves registers
Registers $fp and $v1 are used as callee saves registers:
- $fp because we do not have a dedicated frame pointer
- $v1 because Tiger doesn't have a concept of a double word.
We make them callee-saves because to make the number of both callee and caller saves regsiters approximately equal.

## Not handling MEM(CONST i))
This was one of the cases mentioned in class, but we do not do this because Tiger cannot explicitly access memory, and our compiler never does this in the Tree language.

## framesize instead of FP
We use framesize instead of the FP because our stack cannot be dynamically allocated to.

## End of "main" function
As mentioned above, our earlier design to denote end of program was to have a DONE label at the end. We have changed this to make a function call to a runtime function which is simply an exit call in C. With the correct arguments we control whether the exit is a good, normal exit, or an exit caused due to an error (refer above).

## Assembly listing
We provide the assembly that our compiler generates on test4.tig from Appel's list of testcases. This contains a function to compute the factorial and makes a call to this function, as required in the assignment.

# Fixes added on resubmission
## Bug fixes
* Fixed string manipulation bug (corrected in the second email) causing an exception to be thrown for labels of regular functions of < 4 characters
* Fixed bug with an extraneous MEM node when converting offsets from the frame pointer to offsets from the stack pointer. This affected the correctness of the static links.
* Logic error with assignment to an element of an array. This was due to a misordering of the patterns in munchStm. This has been corrected.
* Fixed bug with 4 arguments. Now correctly writes the extra arguments to the correct offset from the stack pointer

## Changes to FPtoSP translation
* Now we assume that the stack pointer is at the bottom of the outgoing arguments section, and that the frame size is large enough to accommodate the maximum number of outgoing arguments that this frame will need. Making these assumptions greatly simplified the function call logic and eliminated all of the precall and postcall instructions that we previously believed were necessary.
* Now we assume that we have a fixed frame size (large enough to accommodate the maximum number of outgoing arguments) and that the register allocator will write this frame size to <FRAMELABEL>\_framesize when it determines the final size.

