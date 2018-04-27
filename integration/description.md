# Assignment 9
## Team members
1. Caitlin Matuszak
2. Akshar Varma

# Integration phase
Fixes any inconsistencies through the different components and phases to get a working compiler for the Tiger language.

We first mention the changes needed to fix gaps in our compiler to get the final assembly output running. Then we mention modifications we made to the runtime, followed by some minor optimizations we added to our compiler. Finally, we mention a few design choices about our compiler that would have been nice to be able to change but which seem too diffcult to actually change at this stage.

# List of changes
## Frame related changes
There are changes made to `mipsframe.sml` to accommodate the view shift, the prolog and the epilog for functions as well as the mechanism to use strings in MIPS.

Note: We had added a list of registers and a tempMap in the frame module earlier so those changes mentioned in the chapter are already done.

### Finish View shift
This is done using the genViewShift function inside procEntryExit1.

For every argument that is passed in, we need to move them into the place from they'll be used from inside the function based on their accesses. This is done inside procEntryExit1 in Tree statements so that when instruction selection happens, it'll use the correct temps (or store from correct place) whenever it wants to use a variable. Later, the register allocator will try to keep variables in the argument registers if possible (due to coalescing).

Note: We had already done the saving and restoring of the calleeSaves registers in the register allocation phase, we don't need to change anything now.

### procEntryExit3
This finishes up the way functions look. 

* Prolog: 
  - It adds a label to denote the start of the function.
  - Sets a constant equal to the framesize used by the function, calculated by counting the number of locals that are needed. This includes escaping variables as well as anything that the registor allocator spilled.
  - Allocates stack space by moving the stack pointer.
* Epilog: 
  - Adds the code for deallocating stack space.
  - Instruction for jumping out of the function (`jr $ra`)

Our runtime is compatible with both the MARS and SPIM emulators. The only place in our compiler that needs to change for this is when we set the constant. We allow both emulators to be accommodated by using different (instruction) strings for different emulators. The compiler is then run using the emulator version that we are interested in running on. The way in which we set a constant equal to the framesize is done differently depending on the emulator that is used. 

### Strings
The runtime for Tiger assumes that strings are prefixed with a word containing the length of the string.

This is done by putting code like this:
strLabel:
    .word strLength
    .ascii str

### .text and .data segments
Apart from what has been mentioned for strings, we need the following code if there is any string data.
    .data
    .align 4
We add that in just before writing the code fragments into a file. 

The actual function body is also prepended by a `.text` line to indicate the start of the text segment.

# The runtime
We started with the `runtime-le.s` and `sysspim.s` files from the course webpage and made the follwoing changes to them to match the syntax mentioned in the SPIM documentation.

1. Change all the `j $ra` instrucitons to `jr $ra` in both files.
2. In the runtime file, we also changed all function calls to be `jal fnLabel` instead of the two instruction sequence of
`la	$t9,fnLabel
jal	$ra,$t9`

## Dual compatability with MARS and SPIM
We wanted dual compatability since MARS provides a better GUI interface, easier mechanism to add breakpoints and the ability to run code backwards. It is a whole IDE in which we can edit MIPS code instead of depending on another editor.

Functional differences between MARS and SPIM:
1. MARS starts execution at the start of the assembly file while SPIM starts execution with the `main` function. We moved the main function in our runtime to the start of the file and resolved this.
2. Since we use a static constant to keep track of the framesize instead of devoting a register for it, we need to add a line to set this constant. The syntax for this is different in MARS and SPIM.
   - MARS uses `.eqv fnLabel_framesize, value`
   - SPIM uses `fnLabel_framesize=value`
   We handle this using the mechanism mentioned in the section on procEntryExit3.

## Added stringLt and stringLte
The Tiger specification (Appendix, Pg 517, "String comparison") mentions that direct string inequality comparison is allowed and needs to be done lexicographically. However, there is no function in the runtime to do this, only for equality testing. We wrote our own assembly code to compute these two functions (inspired from the way Appel wrote his string equality function rather than the machine produced way of the runtime on our course webpage: http://www.cs.princeton.edu/~appel/modern/spim/).

## Added tig_exit_TigMain
We perform checks for dereferencing nil records as well as for array index being out of bounds. In the cases where such a thing happens, we call the `tig_exit_TigMain` function with either -1 or -2, depending on which error occured and then stop execution after printing an error message.

## Removed allocRecord
The allocRecord provided in the runtime behaves as a wannabe `calloc`, first calling `malloc` and then initializing all of the memory to be 0. However, we already wrote code for record initialization which calls malloc and then initializes the record to its intial value. The language specification (Pg 518 of the book) mentions that record creation requires intializing the fields of the record to some values. This means that we can always assume that there will be some value in there. If we make that assumption then the allocRecord in the runtime is wasting cycles initializing the memory to 0 when we are immediately going to set it to the correct value.

# Minor modifications/misc changes
We made the following minor improvements to the compiler.

## li and sub changed to addi
All instances of a `li` instruction of a number followed by a `sub`, have been changed to `addi` with the negative value of the number going into `li`.

## 0 framesize
In case the framesize is 0 (no escaping variables or spilled variables), we get rid of the instructions to allocate/deallocate the stack frame.

## String literals only allocated once
Due to an earlier optimization, if the same string occurs multiple times in the code, then we only create one label for it. This label is referred to every time that particular string needs to be accessed.

## Tree.CONST(0)
Whenever we see `Tree.CONST(0)` during instruction selection, use the `$zero` register for it instead of `li temp 0`.

## Simple constant folding (TODO look into div by 0)
Anytime there is an addition, multiplication, subtraction or division involving only constants, we replace that with the computed result. This gets rid of the instruction that computes that result.

# Things too difficult to change now
These are a few changes that we would have liked to make but at this stage they are either need changes to the design or look too difficult to do without the chance of introducing many bugs.

## Use $zero for temps that die immediately
This would be nice because that makes it clear to the reader that that value is not being used and the computation is done only for side-effects. This information is contained in the live out set which we no longer have access to when we actually allocate registers. We only have access to the interference graph and even a register which dies as soon as it is made interferes with anything that's live across that instruction and we can’t distinguish between the cases where they legitimately need to have that edge and when it’s there due to a dead temp.

## Space for first four arguments
We always leave space on the stack for the first four arguments. If any of those escape, then we do use that space. However, if we end up spilling one of the incoming arguments then we don't consider the space for the first four arguments to be available. It looks really difficult to figure out which temp corresponds to an original argument during register allocation but it would be nice to squeeze that space as well since we color the spilling slots anyway.

## Load FP into a register for prolog/epilog.
Each time callee saves register spills, we do the same calculation to put the FP into a register before saving/restoring the spilled register. It would be nice to do this once and keep that register throughout the prolog/epilog but this happens when we rewrite the program in the regalloc phase and since these are just like other spills, it is difficult know when to do this. There doesn't seem to be a simple solution for this as such.

## Too many labels for derefNil and outOfBounds
These labels are added by translate which doesn't have a concept of when a new proc starts. But these labels are needed for creating the CFG which is per proc. This poses a difficulty that doesn't seem to be easy to work around without major changes to the desing. We don't want to direclty jump to a label in the runtime to keep a level of abstraction and get to the runtime via externalCall, not directly jumping.
