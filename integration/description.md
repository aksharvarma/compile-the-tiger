# Assignment 9
## Team members
1. Caitlin Matuszak
2. Akshar Varma

# Integration phase
Fixes any inconsistencies through the different components and phases to get a working compiler for the Tiger language. 

Note: To get an executable piece of assembly code, the compiler needs to be run slightly differently (look inside `main.sml` or in the first section of this file).

After the section on logistics, we mention the changes needed to fix gaps in our compiler to get the final assembly output running. Then we mention modifications we made to the runtime, followed by some minor optimizations we added to our compiler. Finally, we mention a few design choices about our compiler that would have been nice to be able to change but which seem too diffcult to actually change at this stage.

# Running the compiler
## Changes to main.sml
`Main.compile` still works as before but since it doesn't concatenate the runtime to the assembly code the resulting code is not executable. However, it is much more readable. Use `Main.compileToExecutable` to compile a file and concatenate it with a runtime. We changed the filenames used so that `.swor` denotes assembly code without the runtime concatenated while `.s` files are executable.

## CompileAll and our testcases
We are also attaching a script which compiles every file in a folder. The `our-testcases` folder contains small pieces of code that illustrate some of the optimizations that we did.

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
  - Sets a constant equal to the framesize used by the function, calculated by counting the number of locals that are needed. This includes escaping variables as well as anything that the registor allocator had to spill.
  - Allocates stack space by moving the stack pointer.
* Epilog: 
  - Adds the code for deallocating stack space.
  - Instruction for jumping out of the function: `jr $ra`

Our runtime is compatible with both the MARS and SPIM emulators. The only place in our compiler that needs to change for this is when we set the constant as the emulators use different syntax for setting constants. The instruction string to set a constant equal to the framesize is done differently depending on the emulator that is used. The compiler is then run using the variable for the emulator that we are interested in running on.

### Strings
The runtime for Tiger assumes that strings are prefixed with a word containing the length of the string.

This is done by putting code like this:
strLabel:
    .word strLength
    .ascii str

### .text and .data segments
Apart from what has been mentioned for strings, we need the following code to denote any data segment content.
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

Note: Ironically, the SPIM emulator didn't through any errors on such syntax despite it being different from what is mentioned in the SPIM documentation. The MARS emulator did through errors on instructions like `j $ra` and `jal $ra, $t9`.

3. Remove `.align` directives present inside text segments in the runtime file.
4. Change `slt` and `sltu` instructions to `slti` and `sltiu` respectively because they were using immediates as arguments.

## Dual compatability with MARS and SPIM
We wanted dual compatability since MARS provides a better GUI interface, easier mechanism to add breakpoints and the ability to run code backwards. It is a whole IDE in which we can edit MIPS code instead of depending on another editor.

Functional differences between MARS and SPIM:
1. MARS starts execution at the start of the assembly file while SPIM starts execution with the `main` function. We moved the main function in our runtime to the start of the file and resolved this.
2. Since we use a static constant to keep track of the framesize instead of devoting a register for it, we need to add a line to set this constant. The syntax for this is different in MARS and SPIM.
   - MARS uses `.eqv fnLabel_framesize, value`
   - SPIM uses `fnLabel_framesize=value`
   We do this using the mechanism mentioned in the section on procEntryExit3.

## Added stringLt and stringLte
The Tiger specification (Appendix, Pg 517, "String comparison") mentions that direct string inequality comparison is allowed and needs to be done lexicographically. However, the runtime only has a function for doing string equality testing, not inequality testing. We wrote our own assembly code to compute these two functions (inspired from the way Appel wrote his [string equality function](http://www.cs.princeton.edu/~appel/modern/spim/runtime.s) rather than the machine produced way of the runtime on our course webpage).

## Added tig_exit_TigMain
We perform checks for dereferencing nil records as well as for array index being out of bounds. In the cases where such a thing happens, we call the `tig_exit_TigMain` function with either -1 or -2, depending on which error occured and then stop execution after printing an error message.

## Removed allocRecord
The allocRecord provided in the runtime behaves as a wannabe `calloc`, first calling `malloc` and then initializing all of the memory to be 0. However, we already wrote code for record initialization which calls malloc and then initializes the record to its intial value. The language specification (Pg 518 of the book) mentions that record creation requires intializing the fields of the record to some values. This means that we can always assume that there will be some value in there. If we make that assumption then the allocRecord in the runtime is wasting cycles initializing the memory to 0 when we are immediately going to set it to the correct value. So we remove the allocRecord function from the runtime we use.

## Static Link caveat
Looking in `runtime.c` it can be seen that the main function of our code is called with 0 as the "static link". This means that the $a0 register always escapes and has to be saved to the frame even though it is never used because our code doesn't use any variables from the runtime. 

# Minor modifications/misc changes
We made the following minor improvements to the compiler.

## li and sub changed to addi (instruction selection)
All instances of a `li` instruction of a number followed by a `sub`, have been changed to `addi` with the negative value of the number going into `li`.

## 0 framesize
In case the framesize is 0 (no escaping variables or spilled variables), we get rid of the instructions to allocate/deallocate the stack frame.

## String literals only allocated once
Due to an earlier optimization, if the same string occurs multiple times in the code, then we only create one label for it. This label is referred to every time that particular string needs to be accessed.

## Tree.CONST(0)
Whenever we see `Tree.CONST(0)` during instruction selection, use the `$zero` register for it instead of `li temp 0`.

## Simple constant folding
Anytime there is an addition, multiplication, subtraction or division involving only constants, we replace that with the computed result. This gets rid of the instruction that computes that result. This optimization is not done if we are dividing by 0. We assume that if the code divides by a literal 0, then the programmer put it in deliberately.

We had resubmitted an earlier phase of the compiler with this optimization added but without checking for division by 0. This would result in a bug in our compiler: good tiger code won't compile. That has been fixed.

# Things too difficult to change now
These are a few changes that we would have liked to make but at this stage they either need changes to the design or look as if it'll be too difficult to do without introducing many bugs.

## Too many labels for derefNil and outOfBounds
Whenever we check for dereferencing of nil records or array out of bound errors, we jump to a label if there is an error and then call an error function. These labels are added by translate which doesn't have a concept of when a new proc/function starts. The labels are then used for creating the CFG which is per proc/function. Translate can only keep track of a global label, not one label per proc/function. But without a label per proc/function the CFG construction wouldn't work out. This poses a difficulty that doesn't seem to be easy to work around without major changes to the design. We also want to keep a level of abstraction and avoid directly jumping to a label in the runtime. Accessing the runtime should be done via the externalCall framework and directly jumping seems too hacky.

## Load FP into a register for prolog/epilog.
Each time callee saves register spills, we do the same calculation to put the FP into a register before saving/restoring the spilled register. It would be nice to do this once and keep that register throughout the prolog/epilog but this happens when we rewrite the program in the regalloc phase and since these are just like other spills, it is difficult know when to do this. There doesn't seem to be a simple solution for this as such.

## Space for first four arguments
We always leave space on the stack for the first four arguments. If any of those escape, then we do use that space. However, if we end up spilling one of the incoming arguments then we don't consider the space for the first four arguments to be available. With the current design, it is difficult to figure out during register allocation which spilling temp corresponded to an incoming argument. If we could, then we could also use those slots/colors while coloring the spilled temps.

## Use $zero for temps that die immediately
This would be nice because that makes it clear to the reader that that value is not being used and the computation is done only for side-effects. This information is contained in the live out set which we no longer have access to when we actually allocate registers. We only have access to the interference graph and even a register which dies as soon as it is made interferes with anything that's live across that instruction and we can’t distinguish between the cases where they legitimately need to have that edge and when it’s there despite a dead temp.
