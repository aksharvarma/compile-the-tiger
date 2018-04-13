# Assignments 4/5
## Team members
1. Caitlin Matuszak
2. Akshar Varma

# Changes since previous submission
* Simplified dummy code for procEntryExit1 since it was mostly useless anyways
* Changed Semant.transProg to return unit since we determined in later assignments that this was the most convenient thing to do, since the frags are held in an easily accessible ref in Translate anyways.
* Changed the way we handle error cases. Previously we were appending statements on to the main fragment. Now we have changed to adding a separate proc fragment for each error case. This makes the main fragment code much cleaner/less confusing.
* Replaced dummy implementation of notRel in Tree.sml with a real implementation since this will be used in the canonicalizer in the next step

# Previous description:
The static semantic analysis phase did type type checking using the Semant module. In the Frame analysis and Translation to IR assignments, this module is heavily augmented using various other modules to get frame analysis done as well as to translate AST into IR tree.

# High level description of new modules
- The FindEscape module is used to find which variables escape (are used by a function at a deeper scope).
- The MipsFrame module (with the FRAME signature) contains all machine specific details including how frames look like, where variables get stored, how static links are followed, how the final fragments produced by translation look like and so on.
- The Temp module defines the infinite supply of temporary registers that we assume we have until the register allocaiton phase is finished. It also provides us with a way to create new (potentially named) labels.
- The Tree module has the IR tree datatypes which is the final result we want out of the translation phase. These are exposed to Semant via the Translate module.
- The Translate module does all the heavylifting and interfacing, allowing Semant to interact with both the MipsFrame module (for frame analysis) and Tree (for AST to IR translation) module as needed.

# Modules' functionality in more detail:
(This whole section can be skipped.)
## FindEscape
  Is used to find which variables escape and to accordingly set the boolean ref corresponding to the variable. This information is used while allocating space on the frame. Uses the following three helper functions to do its job.
  * traverseDecs
    adds variables' escape ref and depth to an environment and recursively calls other functions as appropriate (eg. when traversing functions).
  * traverseVar
    changes the variable's ref value if accessed at a higher depth
  * traverseExp
    traverses expressions and calls the other functions as appropriate

## MipsFrame
  This is the concrete frame module having the FRAME signature as mentioned in the book. It is used to implement the following functionalities:
  * Abstracts out how variables need to be accessed (via temp registers or somewhere in the stack frame).
  * Keeps global copies of FramePointer and ReturnValue temp registers (FP and RV).
  * Abstracts out how the frame looks like. Translate makes blackbox calls to functions in this module to extract this functionality.
  * Has the fragments datatype which is used later to work with strings and function bodies.
  * Has the procEntryExit functions which are used to add prologue and epilogue codes to functions.

## Tree
  * This contains the datatypes that define the Intermediate Representation tree language.
  * The Semant module gets at this via the interface exposed by Translate, to translate the AST into the Tree language. This module doesn't have any functions, only the datatype definition.

## Temp
  * This defines the infinite supply of temporary registers that we assume we have until the register allocaiton phase is finished.
  * It also provides us with a way to create new (potentially named) labels. These are used for externalCalls and error labels.

## Translate
  * This module exposes an interface that allows Semant to translate ASTs into the IR tree.
  * It also provides Semant access to the Frame module with its interface.
  * Whereas Semant's primary function is to type-check, Translate helps it to translate the program on the side.


# Modularity of modules
The following diagram roughly represents the modularity of the new modules: which module interacts with which module.

                       ------ Temp --------
                      /        |           \
                     /         |            \
FindEscape <--> Semant <--> Translate <--> MipsFrame (as Frame)
                               ^
                               |
                               v
                              Tree


# Miscellaneous aspects of interest
## Primarily related to frame analysis
### Finding escaping variables
The FindEscape module traverses the AST and determines which variables are referenced at a depth greater than the one they were declared at. This is done using an escape environment which stores the depth at which variables were declared, along with the escape ref for that variable.

This traversal is using the algorithm mentioned in the book just before semantic analysis, as soon as transProg is called in Semant.

### Representation of frames and levels
#### Frames
Frames are represented as a record containing a name (label), a list of accesses associated with each of the formal parameters, and the number of escaping local variables that have been allocated in the frame so far. Storing the number of escaping local variables in the frame itself allows us to calculate the correct offset for the stack pointer (and the bottom of the frame) at any point in time. This is not including any space needed for caller-saves registers.

#### Levels
Levels are represented as either of the following:
* OUTERMOST, which represents the outermost level which is the parent of tiger program and the level at which the library functions are defined,
* Lev(frame, parent), containing the frame for that level and the parent level. In this way, there is exactly one frame associated with each level, and each level always knows its parent. This parent-child relationship stored in the levels is what enables us to easily handle static links.

### Handling static links
In order to handle static links, each frame needs to have access to the frame of it's immediately enclosing level (i.e. it's parent). By storing the parent level in each level, we are able to access the level of its parent, and it's grandparent and so on until we reach the desired level, at which point we can access the frame we need.

We deviate from the particular Tree expression that Appel provides in that we do not need intermediate offsets to compute the Static Link. We store the SL address at offset 0 from the frame pointer by taking it in as the first parameter of the function. This means that our code for following static links is shorter than what Appel provides.

### Augmenting the Semant and Env modules
The Env module has been augmented to store Translate.accesses and labels in the enventries for the type and variable environments as described in the chapter.

The Semant module has be augmented to call the translate module in appropriate places, including calling Translate.newLevel when a function declaration is found and by calling Translate.allocLocal when a new local variable declaration is found. Each expressions is also translated.

### The view shift
Most of the view shift needs knowledge of MIPS registers (knowing which registers arguments are passed in and so on). This means that we cannot finish implementing the view shift until a later stage where we have access to all the details of the Frame module. We have however made dummy functions wherever applicable and have written the code to implement whatever parts of the view shift that can be implemented at this stage. This is based on discussing with Prof. Shivers about the level of detail needed at this stage.

### Handling more than four procedure parameters
We allow arbitrary length list of parameters and we reserve space for all parameters although non-escaping parameters go into temp registers. Since we reserve space irrespective of the length of the param list or whether it escapes, we know the exact offset for each param. This allows us to handle all cases (with escaping and number of parameters changing) correctly.

The difference for more than 4 parameters will come in only when we implement the view shift. At that time we will need to consider where the arguments are coming in from. As mentioned above, we do not have enough information to implement the view shift (in particular we do not know which registers the arguments will come in). Since any other change that needs to be made is part of the view shift, as far as frame analysis and IR translation is concerned, we handle arbitrary number of parameters.

## Primarily related to translation
### allocLocal signature confusion
The signature that is provided for allocLocal in Chapter 6 is `allocLocal: frame -> bool -> access`. However, in the final signature for the Frame module provided in the book at pg 260, chapter 12, the allocLocal function has the signature: `allocLocal: frame -> int`. The currying is confusing because the only places we have had to call allocLocal are also where we immediately call the function that is returned. The currying doesn't seem to be necessary. However, we do need it to return an `access` type which isn't an int. Since we don't know why the final signature is different, we keep the signature provided in Chapter 6 and write the rest of the code accordingly.

### if-then-else handling and optimization
In order to handle if-then-else's in a smart way, there are several different cases that should be treated differently. The exp type that the entire if/then/else translates to is dependent on whether the if statement should be evaluated for control (Cx), value (Ex), or side effects (Nx). Determining how we want to evaluate the if/then/else then depends on the kinds of exps that are present in the then and else branches. The various cases are broken out and explained extensively in translate.
To highlight a few of the special cases:
* If both the then and else exp's are Nx's then we should evaluate the if/then/else as a statmement and produce an Nx.
* If both the then and else exp's are Cx's then we should evaluate the if/then/else for control and produce another Cx, optimizing the jumps logically.
* We optimize for the AND (Cx in then, 0 in else) and OR (1 in then, Cx in else) cases, specially, evaluating them for control and producing a Cx as this will be a common case for test expressions. Also note that unCx handles 0 and 1 specially and this is used in this case.
* If there is on Cx and one Ex (that is not 0 or 1), then we assume we want to evaluate the if/then/else for value, not control in order to preserve the value that is in the arbitrary expression. If we had evaluated for control and produced a Cx, then the value in the Ex would have been lost if we had actually needed to evaluate for value.
See the if/then/else function in translate for more details.

### String comparison
Since we have control over the runtime, we decided to make two functions (apart from stringEqual) that help us perform string inequality testing. These are the stringLt and stringLte functions that will be added to the runtime.c code at a later point. These new functions make the translated code much shorter and simpler.

### Predefined/Library functions
Since the predefined/library functions are written in the C runtime file, we make calls to these using the externalCall functionality similar to the initArray and string comparison functions. There is a small check that needs to be done for these since users can write their own functions with the same name as library functions. We allow such shadowing.

### procEntryExit1
The procEntryExit1 function inside MipsFrame is essentially a stub with dummy helper functions that will add the code mentioned in steps 4, 5 and 8 in the book (Chapter 7, page 167). We make these functions create labels so that the intended behaviour of the dummy functions is visible when printing the frags.

### STRING frags
Currently, the string frags don't do much. They are simple stored and when they need to be accessed, a reference to the correct label is also stored. Since the details of this are fleshed out at a much later stage, we believe this should suffice for now.


## Further miscellaneous details
### nil is 0
We make the nil (pointer) point to the address 0. This is keeping with the common C convention of making the NULL pointer point to 0. In Tiger, nil behaves somewhat as the NULL pointer in C, although it is only valid in the context of records.

### Dereferencing nil
We have code that checks before dereferencing records with nil pointers. This is a runtime check since records can be made to point to something other than nil by arbitrary code (conditionals, function calls, etc.) and that cannot be distinguished at compile time.

### Bound checking for arrays
We do perform bound checking for arrays by storing the size of the array at offset 0 from the pointer to the array. That is, the first element is the size of the array and the later elements are the elements. This allows us to compare indices to the size and jump to an error label in case there is an out of bounds access attempt.

### newLabel() for brkLabel of outermost level
Since transExp has been modified to take in a break label, we need to pass one in while transExp'ing the outermost level. Since a break in such a location (outside loops) is a type error, this label is a useless and the type-checker will prevent translatio to IR. Thus, we just use an arbitrary Temp.newLabel() for this purpose.

### Dropping values inside SeqExps
Similar to our earlier choice where we allowed multi-expression SeqExps to have non-unit expressions, we do the same while translating into the IR (they become Nx's to drop any values they may return).

### nop
Every time we wish to continue type-checking and need a Tree.exp to do so, we use a `nop` expression defined to be `Ex(T.CONST 0)` in Translate. This expression is also used for the empty sequence (albeit modified to be an Nx).

### While loop
Our while loops keep the test code at end, one of the version suggested by Prof. Shivers.

### For loop
We use the modification suggested in the book to allow the high value in for loop be equal to maxint. This means that our for loop includes two checks and looks a bit like the while loop translation that has two checks.

### No move to RV if procedure
Functions require that their return value be stored in the RV register. While we do this for functions, we optimize 1 instruction by not moving anything into the RV.

### Nxes are kept as Nxes wherever possible
We keep anything that has the UNIT return type (statements, not expressions) as Nxes. Which helps us assume that unNx'ing those things where we expect UNIT type statements, we will be using the trivial unNx'ing case and we'll not add bloating.
