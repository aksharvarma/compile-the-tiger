# Assignments 4/5
## Team members
1. Caitlin Matuszak
2. Akshar Varma

## Representation of frames and levels
* Frames are represented as a record containing a name (label), a list of accesses associated with each of the formal parameters, and the number of local variables that have been allocated in the frame so far. Storing the number of local variables in the frame itself allows us to calculate the correct offset for the stack pointer (and the bottom of the frame) at any point in time.
* Levels are represented as either OUTERMOST, which represents the outermost level which is the parent of tiger programs and the level at which the library functions are defined, or Lev(frame, parent), containing the frame for that level and the parent level. In this way, there is exactly one frame associated with each level, and each level always knows its parent. This parent-child relationship stored in the levels is what will enable us to easily handle static links.

## Handling static links
* In order to handle static links, each frame needs to have access to the frame of it's immediately enclosing level (i.e. it's parent). By storing the parent level in each level,we will be able to access the level of its parent, and it's parent and so on until we reach the desired level, at which point we can access the frame we need.

## Finding escaping variables
* the FindEscape module traverses the AST and determines which variables are referenced at a depth greater than the one they were declared at. This is done using an escape environment which stores the depth at which variables were declared, along with the escape ref for that variable. This is done using the algorithm describing in the textbook.
* This traversal is done before semantic analysis, immediately when transProg is called in Semant.

## Augmenting the Semant and Env modules
* The Env module has been augmented to store Translate.accesses and labels in the enventries for the type and variable environments as described in the chapter.
* The Semant module has be augmented to call the translate module in appropriate places, including calling Translate.newLevel when a function declaration is found and by calling Translate.allocLocal when a new local variable declaration is found

## Handling more than four procedure parameters
* Space is reserved in the frame for all formal parameters. This is ensured by always incrementing the index used to make the offset calculations in either case where the formal parameter will be stored in the frame or a register.
* TODO: REVISIT WHEN IMPLEMENTED: We assume that if there are less or equal to four formal parameters then they will be passed to the function in registers r4-r7. When writing the instructions for the view shift, we move the first four arguments from r4-r7 to their correct places (either a new temp or a place on the frame). If one of the first four arguments escapes then it will be placed on the frame in the appropriate offset above the new frame's frame pointer (there should be room for it because space in the frame is reserved for all formal parameters). If they don't escape then they're simply moved to a new temp.
* For the excess arguments after the fourth one, they should already be located in a known offset from the frame pointer, and we can assign them appropriate accesses as normal.

## The view shift
* TO BE IMPLEMENTED
