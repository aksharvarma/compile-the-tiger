# Assignment 2
## Team members
1. Caitlin Matuszak
2. Akshar Varma

## Resolution of unspecified details
The reference for Tiger leaves some aspects of the language unspecified. We choose to resolve them in the following manner.

### Valueless expressions
The type of a if-then, while and for expressions are enforced to be UNIT because the reference says that they return no value. No value is taken to mean a UNIT type.

### SeqExp
In a sequence expression, we **do not** chose to enforce that types of all expressions except the last one be UNIT. The type of the whole sequence is the type of the last expression. This is based on `test20.tig` in the testcases folder which has an expression `i+1` which would return an int (the error in the file is because `i` is not defined, not because non-last expression in a SeqExp has a non-UNIT type). 

### Assigning to loop variable inside FOR
The language reference specifies that the loop variable over which the FOR loop runs cannot be assigned to inside the loop. We implement this by extending the `types` module to include a UNASSIGNABLE type which is a subtype of INT. This new type is used only for the loop variable of a for expression. 

### Use of `break` as a valid type
We allow the break statement to be used instead of any type. This is done by using the BOTTOM type to be the type of the break statement. 

### Scoping of `break`
We keep track of the level of nesting of loops in a variable and allow breaks only if the nesting level is positive (we are in a loop). We allow an arbitrary number of breaks in a scope where breaks can occur (these pass in terms of type-checking, and then while evaluating, they'll simply break out of the loop on the first occurence of break).

### "Procedures" do not exist
The reference allows for the existence of procedures which do not return any result value. We take this to mean that such a function necessarily returns UNIT. Thus, if a function does not explicitly mention a result type, then it has to return unit. Thus we do not really make a distinction between functions and procedures. Procedures are simply functions that return UNIT.

### Duplicate names inside mutually recursive declarations
We do not allow duplicate type/function names within a mutually recursive set of declarations. A tydec/fundec list can contain names that were already defined earlier and the new ones will shadow the older definitions but there can be no duplicates within a single tydec or fundec list.

### Use of Ty.BOTTOM as the default type.
We use the Ty.BOTTOM type (originally meant for break) as the default "error" type. Whenever we see an error, we assume that the type returned was BOTTOM and continue type checking (unless it should have returned a known value, for example arithmetic will always return INT). This helps to type-check more of the file meaningfully before it starts to generate garbage error messages.

### The type lattice
The types.sml file has a function called isSubtype which is used to enforce the lattice structure of the types. The only real difference from the version mentioned in class is to add the UNASSIGNABLE type, a subtype of INT for the loop variable.

