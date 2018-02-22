# Assignment 2
## Team members
1. Caitlin Matuszak
2. Akshar Varma

## Resolution of unspecified details
The reference for Tiger leaves some aspects of the language unspecified. We choose to resolve them in the following manner.

### SeqExp
In a sequence expression, we chose to enforce that types of all expressions except the last one be UNIT. The type of the whole sequence is the type of the last expression.

### While
The type of a while expression is enforced to be UNIT.

### Assigning to loop variable inside FOR
The language reference specifies that the loop variable over which the FOR loop runs cannot be assigned to inside the loop. We implement this by extending the `types` module to include a UNASSIGNABLE type which is a subtype of INT. This new type is used for the loop variable of for. 

### [CHECK] Duplicate names inside mutually recursive types
[Doing this backwards. Rectify.]
We allow duplicate names to be defined in mutually recursive types but the one that is defined later will be used.

### Use of `break` as a valid type
We allow the break statement to be used instead of any type. This is done by using the BOTTOM type to be the type of the break statement. 

### "Procedures" do not exist
The reference allows for the existence of procedures which do not return any result value. We take this to mean that such a function necessarily returns UNIT. Thus, if a function does not explicitly mention a result type, then it has to return unit.

### Use bottom for "error" type.

## Interesting things about our code


# TODO
* ~~Check Array and Record types (using respective expressions)~~
* ~~Make loop variable of FOR unassignable~~
* ~~Handle mutually recursive function decs~~
* ~~If \exists error, blow up at the end.
	Make another function to set a bool to true before calling E.error~~
* Make sure that breaks only occur within loops.
* Duplicate names in mutually recursive decs
	Could still use typeOfOp but remove ugly looking lists and inList
* Look at the things related to NIL mentioned in Appel.
* Clean up error messages. Make them more informative.
* Change OpExp checking to something neater.


* Use option instead of returning error-like/invalid things.
