# Assignment 2
## Team members
1. Caitlin Matuszak
2. Akshar Varma

## How we handled shift/reduce conflicts

## Make sure to comment ambiguity resolutions (in the code)


The following SR conflict is okay because if there is a LBRACK, then we should be shifting and reading it in.

* shift/reduce conflict (shift ELSE, reduce by rule 24)
  The standard dangling else conflict.
* shift/reduce conflict (shift LBRACK, reduce by rule 34)
  After reading an ID, if we see an LBRACK, we want to wait before reducing to an lvalue, as the full lvalue may be of the form `foo[i].bar` instead of just reducing `foo` as an lvalue.
* shift/reduce conflict (shift FUNCTION, reduce by rule 49)
  shift/reduce conflict (shift TYPE, reduce by rule 47)
  These are known cases both occuring due to having to handle mutually recursive types/function declarations. For example, while building a list of declarations, mutually recursively function declarations might need to be parsed together to make sense. 

## Anything you think is of interest about your parser


* Error numbers are off by 1 in some cases. No clue why.

# ADD COMMENTS TO THE FILES.

# FINISH SELF.
