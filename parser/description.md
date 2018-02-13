# Assignment 2
## Team members
1. Caitlin Matuszak
2. Akshar Varma

## How we handled shift/reduce conflicts
We are expecting 4 remaining shift/reduce conflicts for known and understood cases, and have chosen to leave them for the following reasons:
1. shift/reduce conflict (shift ELSE, reduce by rule 24)
   The standard dangling else conflict that comes with allowing if/then's and if/then/elses.
   Our implementation follows the same direction given in the textbook. We leave this conflict, because
   in order to get the intended behavior, we always want to shift in this case to read the else and its expression.
2. shift/reduce conflict (shift LBRACK, reduce by rule 34)
   This shift/reduce conflict stems from distinguishing between the following two cases in lvalue:
   * an ID by itself is a valid lvalue
   * ID [expression] is also a valid lvalue
   So after reading an ID, if we see an LBRACK, we want to wait before reducing to an lvalue, as the full lvalue may be of the form `foo[i].bar instead of just reducing `foo` as an lvalue.
   Thus, in order to get the intended behavior, we added a seemingly redundant rule to the lvalue non-terminal giving us both:
   * ID LBRACK expr RBRACK
   * lvalue LBRACK expr RBRACK 
   because otherwise there is a conflict in that it would not know which rule to use after reducing the ID to an lvalue. 
   Introducing the redundant rule creates the current shift/reduce conflict, but since we understand it and know that we always want to shift after reading an ID if the next token is an LBRACK, we have chosen to leave this shift/reduce conflict as it stands.
3. shift/reduce conflict (shift FUNCTION, reduce by rule 49)
   This is a known case occuring because in order to handle mutually recursive function declarations, we need to create a list of function 
   declarations in one FunctionDec. 
   Thus we encounter this shift/reduce conflict when trying to build the list of lists for the decs. 
   In this case, we also know that we always want to shift if the next token is FUNCTION, because if we are in this state, we know that 
   we are parsing a list of function declarations and want to parse all of the function declarations that occur in a row into the same
   FunctionDec. 
   Thus, if we are currently in this state parsing function declarations, we always want to shift to continue to read the 
   next declaration if the next token is FUNCTION. 
   Thus, the intended behavior occurs by shifting and we have chosen to leave this conflict.
4. shift/reduce conflict (shift TYPE, reduce by rule 47)
   This is the same conflict as 3, except with type declarations and the token TYPE instead of FUNCTION. 

All of these shift/reduce conflicts may have been able to be solved by hacking with precedence rules, 
but we found that doing so would not have been trivial or a logical use of the precendence and associativity operators, 
and rather than convolute the currently clean and logical precedence section to solve shift/reduce conflicts that are known, understood, and already produce the desired behavior,
we chose to leave the conflicts. 

## Anything you think is of interest about your parser
### Solved shift/reduce conflicts
* We solved a large number of shift/reduce conflicts by introducing the fake non-terminal LOWERTHANOP and using a precedence rule as follows.
* The shift/reduce conflicts occurred in the places where a rule for an expression ended in an expression. 
  This caused a large number of shift/reduce conflicts with all of the rules of the form "expr OP expr" because the parser did not know whether the expr at the end of the former rules should be reduced after reading the first expression or it should keep shifting to reduce "expr OP expr" to be the single expression at the end of that rule. 
* The rules that ended in an expression included, if/then, if/then/else, while loops, for loops, and array declarations. 
  These rules each had a shift/reduce conflict with each of the rules of the form "expr OP expr", creating somewhere around ~100 shift/reduct conflicts total from this same cause.
* Semantically, we decided it made the most sense in a language that doesn't require braces to parse the longest expression in this cases. 
  So for example, if we have while 2 < 3 do 4 + 3, we should parse this as while 2 < 3 do (4 + 3) rather than (while 2 < 3 do 4) + 3.
* To fix this, we chose to exploit precedence rules due to the sheer number of shift/reduce conflicts this case created. 
  To do this, we created the fake terminal LOWERTHANOP and added it to the precedence section at the top (with a lower precedence than any of the operator terminals).
  Then in each of the cases stated above where the rule ended in an expr, we added the %prec directive to give the rule a lower precedence than that of the operator rule.
  Thus, the compiler then knew to shift to continue to parse the longest expression with operators possible after one of those given cases.

### Error repair
* We included some of the basic error repair mechanisms discussed in the book such as preferring common characters to insert such as semicolons or parens as well as some common change cases as well. These are documented in the parser when we declare them.
* These repair mechanisms are not very sophisticated and have many limitations. In particular, the tokens it chooses to insert frequently do not make any semantic sense because the error repair doesn't take types into account and simply inserts a token that will allow it to get to the end of the file. For example, it may frequently insert a MINUS before things because a MINUS expr is always parsed as a valid expression, even if the thing following it isn't semantically a number. 

### Error reporting
* Error reporting for the parser uses the same error reporting mechanism as the lexer: ErrorMsg.error. This error function is passed in to the parser in the Parse.parse function in the driver file given in the starter files. From there, whatever parser file is generated automatically calls this error function when it finds an error. 
* The locations of the errors should be correct, and should indicate the position at which the parser tried to repair the error if it was able to successfully do so. 
* If the parser is able to repair the error and continue to the end of the file, then the errors are printed, but no exception is thrown.
* If the parser was not able to successfully repair the error enough to reach the end of the file, then a ParseError exception is thrown.

### Positions stored in the AST
* When deciding which position to store for various expressions in the AST, we chose to store the left position of the leftmost terminal in the rule.
* For example, for a rule of the form "expr OP expr" we store OPleft in the AST, and for a rule of the form "WHILE expr DO expr" we store WHILEleft in the AST.
* Doing this seemed to make logical sense in most cases and continuing to have a standard rule for assigning position in the AST seemed logical. 

### Parsing lists of arbitrary length
* When parsing lists of arbitrary length, our parser always uses left recursion, because as discussed in class, this uses constant space on the stack to build up the list when parsing.
* However, this meant that when we are building up the list for the AST in the semantic action, we have the opposite case in which we have the inefficient operation of appending the current list onto a list of the new single element. 
* Fixing this would require us to change to right recursion while parsing, so this seemed like a tradeoff between efficiency in the parse and efficiency in the semantic action. We chose to continue to use left recursion to optimize our parsing. 

### Miscellaneous Notes
* We made some minor adjustments to our lexer and have included it in the submission so it can be used when running the parser.
* We changed the name of the string -> symbol function in symbol.sml from symbol to symbolize because we were having trouble getting the function symbol instead of the type symbol, and frankly naming them the same thing was just confusing to begin with.
