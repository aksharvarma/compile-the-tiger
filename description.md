# Assignment 1
## Team members
1. Caitlin Matuszak
2. Akshar Varma

## Comment handling
Nested comments are handled using the start states feature (and the YYBEGIN function) of ML-Lex along with a counter keeping track of the depth of comment nesting. A variable `commentNesting` is initialized to 0 and whenever a new comment starts, the value of this variable is incremented and when a comment ends, the variable value is decremented. The use of start states ensures that these matches are valid. That is, new comments can start anywhere except within a string and comments can only end if we are already inside a comment.

The ability to count enables nested comments to be recognized which is otherwise not possible with regular expressions alone.

## How you handled errors
Whenever an error occurs, we add an entry to an `errorList` variable (defined in the `ErrorMsg` module). These errors are then printed out at EOF using a slightly modified `error` function from the `ErrorMsg` module. It prints the start and end position (line number+column offset) of the relevant part of the code that caused the error.

There are four errors that we handle during lexing. These errors would cause invalid tokens to be sent to the parser. Since these errors can be detected by the lexer, we report these in the lexer itself.
1. Illegal escape characters inside a string literal. (see later section for details)
2. Unclosed string literal at EOF.
3. Unclosed comment at EOF.
4. Illegal characters in the code (eg: $, ^, ~, etc.).

## How you handled end-of-file
Two things need to be handled at EOF, unclosed strings and open comments. Both of these are done via variables that keep track of whether we are inside a comment/string. When the `eof()` function is called, these variable values are checked to determine if one of these errors is present. If so, then those errors are reported and printed along with any other errors found.

## Anything else you think is of interest about your lexer 
* Some changes were made to the `ErrorMsg` module to make it print errors in a format closer to what SML prints.

* Identifiers cannot start with underscores or numbers, only letters. This is due to a strict reading of the description in the Appendix of Appel's book.

### Handling of strings
We made the following decisions regarding handling of strings, due to lack of preciseness in the language description in Appel's book.

* A start state called `SKIPSTRING` was used to skip any whitespace between two backslashes. This was needed to ensure that the line counting machinery functioned properly even within a string.

* Only the [control escape sequences](https://en.wikipedia.org/wiki/ASCII#ASCII_control_characters) defined in the table in the linked Wikipedia page are allowed. Other than these, the escape characters like newline, tab, etc. that are listed explicitly in the book are allowed. Any other escape is considered illegal and is an error.

* Similarly, any non-printable character (ASCII code less than 32) are also considered errors inside a string.

The last two decisions were made with the aim of passing only valid tokens to the parser. Anything the lexer can determine to be invalid, the lexer reports as an error. This is in line with the philosophy of using the simplest tool to do a job. Reporting these as errors ensures that the lexer passes on tokens that are valid to the best of it's knowledge. If the lexer cannot determine correctness in the general case, then the decision is deferred to a later component of the compiler.
