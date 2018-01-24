# Assignment 1
## Team members
1. Caitlin Matuszak
2. Akshar Varma

## How we handled comments
Nested comments are handled by creating a new state "COMMENT" and using the start states feature (and the YYBEGIN function) of ML-Lex, along with a counter to keep track of the depth of comment nesting.
A variable `commentNesting` is initialized to 0. Whenever a new comment starts, the value of this variable is incremented, and when a comment ends, the variable value is decremented.
By specifying the start states these rules are applicable for, we ensure that this only occurs when exiting and entering a comment. That is, new comments can start anywhere except within a string and comments can only end if we are already inside a comment.

The ability to count outside of the rules enables nested comments to be recognized, which regular expressions alone cannot do.

## How we handled errors
Whenever an error occurs, we add an entry to an `errorList` list variable (defined in the `ErrorMsg` module). These errors are then printed out at the end of lexing, using the `error` function from the `ErrorMsg` module (slightly modified by us). Every error prints the line number and column offsets of the start and end of the input that caused the error along with a message.

There are four errors that we handle during lexing. These errors would cause invalid tokens to be sent to the parser. Since these errors can be detected by the lexer, we report these in the lexer itself.
1. Illegal escape characters inside a string literal. (see later section for details)
2. Unclosed string literal at EOF.
3. Unclosed comment at EOF.
4. Illegal characters in the code (eg: $, ^, ~, etc.).

Our lexer ignores the parts of the code that are illegal escapes in strings or illegal characters and continues lexing after adding those as errors to the `errorList` variable. This means that we don't stop lexing at the first error but try to report all errors as well as possible.

When we have reached the end of the file, if any errors were found, we print them all, and then throw an exception. If there are errors in lexing, there is no use proceeding to the parser, because the compiler already knows there were errors. To help the user as much as possible, we print all errors found in lexing rather than just stopping after the first.

## How you handled end-of-file
Two things need to be handled at EOF, unclosed strings and open comments. Both of these are done via variables that keep track of whether we are inside a comment/string. When the `eof()` function is called, these variable values are checked to determine if we are still inside a comment/string when the file ended. If that is the case then those errors are reported and printed along with any other errors found.
If errors were found, we print them and throw an exception. If no errors were found, then we produce the EOF token and end.

## Anything else you think is of interest about your lexer
* Identifiers cannot start with underscores or numbers, only letters. This is due to a strict reading of the description in the Appendix of Appel's book. The book lists underscores separate from letters and says that identifiers can only start with a letter.

* Some changes were made to the `ErrorMsg` module to make it print errors in a format closer to what SML prints.

* The `initPos` variable in the generated `tiger.lex.sml` file is manually changed from 2 to 1 after the file has been generated to account for the ML-Lex bug that is present in the backwards compatibility mode of `ml-ulex`.

* For error reporting, the line numbers and column offsets are indexed by 1, i.e. the first character in the file is at position "1.1". Similarly, the offsets stored in the tokens are 1 indexed, i.e. if the first token starts at the first character in the file, its start position will be stored as 1. In the printing of the errors, the start position is inclusive, and the end position is exclusive, i.e. if the first character of the file is an invalid token, the error message will print the positions as "1.1--1.2".

### Handling of strings
We made the following decisions regarding handling of strings, based on whatever description was available in Appel's book.

* A start state called `SKIPSTRING` was used to skip any whitespace between two backslashes. This was needed to ensure that the line counting machinery functioned properly even within a string.

* Only the [control escape sequences](https://en.wikipedia.org/wiki/ASCII#ASCII_control_characters) defined in the table in the linked Wikipedia page are allowed. Other than these, the escape characters like newline, tab, etc. that are listed explicitly in the book are allowed. Any other escape is considered illegal and is an error.

* Similarly, any non-printable character (ASCII code less than 32) in a string are also considered errors.

The last two decisions were made with the aim of passing only valid tokens to the parser. Anything the lexer can determine to be invalid, the lexer reports as an error. This is in line with the philosophy of using the simplest tool to do a job. Reporting these as errors ensures that the parser (or later components of the compiler) can depend on the internal integrity of tokens. If the lexer cannot determine correctness in the general case, then the decision is deferred to a later component of the compiler. This includes all things requiring understanding the relation between tokens.
