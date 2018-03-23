(* This contains the IR tree provided.
 *
 * There are some binops and relops that are not used by us that we are
 * not removing because they might be useful later on.
 *
 * Further, there are two functions not defined and not needed yet.
 * They have been commented out from the signature.
 *)
signature TREE =
sig
  type label = Temp.label
  type size

  datatype stm = SEQ of stm * stm
               | LABEL of label
               | JUMP of exp * label list
               | CJUMP of relop * exp * exp * label * label
               | MOVE of exp * exp
               | EXP of exp

       and exp = BINOP of binop * exp * exp
               | MEM of exp
               | TEMP of Temp.temp
               | ESEQ of stm * exp
               | NAME of label
               | CONST of int
               | CALL of exp * exp list

       and binop = PLUS | MINUS | MUL | DIV
                   | AND | OR | LSHIFT | RSHIFT | ARSHIFT | XOR

       and relop = EQ | NE | LT | GT | LE | GE
                   | ULT | ULE | UGT | UGE

(* The following two functions are absent from the structure below.
 * Commented out for now although the canonicalizer seems to need them
 *)
(* val notRel : relop -> relop *)
(* val commute: relop -> relop *)
end

structure Tree : TREE =
struct
type label=Temp.label
type size = int

datatype stm = SEQ of stm * stm
             | LABEL of label
             | JUMP of exp * label list
             | CJUMP of relop * exp * exp * label * label
             | MOVE of exp * exp
             | EXP of exp

     and exp = BINOP of binop * exp * exp
             | MEM of exp
             | TEMP of Temp.temp
             | ESEQ of stm * exp
             | NAME of label
             | CONST of int
             | CALL of exp * exp list

     and binop = PLUS | MINUS | MUL | DIV
                 | AND | OR | LSHIFT | RSHIFT | ARSHIFT | XOR

     and relop = EQ | NE | LT | GT | LE | GE
                 | ULT | ULE | UGT | UGE

end

