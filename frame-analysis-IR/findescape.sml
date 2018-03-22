(* This module is used for escape analysis.
 *
 * It is called by Semant's transProg and walks through the AST to look
 * at variables and see which ones are called from a different frame.
 * The boolen escape ref inside those variables' VarEntry is made true
 * so that Semant can perform the necessary frame analysis properly.
 * 
 * Other modules only see the function 
 *     findEscape: Absyn.exp -> unit
 * which sets all bool refs for variables in the AST to correct values
 * This is done in place (mutates) the refs inside VarEntries so that
 * the Semant and Translate modules can use them for frame analysis.
 *
 * There are three helper functions that findEscape uses:
 * - traverseVar: escEnv * depth * Absyn.var -> unit
 * - traverseExp: escEnv * depth * Absyn.exp -> unit
 * - traverseDecs: escEnv * depth * Absyn.dec list -> unit
 *)

structure FindEscape: sig val findEscape: Absyn.exp -> unit end =
struct

(* Outermost level is depth 0, a function inside current level will be
 * one level deeper (currLevel+1)
 *)
type depth = int

(* This table keeps track of the depth at which a variable was defined
 * and sets its escape bool ref to true if the variable is accessed at
 * a deeper level.
 *)
type escEnv = (depth * bool ref) Symbol.table

(* The three helper functions *)

(* traverseVar: escEnv * depth * Absyn.var -> unit
 * 
 * traverses the variables in the AST, setting the escapes to true when
 * a simple var is found referenced deeper than where it was declared.
 * Other variables make recursive calls.
 *)

(* Simple variable case. Setting escape to true can only happen here  
 *)
fun traverseVar(env:escEnv, depth:depth, Absyn.SimpleVar(id, pos)): unit =
    (case Symbol.look(env, id)
      of NONE => () (* type error reported by Semant *)
       | SOME(d, esc) => if (depth > d)
                              (* escapes if current depth > depth *)
                         then esc := true
                         else ())
  (* Recursively traverse the record var whose field accessed.
   * Fields of a record cannot escape, so we can ignore those
   *)
  | traverseVar(env, d, Absyn.FieldVar(var, id, pos)): unit =
    traverseVar(env, d, var)

  (* Recursively traverse the subscript expression
   * and the array var whose subscript was accessed. 
   *)
  | traverseVar(env, d, Absyn.SubscriptVar(var, e, pos)): unit =
    (traverseExp(env, d, e);
     traverseVar(env, d, var))

(* traverseExp: escEnv * depth * Absyn.exp -> unit
 *
 * Recursively traverses the exp's in the AST and make calls to the
 * traverseVar and traverseDec when needed. 
 * No modifications to the escape field are made by this function.
 *)

(* Traverse the variables by calling traverseVar *)
and traverseExp(env:escEnv, d:depth, Absyn.VarExp(var)): unit =
    traverseVar(env, d, var)

  (* Traverse the body of the let expression in the escape environment
   * augmented by traversing the declarations
   *)
  | traverseExp(env, d, Absyn.LetExp({decs, body, pos})) =
    traverseExp(traverseDecs(env, d, decs), d, body)

  (* Apply traverseExp to each function argument expression *)
  | traverseExp(env, d, Absyn.CallExp({func, args, pos})) =
    app (fn (a) => traverseExp(env, d, a)) args

  (* Recursively traverse the left and right expressions in a binOp *)
  | traverseExp(env, d, Absyn.OpExp({left, oper, right, pos})) =
    (traverseExp(env, d, left);
     traverseExp(env, d, right))

  (* Apply traverseExp to each field expression *)
  | traverseExp(env, d, Absyn.RecordExp({fields, typ, pos})) =
    app (fn (sym, exp, pos) => traverseExp(env, d, exp)) fields

  (* Apply traverseExp to each expression in the sequence *)
  | traverseExp(env, d, Absyn.SeqExp(exps)) =
    app (fn (e, pos) => traverseExp(env, d, e)) exps

  (* Traverse the var on the LHS and the expression on the RHS *)
  | traverseExp(env, d, Absyn.AssignExp({var, exp, pos})) =
    (traverseVar(env, d, var); traverseExp(env, d, exp))

  (* Traverse the test and then' expressions and if present, else' *)
  | traverseExp(env, d, Absyn.IfExp({test, then', else', pos})) =
    (traverseExp(env, d, test);
     traverseExp(env, d, then');
     case else'
      of NONE => ()
       | SOME(e) => traverseExp(env, d, e))

  (* Traverse the test and body expressions of the while loop *)
  | traverseExp(env, d, Absyn.WhileExp({test, body, pos})) =
    (traverseExp(env, d, test);
     traverseExp(env, d, body))

  (* traverse the lo and hi expressions in the current escEnv,
   * then traverse the loop's body in the escape environment
   * augmented with the loop variable 
   *)
  | traverseExp(env, d, Absyn.ForExp({var, escape, lo, hi, body, pos})) =
    (traverseExp(env, d, lo);
     traverseExp(env, d, hi);
     traverseExp(Symbol.enter(env, var, (d, escape)), d, body))

  (* traverse the size and init expressions of the array *)
  | traverseExp(env, d, Absyn.ArrayExp({typ, size, init, pos})) =
    (traverseExp(env, d, size);
     traverseExp(env, d, init))

  (* Skip other expressions because depth can't increase *)
  | traverseExp(env, d, _) = ()


(* traverseDecs: escEnv * depth * Absyn.dec list -> unit
 * 
 * Recursively traverses the given decs list
 * This is the main function that adds relevant escape refs to the
 * escape environment at the current depth level
 *)
and traverseDecs(env, d, []): escEnv = env (* base case *)

  (* The other cases, handle first dec and recurse over the rest *)
  | traverseDecs(env, d, dec::decs: Absyn.dec list): escEnv =
    (* Vars, Funcs, Types behave differently, case over them *)
    case dec of                 
        (* Variables
         *
         * Traverse the init expression and augment escEnv with the 
         * escape field of the current vardec.
         * Recursively continue parsing the decs list with new escEnv
         *)
        Absyn.VarDec({name, escape, typ, init, pos}) =>
        (traverseExp(env, d, init);
         traverseDecs(Symbol.enter(env, name, (d, escape)), d, decs))

      (* Typedec 
       * 
       * They can't refer to anything that escapes. Just recurse. *)
      | Absyn.TypeDec(tylist) => traverseDecs(env, d, decs)

      (* FunctionDecs
       * 
       * They are done one by one, each function adds 1 to the depth.
       * We then traverse the body with incremented depth.
       *)
      (* Recur if end of Fundec list *)
      | Absyn.FunctionDec([]) => traverseDecs(env, d, decs)

      (* Each function is done individually. 
       * Params are added to escEnv and then the body is traversed *)
      | Absyn.FunctionDec({name, params, result, body, pos}::fs) =>
        let
          (* Add the escape field of given param at (depth + 1) *)
          fun addEscape({name, escape, typ, pos}, env) =
              Symbol.enter(env, name, (d + 1, escape))

          (* Augment escEnv with escape refs of all function params *)
          val resultEnv = foldr addEscape env params
        in
          (* Traverse the body expression of the function
           *  using d+1 because we are in a new frame (function)
           *)
          (traverseExp(resultEnv, d + 1, body);

           (* The rest of the funDecs are recursed over.
            * Use depth d and the old escEnv, we're out of the frame *)
           traverseDecs(env, d, Absyn.FunctionDec(fs)::decs))
        end

(* Main function that calls traverseExp on the AST *)
fun findEscape(prog: Absyn.exp): unit =
    traverseExp(Symbol.empty, 0, prog)
end
