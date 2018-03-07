(* Finds and marks all variables that escape in the given AST *)
structure FindEscape: sig val findEscape: Absyn.exp -> unit end =
struct
    type depth = int

    (* the escape environment is a table mapping id's to a tuple of the depth at
       which they were declared and the ref for the escape field of that variable *)
    type escEnv = (depth * bool ref) Symbol.table

    (* traverses the var's in the AST, setting the escapes to true when
       a simple var is found referenced deeper than the depth they were declared at *)
    fun traverseVar(env:escEnv, depth:depth, Absyn.SimpleVar(id, pos)): unit =
            (case Symbol.look(env, id)
                of NONE => () (* type checker will report error for this case *)
                | SOME(d, esc) => if (depth > d) (* if the current depth > depth in the escape environment,
                                                    then set the escape to true *)
                                  then esc := true
                                  else ())
      | traverseVar(env:escEnv, d:depth, Absyn.FieldVar(var, id, pos)): unit =
            (* Recursively traverse the var to the left of the field access.
               An individual field of a record cannot escape, only the record
               itself can *)
            traverseVar(env, d, var)
      | traverseVar(env:escEnv, d:depth, Absyn.SubscriptVar(var, e, pos)): unit =
            (* Recursively traverse the subscript expression, and the
               var to the left of the subscript access. *)
            (traverseExp(env, d, e);
             traverseVar(env, d, var))
    (* recursively traverses the exp's in AST, calling traverseVar and traverseDec when appropriate *)
    and traverseExp(env:escEnv, d:depth, Absyn.VarExp(var)): unit =
            (* traverse the var *)
            traverseVar(env, d, var)
      | traverseExp(env:escEnv, d:depth, Absyn.LetExp({decs, body, pos})) =
            (* traverse the body of the let expression in the escape environment
               augmented by traversing the declarations *)
            traverseExp(traverseDecs(env, d, decs), d, body)
      | traverseExp(env:escEnv, d:depth, Absyn.CallExp({func, args, pos})) =
            (* apply traverseExp to each of the argument expressions *)
            app (fn (a) => traverseExp(env, d, a)) args
      | traverseExp(env:escEnv, d:depth, Absyn.OpExp({left, oper, right, pos})) =
            (* recursively traverse the left and right expressions *)
            (traverseExp(env, d, left);
             traverseExp(env, d, right))
      | traverseExp(env:escEnv, d:depth, Absyn.RecordExp({fields, typ, pos})) =
            (* apply traverseExcp to each of the field expressions *)
            app (fn (sym, exp, pos) => traverseExp(env, d, exp)) fields
      | traverseExp(env:escEnv, d:depth, Absyn.SeqExp(exps)) =
            (* apply traverseExp to each of the expressions in the sequence *)
            app (fn (e, pos) => traverseExp(env, d, e)) exps
      | traverseExp(env:escEnv, d:depth, Absyn.AssignExp({var, exp, pos})) =
            (* traverse the var on the LHS and the expression on the RHS *)
            (traverseVar(env, d, var); traverseExp(env, d, exp))
      | traverseExp(env:escEnv, d:depth, Absyn.IfExp({test, then', else', pos})) =
            (* traverse the test and then' expressions as well as the else' expression if present *)
            (traverseExp(env, d, test);
            traverseExp(env, d, then');
            case else'
                of NONE => ()
                | SOME(e) => traverseExp(env, d, e))
      | traverseExp(env:escEnv, d:depth, Absyn.WhileExp({test, body, pos})) =
            (* traverse the test and the body expressions of the while loop *)
            (traverseExp(env, d, test);
            traverseExp(env, d, body))
      | traverseExp(env:escEnv, d:depth, Absyn.ForExp({var, escape, lo, hi, body, pos})) =
            (* traverse the lo and hi expressions in the current escape environment,
               then traverse the body of the loop in the escape environment augmented with
               the loop variable *)
            (traverseExp(env, d, lo);
             traverseExp(env, d, hi);
             traverseExp(Symbol.enter(env, var, (d, escape)), d, body))
      | traverseExp(env:escEnv, d:depth, Absyn.ArrayExp({typ, size, init, pos})) =
            (* traverse the size and init expressions *)
            (traverseExp(env, d, size);
             traverseExp(env, d, init))
      | traverseExp(env:escEnv, d:depth, _) = ()
    (* recursively traverse the given decs list, adding the relevant escape refs to the
       environment at the current depth *)
    and traverseDecs(env, d, []): escEnv = env
      | traverseDecs(env, d, dec::decs: Absyn.dec list): escEnv =
        case dec of
            Absyn.VarDec({name, escape, typ, init, pos}) =>
                (* traverse the init expression, and recursively continue parsing the
                   decs list with an environment augmented with the current depth associated
                   with the escape field of the current vardec *)
                (traverseExp(env, d, init);
                traverseDecs(Symbol.enter(env, name, (d, escape)), d, decs))
            (* type decs can't refer to anything that escapes so just recursively
               continue traversing the decs list *)
            | Absyn.TypeDec(tylist) => traverseDecs(env, d, decs)
            (* continue parsing the rest of the decs if we have reached the end of the list
               of function decs *)
            | Absyn.FunctionDec([]) => traverseDecs(env, d, decs)
            | Absyn.FunctionDec({name, params, result, body, pos}::fs) =>
                let
                    (* add the escape field from the given param to the environment
                       at the depth of the next function (depth + 1) *)
                    fun addEscape({name, escape, typ, pos}, env) =
                        Symbol.enter(env, name, (d + 1, escape))
                    (* the escape environment augmented with the information for the function params *)
                    val resultEnv = foldr addEscape env params
                in
                    (* traverse the body of the function in the next depth level in the
                       environment augmented with the function params *)
                    (traverseExp(resultEnv, d + 1, body);
                    (* traverse the rest of the function decs list at the current depth
                       in the escape environment without the new params *)
                     traverseDecs(env, d, Absyn.FunctionDec(fs)::decs))
                end

    (* traverse the AST for the given tiger program *)
    fun findEscape(prog: Absyn.exp) : unit = traverseExp(Symbol.empty, 0, prog)
end
