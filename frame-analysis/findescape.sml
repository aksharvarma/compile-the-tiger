structure FindEscape: sig val findEscape: Absyn.exp -> unit end =
struct
    type depth = int
    type escEnv = (depth * bool ref) Symbol.table

    fun traverseVar(env:escEnv, depth:depth, Absyn.SimpleVar(id, pos)): unit =
            (print("var " ^ Symbol.name(id) ^ " at depth: "^Int.toString(depth)^"\n");
            case Symbol.look(env, id)
                of NONE => () (* type checker will report error for this case *)
                | SOME(d, esc) => if (depth > d)
                                  then (print(Int.toString(depth) ^ ":" ^ Int.toString(d) ^ "\n"); esc := true)
                                  else ())
      | traverseVar(env:escEnv, d:depth, Absyn.FieldVar(var, id, pos)): unit =
            traverseVar(env, d, var)
      | traverseVar(env:escEnv, d:depth, Absyn.SubscriptVar(var, e, pos)): unit =
            (traverseExp(env, d, e); traverseVar(env, d, var))

    and traverseExp(env:escEnv, d:depth, Absyn.VarExp(var)): unit =
            traverseVar(env, d, var)
      | traverseExp(env:escEnv, d:depth, Absyn.LetExp({decs, body, pos})) =
            traverseExp(traverseDecs(env, d, decs), d, body)
      | traverseExp(env:escEnv, d:depth, Absyn.CallExp({func, args, pos})) =
            app (fn (a) => traverseExp(env, d, a)) args
      | traverseExp(env:escEnv, d:depth, Absyn.OpExp({left, oper, right, pos})) =
            (traverseExp(env, d, left);
             traverseExp(env, d, right))
      | traverseExp(env:escEnv, d:depth, Absyn.RecordExp({fields, typ, pos})) =
            app (fn (sym, exp, pos) => traverseExp(env, d, exp)) fields
      | traverseExp(env:escEnv, d:depth, Absyn.SeqExp(exps)) =
            app (fn (e, pos) => traverseExp(env, d, e)) exps
      | traverseExp(env:escEnv, d:depth, Absyn.AssignExp({var, exp, pos})) =
            (traverseVar(env, d, var); traverseExp(env, d, exp))
      | traverseExp(env:escEnv, d:depth, Absyn.IfExp({test, then', else', pos})) =
            (traverseExp(env, d, test);
            traverseExp(env, d, then');
            case else'
                of NONE => ()
                | SOME(e) => traverseExp(env, d, e))
      | traverseExp(env:escEnv, d:depth, Absyn.WhileExp({test, body, pos})) =
            (traverseExp(env, d, test);
            traverseExp(env, d, body))
      | traverseExp(env:escEnv, d:depth, Absyn.ForExp({var, escape, lo, hi, body, pos})) =
            (traverseExp(env, d, lo);
             traverseExp(env, d, hi);
             traverseExp(Symbol.enter(env, var, (d, escape)), d, body))
      | traverseExp(env:escEnv, d:depth, Absyn.ArrayExp({typ, size, init, pos})) =
            (traverseExp(env, d, size);
             traverseExp(env, d, init))
      | traverseExp(env:escEnv, d:depth, _) = ()

    and traverseDecs(env, d, []): escEnv = env
      | traverseDecs(env, d, dec::decs: Absyn.dec list): escEnv =
        case dec of
            Absyn.VarDec({name, escape, typ, init, pos}) =>
                (traverseExp(env, d, init);
                traverseDecs(Symbol.enter(env, name, (d, escape)), d, decs))
            | Absyn.TypeDec(tylist) => traverseDecs(env, d, decs)
            | Absyn.FunctionDec([]) => traverseDecs(env, d, decs)
            | Absyn.FunctionDec({name, params, result, body, pos}::fs) =>
                let
                    fun addEscape({name, escape, typ, pos}, env) =
                        Symbol.enter(env, name, (d + 1, escape))
                    val resultEnv = foldr addEscape env params
                in
                    (print("entering function body\n"); traverseExp(resultEnv, d + 1, body); traverseDecs(resultEnv, d, Absyn.FunctionDec(fs)::decs))
                end

    fun findEscape(prog: Absyn.exp) : unit = traverseExp(Symbol.empty, 0, prog)
end
