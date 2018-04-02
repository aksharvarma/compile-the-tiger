structure Assem = struct

  type reg = string
  type temp = Temp.temp
  type label = Temp.label

  datatype instr = OPER of {assem: string,
                            dst: temp list,
                            src: temp list,
                            jump: label list option}
                 | LABEL of {assem: string, lab: Temp.label}
                 | MOVE of {assem: string,
                            dst: temp,
                            src: temp}

  (* getAssem : instr -> string
   *
   * Extracts the assem string from the given instr
   *)
  fun getAssem(OPER{assem, dst, src, jump}) = assem
    | getAssem(LABEL{assem, lab}) = assem
    | getAssem(MOVE{assem, dst, src}) = assem

  (* getDst: instr -> Temp.temp list
   *
   * Extracts the list of destination temps from the given instr
   *)
  fun getDst(OPER{assem, dst, src, jump}) = dst
    | getDst(LABEL{assem, lab}) = []
    | getDst(MOVE{assem, dst, src}) = [dst]

  (* getSrc: instr -> Temp.temp list
   *
   * Extracts the list of source temps from the given instr
   *)
  fun getSrc(OPER{assem, dst, src, jump}) = src
    | getSrc(LABEL{assem, lab}) = []
    | getSrc(MOVE{assem, dst, src}) = [src]

  (* getJumps: instr -> Temp.label list
   *
   * Extracts the list of labels that an instruction
   * can possibly jump to.
   * For instructions with no jumps, returns an empty list
   *)
  fun getJumps(OPER{assem, dst, src, jump}) =
      (case jump
       of NONE => []
        | SOME(l) => l)
    (* Neither LABELs or MOVEs can have any jumps *)
    | getJumps(_) = []

  (* isMove : instr -> bool
   *
   * Returns true if given instruction is a move instruction
   *)
  fun isMove(MOVE _) = true
    | isMove(_) = false

  fun format saytemp =
      let
        fun speak(assem,dst,src,jump) =
            let val saylab = Symbol.name
                fun f(#"'":: #"s":: i::rest) =
                    (explode(saytemp(List.nth(src,ord i - ord #"0"))) @ f rest)
                  | f( #"'":: #"d":: i:: rest) =
                    (explode(saytemp(List.nth(dst,ord i - ord #"0"))) @ f rest)
                  | f( #"'":: #"j":: i:: rest) =
                    (explode(saylab(List.nth(jump,ord i - ord #"0"))) @ f rest)
                  | f( #"'":: #"'":: rest) = #"'" :: f rest
                  | f( #"'":: _ :: rest) = ErrorMsg.impossible "bad Assem format"
                  | f(c :: rest) = (c :: f rest)
                  | f nil = nil
            in implode(f(explode assem))
            end
    in
      (
        fn OPER{assem,dst,src,jump=NONE} => (speak(assem,dst,src,nil))
          | OPER{assem,dst,src,jump=SOME j} => speak(assem,dst,src,j)
          | LABEL{assem,...} => assem
          | MOVE{assem,dst,src} => speak(assem,[dst],[src],nil))
     end
end

