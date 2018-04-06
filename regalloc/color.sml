signature COLOR =
sig
  type allocation = Frame.register Temp.Table.table

  val color: {interference: Liveness.igraph,
              initial: allocation,
              spillCost: Graph.node -> int,
              registers: Frame.register list}
               -> allocation * Temp.temp list
end

structure Color: COLOR =
struct
  type allocation = Frame.register Temp.Table.table

  fun color{interference, initial, spillCost, registers} = (Temp.Table.empty, [])
end
