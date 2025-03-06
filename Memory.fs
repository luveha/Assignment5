module Interpreter.Memory
    
    type memory = {m: Map<int,int>; next: int}

    let empty memsize = {m = Map.empty<int,int>; next=0}

    let rec alloc size mem =
        match size with
        | 1 -> Some({m= mem.m.Add(size,0);next=mem.next+1},mem.next+1)
        | s when s > 0 -> alloc (s-1) {m = mem.m.Add(size,0); next=mem.next+1}
        | _ -> None
    
    let free _ = failwith "not implemented"
        
    let setMem _ = failwith "not implemented"
        
    let getMem _ = failwith "not implemented"