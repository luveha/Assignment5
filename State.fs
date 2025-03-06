module Interpreter.State

    open Result
    open Language
  
    let alloc _ = failwith "not implemented"
    let free _ = failwith "not implemented"
    let getMem _ = failwith "not implemented"
    let setMem _ = failwith "not implemented"

    let reservedVariableNameList = ["if"; "then"; "else"; "while"; "declare"; "print"; "random"; "fork"; "__result__"]
    let reservedVariableName s= List.exists(fun x -> x = s) reservedVariableNameList

    let validVariableName (s: string) = 
        match s.[0] with
        | c when System.Char.IsAsciiLetter(c) || c = '_' -> String.forall(fun c-> System.Char.IsAsciiLetterOrDigit(c) || c = '_') s.[1..]
        | _ -> false
    
    
    type state = {m: List<Map<string,int>>}
    
    let mkState () = {m = [Map.empty<string,int>]}

    let push st = {m = Map.empty<string,int> :: st.m}

    let pop st = 
        match st.m with
        | t :: h -> {m = h}
        | _ -> failwith "List is empty"  
    

    let declare x st =
        match st.m.Head with
        | m when m.ContainsKey x -> 
            Error (VarNotDeclared(x)) //HERE
        | _ when reservedVariableName x -> 
            Error (ReservedName(x))
        | _ when not (validVariableName x) -> 
            Error (InvalidVarName(x))
        | m ->
            Ok ({m = m.Add(x,0) :: (pop st).m})

    let rec getVar x st =
        match st.m with
        | [] -> Error (VarNotDeclared(x))
        | h :: t ->
            match h.ContainsKey x with
            | true -> Ok h.[x]
            | _ -> getVar x {m = t}

    let rec setVar x v st = 
        match st.m with
        | [] -> Error (VarNotDeclared(x))
        | h :: t ->
            match h.ContainsKey x with
            | true -> Ok {m = h.Add(x,v) :: t}
            | _ -> setVar x v {m = t}


    let random _ = failwith "not implemented"