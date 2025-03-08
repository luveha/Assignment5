module Interpreter.State
    open Language
    
    type state = {m: Map<string,int>}
    val mkState : int -> state 

    val declare : string -> state -> state option
    val getVar : string -> state -> int option
    val setVar: string -> int -> state -> state option