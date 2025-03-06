module Interpreter.State
    open Language
    
    type state = {m: List<Map<string,int>>}

    val mkState : unit -> state 
    val declare : string -> state -> Result<state,error>
    val getVar : string -> state -> Result<int,error>
    val setVar: string -> int -> state -> Result<state,error>

    val pop : state -> state
    val push: state -> state