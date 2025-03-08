module Interpreter.State
    open Language
    
    type state = {m: Map<string,int>}
    val mkState : int -> state 

    val declare : string -> state -> state option
    val getVar : string -> state -> int option
    val setVar: string -> int -> state -> state option
    (* 

    val declare : string -> state -> Result<state,error>
    val getVar : string -> state -> Result<int,error>
    val setVar: string -> int -> state -> Result<state,error>

    val pop : state -> state
    val push: state -> state *)