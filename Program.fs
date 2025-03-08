open Interpreter.Language
open Interpreter.Eval
open Interpreter.State
open Interpreter.Memory

let [<EntryPoint>] main _ =
    printfn "Actual: %A" (10 |> empty |> getMem 3)
    printfn "Expected: None"
    printfn "--------"
    printfn "Actual: %A" (10 |> empty |> setMem 3 42)
    printfn "Expected: None"
    printfn "--------"
    printfn "Actual: %A" (10 |> empty |> alloc 3)
    printfn "Expected: ????"
    printfn "--------"
    printfn "Actual: %A" (10 |> empty |> alloc 3 |> Option.bind (fun (m, ptr) -> m |> setMem ptr 42 |> Option.bind (getMem ptr)))
    printfn "Expected: Some 42"
    printfn "--------"
    0
