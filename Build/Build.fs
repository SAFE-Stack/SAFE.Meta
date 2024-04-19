open Fake.Core

let execContext = Context.FakeExecutionContext.Create false "build.fsx" []
Context.setExecutionContext (Context.RuntimeContext.Fake execContext)

Target.create "Bundle" (fun _ -> printfn "We are bundling!")



[<EntryPoint>]
let main args =
    try
        match args with
        | [| target |] -> Target.runOrDefault target
        | _ -> Target.runOrDefault "Bundle"

        0
    with e ->
        printfn "%A" e
        1