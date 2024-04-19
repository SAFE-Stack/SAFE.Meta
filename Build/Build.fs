open Fake.Core
open Fake.DotNet

let projects = [
    "../src/SAFE.client/SAFE.Client.fsproj"
    "../src/SAFE.server/SAFE.Server.fsproj"
]

let execContext = Context.FakeExecutionContext.Create false "build.fsx" []
Context.setExecutionContext (Context.RuntimeContext.Fake execContext)

Target.create "Bundle" (fun _ -> projects |> List.map (DotNet.build id) |> ignore)



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