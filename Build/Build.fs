open Fake.Core
open Fake.Core.TargetOperators
open Fake.DotNet

let projects = [
    "../src/SAFE.client/SAFE.Client.fsproj"
    "../src/SAFE.server/SAFE.Server.fsproj"
]

let outputFolder = "../nugetPackages"

let execContext = Context.FakeExecutionContext.Create false "build.fsx" []
Context.setExecutionContext (Context.RuntimeContext.Fake execContext)

Target.create "Bundle" (fun _ ->
    projects
    |> List.map (
        DotNet.pack (fun args -> {
            args with
                Configuration = DotNet.BuildConfiguration.Release
                OutputPath = Some outputFolder
        })
    )
    |> ignore)


Target.create "Publish" (fun _ ->
    let nugetApiKey = Environment.environVarOrFail "NUGET_API_KEY"

    let nugetArgs =
        $"""push "{outputFolder}/*.nupkg" --api-key {nugetApiKey} --source https://api.nuget.org/v3/index.json"""

    let result =
        DotNet.exec (fun x -> { x with DotNetCliPath = "dotnet" }) "new" nugetArgs

    if not result.OK then
        failwithf "`dotnet %s` failed" "nuget push")


"Bundle" ==> "Publish" |> ignore


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