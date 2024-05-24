open Fake.Core
open Fake.Core.TargetOperators
open Fake.IO

let execContext = Context.FakeExecutionContext.Create false "build.fsx" []
Context.setExecutionContext (Context.RuntimeContext.Fake execContext)

module Processes =
    let createProcess exe args dir =
        // Use `fromRawCommand` rather than `fromRawCommandLine`, as its behaviour is less likely to be misunderstood.
        // See https://github.com/SAFE-Stack/SAFE-template/issues/551.
        CreateProcess.fromRawCommand exe args
        |> CreateProcess.withWorkingDirectory dir
        |> CreateProcess.ensureExitCode


    let run proc arg dir = proc arg dir |> Proc.run |> ignore
    let dotnet = createProcess "dotnet"
    
    let runDotnet = run dotnet

let sourceFolder = Path.getFullName """..\src"""

let outputFolder = Path.getFullName """..\nugetPackages"""

let projects = [ "SAFE.Client"; "SAFE.Server" ]


Target.create "Bundle" (fun _ ->
    projects
    |> List.map (fun project -> Processes.runDotnet [ "pack"; "-o"; outputFolder ] $"""{sourceFolder}/{project}""")
    |> ignore)


Target.create "Publish" (fun _ ->
    let nugetApiKey = Environment.environVarOrFail "NUGET_API_KEY"

    let nugetArgs = [
        "push"
        outputFolder + """\*.nupkg"""
        "--api-key"
        nugetApiKey
        "--source"
        """https://api.nuget.org/v3/index.json"""
    ]

    Processes.runDotnet [ "nuget"; yield! nugetArgs ] sourceFolder)

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