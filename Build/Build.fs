﻿open Fake.Core
open Fake.Core.TargetOperators
open Fake.IO
open Fake.JavaScript


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

let sourceFolder = Path.getFullName """../src"""
let outputFolder = Path.getFullName """../nugetPackages"""

let clientTestFolder = Path.getFullName """../test/SAFE.Client.Tests"""

let projects = [ "SAFE.Client"; "SAFE.Server" ]

Target.create "Test" (fun _ ->
    Npm.install (fun o -> {
        o with
            WorkingDirectory = clientTestFolder
    })

    Npm.run "test" (fun o -> {
        o with
            WorkingDirectory = clientTestFolder
    }))

Target.create "Bundle" (fun _ ->
    let version = Environment.environVarOrFail "VERSION"
    let releaseNotes = Environment.environVarOrFail "RELEASE_NOTES_URL"

    projects
    |> List.map (fun project ->
        Processes.runDotnet
            [
                "pack"
                "-o"
                outputFolder
                $"-p:PackageVersion={version}"
                $"-p:PackageReleaseNotes={releaseNotes}"
            ]
            $"""{sourceFolder}/{project}""")
    |> ignore)


Target.create "Publish" (fun _ ->
    let nugetApiKey = Environment.environVarOrFail "NUGET_API_KEY"

    let nugetArgs = [
        "push"
        "*.nupkg"
        "--api-key"
        nugetApiKey
        "--source"
        """https://api.nuget.org/v3/index.json"""
    ]

    Processes.runDotnet [ "nuget"; yield! nugetArgs ] outputFolder)

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