module Client.Tests

open Fable.Mocha
open SAFE

[<RequireQualifiedAccess>]
type RemoteDataCase =
    | NotStarted
    | LoadingEmpty
    | LoadingPopulated
    | Loaded

    member this.Label =
        match this with
        | NotStarted -> "Not started"
        | LoadingEmpty -> "Loading empty"
        | LoadingPopulated -> "Loading populated"
        | Loaded -> "loaded"

    member this.Example =
        match this with
        | NotStarted -> RemoteData.NotStarted
        | LoadingEmpty -> Loading None
        | LoadingPopulated -> Loading(Some true)
        | Loaded -> RemoteData.Loaded true

module RemoteDataCase =
    let all = [
        RemoteDataCase.NotStarted
        RemoteDataCase.LoadingEmpty
        RemoteDataCase.LoadingPopulated
        RemoteDataCase.Loaded
    ]


    let inline testListForAll<'T when 'T: equality>
        title
        (transformation: RemoteData<bool> -> 'T)
        (expectedMaker: RemoteDataCase -> 'T)
        =

        testList
            title
            (all
             |> List.map (fun case ->
                 let expected = expectedMaker case

                 (fun _ -> Expect.equal (transformation case.Example) expected "")
                 |> testCase case.Label))

let remoteData =
    testList "RemoteData" [
        RemoteDataCase.testListForAll "defaultValue" (fun case -> case.DefaultValue false) (function
            | RemoteDataCase.NotStarted
            | RemoteDataCase.LoadingEmpty -> false
            | RemoteDataCase.LoadingPopulated
            | RemoteDataCase.Loaded -> true)

        RemoteDataCase.testListForAll "HasLoaded" (_.HasLoaded) (function
            | RemoteDataCase.NotStarted
            | RemoteDataCase.LoadingEmpty
            | RemoteDataCase.LoadingPopulated -> false
            | RemoteDataCase.Loaded -> true)

        RemoteDataCase.testListForAll "AsOption" (_.AsOption) (function
            | RemoteDataCase.NotStarted
            | RemoteDataCase.LoadingEmpty -> None
            | RemoteDataCase.LoadingPopulated
            | RemoteDataCase.Loaded -> Some true)

        RemoteDataCase.testListForAll "hasStarted" (_.HasStarted) (function
            | RemoteDataCase.NotStarted -> false
            | RemoteDataCase.LoadingEmpty
            | RemoteDataCase.LoadingPopulated
            | RemoteDataCase.Loaded -> true)

        RemoteDataCase.testListForAll "hasData" (_.HasData) (function
            | RemoteDataCase.NotStarted
            | RemoteDataCase.LoadingEmpty -> false
            | RemoteDataCase.LoadingPopulated
            | RemoteDataCase.Loaded -> true)

        RemoteDataCase.testListForAll "isStillLoading" (_.IsStillLoading) (function
            | RemoteDataCase.NotStarted -> false
            | RemoteDataCase.LoadingEmpty
            | RemoteDataCase.LoadingPopulated -> true
            | RemoteDataCase.Loaded -> false)

        RemoteDataCase.testListForAll "isRefresing" (_.IsRefreshing) (function
            | RemoteDataCase.NotStarted -> false
            | RemoteDataCase.LoadingEmpty -> false
            | RemoteDataCase.LoadingPopulated -> true
            | RemoteDataCase.Loaded -> false)

        RemoteDataCase.testListForAll "hasNotStarted" (_.HasNotStarted) (function
            | RemoteDataCase.NotStarted -> true
            | RemoteDataCase.LoadingEmpty
            | RemoteDataCase.LoadingPopulated
            | RemoteDataCase.Loaded -> false)

        RemoteDataCase.testListForAll "map" (_.Map(not)) (function
            | RemoteDataCase.NotStarted -> NotStarted
            | RemoteDataCase.LoadingEmpty -> Loading None
            | RemoteDataCase.LoadingPopulated -> Loading(Some false)
            | RemoteDataCase.Loaded -> Loaded false)

        testList "bind" [
            RemoteDataCase.testListForAll "toLoaded" (_.Bind(Loaded)) (function
                | RemoteDataCase.NotStarted -> NotStarted
                | RemoteDataCase.LoadingEmpty -> Loading None
                | RemoteDataCase.LoadingPopulated
                | RemoteDataCase.Loaded -> Loaded true)

            RemoteDataCase.testListForAll "toNotStarted" (_.Bind(fun _ -> NotStarted)) (function
                | RemoteDataCase.NotStarted -> NotStarted
                | RemoteDataCase.LoadingEmpty -> Loading None
                | RemoteDataCase.LoadingPopulated
                | RemoteDataCase.Loaded -> NotStarted)
        ]

        RemoteDataCase.testListForAll "startLoading" (_.StartLoading()) (function
            | RemoteDataCase.NotStarted -> Loading None
            | RemoteDataCase.LoadingEmpty -> Loading None
            | RemoteDataCase.LoadingPopulated -> Loading (Some true)
            | RemoteDataCase.Loaded -> Loading (Some true))
    ]

[<EntryPoint>]
let main _ = Mocha.runTests remoteData