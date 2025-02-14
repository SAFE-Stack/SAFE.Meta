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

let optimistic =
    testList "Optimistic" [
        testList "create" [
            testCase "creates new value with no history" <| fun _ ->
                let opt = Optimistic.create 42
                Expect.equal opt.Value (Some 42) "Current value should be set"
                Expect.equal opt.Prev None "Previous value should be None"
        ]
        
        testList "empty" [
            testCase "creates empty optimistic value" <| fun _ ->
                let opt = Optimistic.empty
                Expect.equal opt.Value None "Current value should be None"
                Expect.equal opt.Prev None "Previous value should be None"
        ]
        
        testList "update" [
            testCase "updates value and shifts previous" <| fun _ ->
                let opt = Optimistic.create 42
                let updated = opt.Update 84
                Expect.equal updated.Value (Some 84) "Current value should be updated"
                Expect.equal updated.Prev (Some 42) "Previous value should be old current"
        ]
        
        testList "rollback" [
            testCase "rolls back to previous value" <| fun _ ->
                let opt = Optimistic.create 42 |> Optimistic.update 84
                let rolled = opt.Rollback()
                Expect.equal rolled.Value (Some 42) "Current value should be previous"
                Expect.equal rolled.Prev None "Previous value should be cleared"
        ]
        
        testList "map" [
            testCase "maps both values" <| fun _ ->
                let opt = { Value = Some 42; Prev = Some 21 }
                let mapped = opt.Map string
                Expect.equal mapped.Value (Some "42") "Current value should be mapped"
                Expect.equal mapped.Prev (Some "21") "Previous value should be mapped"
        ]
        
        testList "bind" [
            testCase "binds value with history" <| fun _ ->
                let opt = { Value = Some 42; Prev = Some 21 }
                let bound = opt.Bind (fun x -> { Value = Some (string x); Prev = None})
                Expect.equal bound.Value (Some "42") "Current value should be bound"
                Expect.equal bound.Prev (None) "Previous value should be bound"
        ]
        
        testList "asOption" [
            testCase "returns current value as option" <| fun _ ->
                let opt = Optimistic.create 42
                Expect.equal (Optimistic.asOption opt) (Some 42) "Should return current value"
        ]
        
        testList "asPrevOption" [
            testCase "returns previous value as option" <| fun _ ->
                let opt = Optimistic.create 42 |> Optimistic.update 84
                Expect.equal (Optimistic.asPrevOption opt) (Some 42) "Should return previous value"
        ]
    ]

let allTests = 
    testList "All Tests" [
        remoteData
        optimistic
    ]

[<EntryPoint>]
let main _ = Mocha.runTests allTests
