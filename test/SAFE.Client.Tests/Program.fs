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
                match opt with
                | Exists (value, prev) ->
                    Expect.equal value 42 "Current value should be set"
                    Expect.equal prev None "Previous value should be None"
                | NonExistant -> 
                    failtest "Should not be NonExistant"
        ]
        
        testList "empty" [
            testCase "creates empty optimistic value" <| fun _ ->
                let opt = Optimistic.empty
                Expect.equal opt NonExistant "Should be NonExistant"
        ]
        
        testList "Value property" [
            testCase "returns Some for existing value" <| fun _ ->
                let opt = Optimistic.create 42
                Expect.equal opt.Value (Some 42) "Should return Some with current value"
            
            testCase "returns None for NonExistant" <| fun _ ->
                let opt = Optimistic.empty
                Expect.equal opt.Value None "Should return None for NonExistant"
        ]
        
        testList "update" [
            testCase "updates value and shifts previous" <| fun _ ->
                let opt = Optimistic.create 42
                let updated = opt.Update 84
                match updated with
                | Exists (value, prev) ->
                    Expect.equal value 84 "Current value should be updated"
                    Expect.equal prev (Some 42) "Previous value should be old current"
                | NonExistant ->
                    failtest "Should not be NonExistant"
            
            testCase "update on NonExistant remains NonExistant" <| fun _ ->
                let opt = Optimistic.empty
                let updated = opt.Update 42
                Expect.equal updated NonExistant "Should remain NonExistant"
        ]
        
        testList "rollback" [
            testCase "rolls back to previous value" <| fun _ ->
                let opt = Optimistic.create 42 |> fun o -> o.Update 84
                let rolled = opt.Rollback()
                match rolled with
                | Exists (value, prev) ->
                    Expect.equal value 42 "Current value should be previous"
                    Expect.equal prev None "Previous value should be None"
                | NonExistant ->
                    failtest "Should not be NonExistant"
            
            testCase "rollback on NonExistant remains NonExistant" <| fun _ ->
                let opt = Optimistic.empty
                let rolled = opt.Rollback()
                Expect.equal rolled NonExistant "Should remain NonExistant"
        ]
        
        testList "map" [
            testCase "maps both current and previous values" <| fun _ ->
                let opt = Optimistic.create 42 |> fun o -> o.Update 84
                let mapped = opt.Map string
                match mapped with
                | Exists (value, prev) ->
                    Expect.equal value "84" "Current value should be mapped"
                    Expect.equal prev (Some "42") "Previous value should be mapped"
                | NonExistant ->
                    failtest "Should not be NonExistant"
            
            testCase "map on NonExistant remains NonExistant" <| fun _ ->
                let opt = Optimistic.empty
                let mapped = opt.Map string
                Expect.equal mapped NonExistant "Should remain NonExistant"
        ]
        
        testList "module functions" [
            testCase "update function matches member" <| fun _ ->
                let opt = Optimistic.create 42
                let memberUpdate = opt.Update 84
                let moduleUpdate = Optimistic.update 84 opt
                Expect.equal moduleUpdate memberUpdate "Module update should match member update"
            
            testCase "rollback function matches member" <| fun _ ->
                let opt = Optimistic.create 42 |> fun o -> o.Update 84
                let memberRollback = opt.Rollback()
                let moduleRollback = Optimistic.rollback opt
                Expect.equal moduleRollback memberRollback "Module rollback should match member rollback"
            
            testCase "map function matches member" <| fun _ ->
                let opt = Optimistic.create 42
                let memberMap = opt.Map string
                let moduleMap = Optimistic.map string opt
                Expect.equal moduleMap memberMap "Module map should match member map"
        ]
    ]

let allTests = 
    testList "All Tests" [
        remoteData
        optimistic
    ]

[<EntryPoint>]
let main _ = Mocha.runTests allTests
