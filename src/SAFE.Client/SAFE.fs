namespace SAFE

open Fable.Remoting.Client

/// Contains functionality to interact with the Server.
type Api =
    /// <summary>Creates a Fable Remoting API proxy.</summary>
    /// <param name="routeBuilder">A function which takes the name of the API and the name of the method and returns the route to the server. Defaults to "/api/{api name}/{method name}"</param>
    static member inline makeProxy<'TApi>(?routeBuilder) =
        let routeBuilder = defaultArg routeBuilder (sprintf "/api/%s/%s")

        Remoting.createApi ()
        |> Remoting.withRouteBuilder routeBuilder
        |> Remoting.buildProxy<'TApi>

/// An asynchronous message which should either be Started, or has Finished. Used commonly with the Elmish server request / response pattern.
type AsyncMsg<'a, 't> =
    | Start of 'a
    | Finished of 't

/// As per Deferred<'T> except can also be refreshed.
type RefreshableDeferred<'t> =
    | NotStarted
    | Initialising
    | Refreshing of 't
    | Resolved of 't

/// Some data which is has not yet been started being calculated, is currently being calculated, or has been resolved. Typically used for data on a model that will be loaded from the server.
type Deferred<'t> =
    | NotStarted
    | InProgress
    | Resolved of 't

    member this.HasResolved =
        match this with
        | Resolved _ -> true
        | NotStarted
        | InProgress -> false

    member this.IsStillInProgress =
        match this with
        | InProgress -> true
        | NotStarted
        | Resolved _ -> false

    member this.HasStarted =
        match this with
        | NotStarted -> false
        | InProgress
        | Resolved _ -> true

    /// Transforms the underlying value of the input deferred value when it exists from type to another
    member this.Map mapper =
        match this with
        | NotStarted -> NotStarted
        | InProgress -> InProgress
        | Resolved value -> Resolved(mapper value)

    /// Verifies that a `Deferred<'T>` value is resolved and the resolved data satisfies a given requirement.
    member this.Exists predicate =
        match this with
        | NotStarted -> false
        | InProgress -> false
        | Resolved value -> predicate value

    /// Like `map` but instead of transforming just the value into another type in the `Resolved` case, it will transform the value into potentially a different case of the `Deferred<'T>` type.
    member this.Bind binder =
        match this with
        | NotStarted -> NotStarted
        | InProgress -> InProgress
        | Resolved value -> binder value

module Deferred =
    let toOption =
        function
        | NotStarted
        | InProgress -> None
        | Resolved value -> Some value

    let defaultValue x =
        function
        | NotStarted
        | InProgress -> x
        | Resolved value -> value

    /// Returns whether the `Deferred<'T>` value has been resolved or not.
    let resolved (deferred: Deferred<'T>) = deferred.HasResolved

    /// Returns whether the `Deferred<'T>` value is in progress or not.
    let inProgress (deferred: Deferred<_>) = deferred.IsStillInProgress

    /// Transforms the underlying value of the input deferred value when it exists from type to another
    let map mapper (deferred: Deferred<'T>) = deferred.Map mapper

    /// Verifies that a `Deferred<'T>` value is resolved and the resolved data satisfies a given requirement.
    let exists predicate (deferred: Deferred<'T>) = deferred.Exists predicate

    /// Like `map` but instead of transforming just the value into another type in the `Resolved` case, it will transform the value into potentially a different case of the `Deferred<'T>` type.
    let bind binder (deferred: Deferred<'T>) = deferred.Bind binder

/// An alias for a Deferred message which is a Result.
type DeferredResult<'a, 'b> = Deferred<Result<'a, 'b>>