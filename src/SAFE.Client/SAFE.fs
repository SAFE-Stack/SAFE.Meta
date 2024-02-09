namespace SAFE

open Fable.Remoting.Client

/// Contains functionality to interact with Fable Remoting APIs.
type Api =
    /// <summary>Quickly creates a Fable Remoting API proxy.</summary>
    /// <param name="routeBuilder">An optional function which takes the name of the API and the method being called, and generates the route to the server API. Defaults to `/api/{api name}/{method name}` e.g. `/api/ITodoApi/GetTodos`.</param>
    static member inline makeProxy<'TApi>(?routeBuilder) =
        let routeBuilder = defaultArg routeBuilder (sprintf "/api/%s/%s")

        Remoting.createApi ()
        |> Remoting.withRouteBuilder routeBuilder
        |> Remoting.buildProxy<'TApi>

/// Used commonly to model asynchronous calls to the server (or any other external service) within an Elmish message instead of two separate messages e.g. LoadData and DataLoaded. `Start` represents the initial request (command); `Finished` contains the resultant data on the callback.
/// See the AsyncOperation type in https://zaid-ajaj.github.io/the-elmish-book/#/chapters/commands/async-state for more details.
type ApiCall<'TIn, 'TOut> =
    | Start of 'TIn
    | Finished of 'TOut

/// Typically used for data on a model that will be loaded from the server. This type represents some data which has either not yet started loading, is currently in the process of being loaded, or has been loaded and is available.
/// See the Deferred type in https://zaid-ajaj.github.io/the-elmish-book/#/chapters/commands/async-state#conclusion for more details.
type Remote<'T> =
    | NotStarted
    | Loading
    | Loaded of 'T

    /// Unwraps the Loaded value, or returns the supplied default value.
    member this.DefaultValue v =
        match this with
        | NotStarted
        | Loading -> v
        | Loaded value -> value

    /// Returns whether the `Remote<'T>` value has been loaded or not.
    member this.HasLoaded =
        match this with
        | NotStarted
        | Loading -> false
        | Loaded _ -> true

    /// Returns whether the `Remote<'T>` value is loading or not.
    member this.IsStillLoading =
        match this with
        | Loading -> true
        | NotStarted
        | Loaded _ -> false

    /// Returns whether the `Remote<'T>` value has started loading or not.
    member this.HasStarted =
        match this with
        | NotStarted -> false
        | Loading
        | Loaded _ -> true

    /// Maps the underlying value of the remote data, when it exists, into another shape.
    member this.Map mapper =
        match this with
        | NotStarted -> NotStarted
        | Loading -> Loading
        | Loaded value -> Loaded(mapper value)

    /// Verifies that a `Remote<'T>` value is loaded, and that the data satisfies a given requirement.
    member this.Exists predicate =
        match this with
        | NotStarted -> false
        | Loading -> false
        | Loaded value -> predicate value

    /// Like `map` but instead of mapping just the value into another type in the `Loaded` case, it will transform the value into potentially a different case of the `Remote<'T>` type.
    member this.Bind binder =
        match this with
        | NotStarted -> NotStarted
        | Loading -> Loading
        | Loaded value -> binder value

    /// Maps `Loaded` to `Some`, everything else to `None`.
    member this.ToOption() =
        match this with
        | NotStarted
        | Loading -> None
        | Loaded value -> Some value

/// As per `Remote<'T>` except can also be refreshed.
type Refresa<'t> =
    | Refreshing of 't
    | Remote of Remote<'t>

/// Contains utility functions on the `Remote` type.
module Remote =
    /// Maps `Loaded` to `Some`, everything else to `None`.
    let toOption (remote: Remote<'T>) = remote.ToOption

    /// Unwraps the Loaded value, or returns the default value.
    let defaultValue defaultValue (remote: Remote<'T>) = remote.DefaultValue defaultValue

    /// Returns whether the `Remote<'T>` value has been loaded or not.
    let hasLoaded (remote: Remote<'T>) = remote.HasLoaded

    /// Returns whether the `Remote<'T>` value has started loading or not.
    let hasStarted (remote: Remote<'T>) = remote.HasStarted

    /// Returns whether the `Remote<'T>` value is loading or not.
    let isLoading (remote: Remote<_>) = remote.IsStillLoading

    /// Maps the underlying value of the remote data, when it exists, into another shape
    let map mapper (remote: Remote<'T>) = remote.Map mapper

    /// Verifies that a `Remote<'T>` value is loaded, and that the data satisfies a given requirement.
    let exists predicate (remote: Remote<'T>) = remote.Exists predicate

    /// Like `map` but instead of mapping just the value into another type in the `Loaded` case, it will transform the value into potentially a different case of the `Remote<'T>` type.
    let bind binder (remote: Remote<'T>) = remote.Bind binder

/// An alias for a Remote message which is a Result.
type RemoteResult<'a, 'b> = Remote<Result<'a, 'b>>