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
/// See https://zaid-ajaj.github.io/the-elmish-book/#/chapters/commands/async-state for more details.
type ApiCall<'TIn, 'TOut> =
    | Start of 'TIn
    | Finished of 'TOut

/// Typically used for data on a model that will be loaded from the server. This type represents some data which has either not yet started loading, is currently in the process of being loaded, or has been loaded and is available.
/// See https://zaid-ajaj.github.io/the-elmish-book/#/chapters/commands/async-state#conclusion for more details.
type RemoteData<'T> =
    | NotStarted
    | Loading
    | Loaded of 'T

    /// Unwraps the Loaded value, or returns the supplied default value.
    member this.DefaultValue v =
        match this with
        | NotStarted
        | Loading -> v
        | Loaded value -> value

    /// Returns whether the `RemoteData<'T>` value has been loaded or not.
    member this.HasLoaded =
        match this with
        | NotStarted
        | Loading -> false
        | Loaded _ -> true

    /// Returns whether the `RemoteData<'T>` value is loading or not.
    member this.IsStillLoading =
        match this with
        | Loading -> true
        | NotStarted
        | Loaded _ -> false

    /// Returns whether the `RemoteData<'T>` value has started loading or not.
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

    /// Verifies that a `RemoteData<'T>` value is loaded, and that the data satisfies a given requirement.
    member this.Exists predicate =
        match this with
        | NotStarted -> false
        | Loading -> false
        | Loaded value -> predicate value

    /// Like `map` but instead of mapping just the value into another type in the `Loaded` case, it will transform the value into potentially a different case of the `RemoteData<'T>` type.
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

/// As per `RemoteData<'T>` except can also be refreshed.
type RefreshableRemoteData<'t> =
    | Refreshing of 't
    | RemoteData of RemoteData<'t>

/// Contains utility functions on the `RemoteData` type.
module RemoteData =
    /// Maps `Loaded` to `Some`, everything else to `None`.
    let toOption (deferred: RemoteData<'T>) = deferred.ToOption

    /// Unwraps the Loaded value, or returns the default value.
    let defaultValue defaultValue (remoteData: RemoteData<'T>) = remoteData.DefaultValue defaultValue

    /// Returns whether the `RemoteData<'T>` value has been loaded or not.
    let hasLoaded (remoteData: RemoteData<'T>) = remoteData.HasLoaded

    /// Returns whether the `RemoteData<'T>` value has started loading or not.
    let hasStarted (remoteData: RemoteData<'T>) = remoteData.HasStarted

    /// Returns whether the `RemoteData<'T>` value is loading or not.
    let isLoading (remoteData: RemoteData<_>) = remoteData.IsStillLoading

    /// Maps the underlying value of the remote data, when it exists, into another shape
    let map mapper (remoteData: RemoteData<'T>) = remoteData.Map mapper

    /// Verifies that a `RemoteData<'T>` value is loaded, and that the data satisfies a given requirement.
    let exists predicate (remoteData: RemoteData<'T>) = remoteData.Exists predicate

    /// Like `map` but instead of mapping just the value into another type in the `Loaded` case, it will transform the value into potentially a different case of the `RemoteData<'T>` type.
    let bind binder (remoteData: RemoteData<'T>) = remoteData.Bind binder

/// An alias for a RemoteData message which is a Result.
type RemoteDataResult<'a, 'b> = RemoteData<Result<'a, 'b>>