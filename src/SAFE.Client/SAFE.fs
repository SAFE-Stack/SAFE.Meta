namespace SAFE

open Fable.Remoting.Client
open Fable.SimpleJson

/// Contains functionality to interact with Fable Remoting APIs.
type Api =
    /// <summary>Quickly creates a Fable Remoting API proxy. For more details, see https://zaid-ajaj.github.io/Fable.Remoting/.</summary>
    /// <param name="routeBuilder">An optional function which takes the name of the API and the method being called, and generates the route to the server API. Defaults to `/api/{api name}/{method name}` e.g. `/api/ITodoApi/GetTodos`.</param>
    /// <param name="customOptions">An optional function which allows you to customize the API proxy such as Binary Serialization e.g. https://zaid-ajaj.github.io/Fable.Remoting/#/advanced/binary-serialization.</param>
    static member inline makeProxy<'TApi>(?routeBuilder, ?customOptions) =
        let routeBuilder = defaultArg routeBuilder (sprintf "/api/%s/%s")
        let customOptions = defaultArg customOptions id

        Remoting.createApi ()
        |> Remoting.withRouteBuilder routeBuilder
        |> customOptions
        |> Remoting.buildProxy<'TApi>

[<AutoOpen>]
module Extensions =
    type System.Exception with

        /// If propagating exceptions with Fable Remoting, this helper method deserializes any `ProxyRequestException` into a record that contains the exception details.
        /// See https://zaid-ajaj.github.io/Fable.Remoting/#/advanced/error-handling for more details.
        member this.GetPropagatedError() =
            match this with
            | :? ProxyRequestException as exn ->
                let response =
                    exn.ResponseText
                    |> Json.parseAs<{|
                        error: {| ClassName: string; Message: string |}
                        ignored: bool
                        handled: bool
                    |} >

                response.error
            | ex -> {|
                ClassName = "Unknown"
                Message = ex.Message
              |}

/// Used commonly to model asynchronous calls to the server (or any other external service) within an Elmish message instead of two separate messages e.g. `LoadData` and `DataLoaded`. `Start` represents the initial request (command); `Finished` contains the resultant data on the callback.
///
/// For an example of how to use this type, see https://safe-stack.github.io/docs/recipes/client-server/mvu-roundtrip/.
///
/// For reference documentation, see the AsyncOperation type in https://zaid-ajaj.github.io/the-elmish-book/#/chapters/commands/async-state.
type ApiCall<'TStart, 'TFinished> =
    /// Represents an API call that should be initiated.
    | Start of 'TStart
    /// Represents the response of an API call.
    | Finished of 'TFinished

/// Typically used for data on a model that will be loaded from the server. This type represents some data which has either not yet started loading, is currently in the process of being loaded, or has been loaded and is available.
/// See the Deferred type in https://zaid-ajaj.github.io/the-elmish-book/#/chapters/commands/async-state#conclusion for more details.
type RemoteData<'T> =
    /// The data has not yet started loading.
    | NotStarted
    /// The data is now being loaded.
    | Loading
    /// The data is available.
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
type RefresableRemoteData<'t> =
    | Refreshing of 't
    | Remote of RemoteData<'t>

/// Contains utility functions on the `Remote` type.
module RemoteData =
    /// Maps `Loaded` to `Some`, everything else to `None`.
    let toOption (remote: RemoteData<'T>) = remote.ToOption

    /// Unwraps the Loaded value, or returns the default value.
    let defaultValue defaultValue (remote: RemoteData<'T>) = remote.DefaultValue defaultValue

    /// Returns whether the `RemoteData<'T>` value has been loaded or not.
    let hasLoaded (remote: RemoteData<'T>) = remote.HasLoaded

    /// Returns whether the `RemoteData<'T>` value has started loading or not.
    let hasStarted (remote: RemoteData<'T>) = remote.HasStarted

    /// Returns whether the `RemoteData<'T>` value is loading or not.
    let isLoading (remote: RemoteData<_>) = remote.IsStillLoading

    /// Maps the underlying value of the remote data, when it exists, into another shape
    let map mapper (remote: RemoteData<'T>) = remote.Map mapper

    /// Verifies that a `RemoteData<'T>` value is loaded, and that the data satisfies a given requirement.
    let exists predicate (remote: RemoteData<'T>) = remote.Exists predicate

    /// Like `map` but instead of mapping just the value into another type in the `Loaded` case, it will transform the value into potentially a different case of the `RemoteData<'T>` type.
    let bind binder (remote: RemoteData<'T>) = remote.Bind binder

/// An alias for a RemoteData message which is a Result.
type RemoteDataResult<'a, 'b> = RemoteData<Result<'a, 'b>>