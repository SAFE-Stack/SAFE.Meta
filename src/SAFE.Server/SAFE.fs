namespace SAFE

open Giraffe
open Fable.Remoting.Server
open Fable.Remoting.Giraffe

[<AutoOpen>]
module Constants =
    open Microsoft.Extensions.Hosting

    /// Tests if the current environment is development, based on the presence of the ASPNETCORE_ENVIRONMENT envionment variable.
    let isDev =
        System.Environment.GetEnvironmentVariable "ASPNETCORE_ENVIRONMENT" = Environments.Development

/// Pipeline wrappers to add different kinds of dependencies into the DI container.
module DI =
    open Microsoft.Extensions.DependencyInjection
    let singleton<'T when 'T: not struct> (services: IServiceCollection) = services.AddSingleton<'T>()
    let transient<'T when 'T: not struct> (services: IServiceCollection) = services.AddTransient<'T>()
    let scoped<'T when 'T: not struct> (services: IServiceCollection) = services.AddScoped<'T>()

[<AutoOpen>]
module Extensions =
    open Microsoft.AspNetCore.Http
    open Microsoft.Extensions.Configuration

    type HttpContext with

        /// Gets the current IConfiguration from the IOC container.
        member this.Configuration = this.GetService<IConfiguration>()

type Api =
    /// <summary>Creates a Fable Remoting API as an HTTP Handler for Giraffe.</summary>
    /// <param name="routeBuilder">A function which takes the name of the API and the name of the method and returns the route to the server. Defaults to "/api/{api name}/{method name}".</param>
    /// <param name="errorHandler">A function which takes the Exception with a RouteInfo and returns a Giraffe Handler. Defaults to Ignore the exception i.e. it will not be cascaded.</param>
    /// <param name="customOptions">A function which takes the current Fable Remoting options and returns a new set of options.</param>
    static member make(api, ?routeBuilder, ?errorHandler, ?customOptions) =
        let customOptions = defaultArg customOptions id
        let errorHandler = defaultArg errorHandler (fun _ _ -> Ignore)
        let routeBuilder = defaultArg routeBuilder (sprintf "/api/%s/%s")

        Remoting.createApi ()
        |> Remoting.withRouteBuilder routeBuilder
        |> Remoting.withErrorHandler errorHandler
        |> customOptions
        |> Remoting.fromContext api
        |> Remoting.buildHttpHandler