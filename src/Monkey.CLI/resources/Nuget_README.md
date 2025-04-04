# Monkey.CLI

**THIS PROJECT IS STILL A WIP**

A CLI tool for the [Monkey programming language](https://monkeylang.org/) that runs on .NET.
Monkey is a general-purpose, minimal, procedural language with first-class functions.
With a robust toolchain and tight .NET integration, it offers a feature rich platform with an emphasis on rapid prototyping.


&nbsp;
#### Easy to install
```
dotnet tool install --global bryjen.monkey
```
No bootstrapping or building from source required, all you need is a compatible .NET runtime installed.


&nbsp;
#### Development experience first
```
error: Invalid generic type.
  ┌─ C:\Users\Public\Program.mk:1:22
  |
0 | Result<string, System.Collections.Generic.List<string>>
  |                      ^ Expected a Comma ',', or a GreaterThan '>''.
```
Inspired by the error handling and error messages of modern functional languages (OCaml, F#, Rust, Gleam, etc.), the toolchain is developed to make writing and debugging as stress-free and predictable as possible.


&nbsp;
#### Dotnet Interop
```csharp
// csharp interop example
let builder = WebApplication.CreateBuilder(args);
let app = builder.Build();

let middleware = async fn(HttpContext context, Func<Task> next) {
    // preprocessing
    await next();
    // postprocessing
};

app.Use(middleware);
app.Run();
```
With first-class .NET support, Monkey can seamlessly use countless C#/F#/VB.NET libraries.
Monkey can also transpile into C#, allowing you to see how your Monkey code translates to C#.


&nbsp;
#### Functional concepts
```fsharp
type Verbosity =
    | Normal
    | Debug
    | Diagnostic
    
let performAction = fn([object -> object] action, Verbosity verbosity) : Result<unit, Exception> {
    ...
}

performAction(...)
```
Inspired by the simplicity of traditional procedural programming languages and the expressive and rigid structures of functional programming languages, Monkey aims to find a middle-ground and present the best of both worlds.