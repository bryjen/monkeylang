<div align="center">
    <p align="center">
        <h1 align="center"><b>MonkeyLang</b></h1>
        <br/>
	<img src="https://github.com/user-attachments/assets/c3c25f7d-e5d4-4f26-b939-0b980157e352"/>
        <br/>
	A toolchain for the <a href="https://monkeylang.org/" target="_blank">Monkey</a> programming language on the .NET platform.
	<br/>
	<b>Note that this project is still under active developement.</b>
    </p>
</div>


&nbsp;
A CLI tool for the Monkey programming language that runs on .NET.
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
