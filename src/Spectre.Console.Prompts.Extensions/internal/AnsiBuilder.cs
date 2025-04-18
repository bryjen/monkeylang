// ReSharper disable CheckNamespace
using Spectre.Console.Rendering;

namespace Spectre.Console.Prompts.Extensions;

using System.Reflection;

// Exposes some of the internal functions/method from 'Spectre.Console.AnsiBuilder'
// The class is mainly responsible for rendering objects into the console.

public static class AnsiBuilder
{
    private static readonly MethodInfo BuildFunction;

    // Attempts to initialize 'buildFunction'
    static AnsiBuilder()
    {
        Assembly assembly = Assembly.Load("Spectre.Console");
        Type? classType = assembly.GetType("Spectre.Console.AnsiBuilder");

        if (classType is null)
        {
            const string errorMessage = "Could not find the type 'Spectre.Console.AnsiBuilder' in the assembly 'Spectre.Console'.";
            throw new MissingMemberException(errorMessage);
        }

        Type[] parameterTypes = { typeof(IAnsiConsole), typeof(IRenderable) };
        var methodInfo = classType.GetMethod("Build", BindingFlags.Static | BindingFlags.Public, null, parameterTypes, null);

        if (methodInfo is null)
        {
            const string errorMessage = "Could not find the method 'Build'.";
            throw new MissingMemberException(errorMessage);
        }

        BuildFunction = methodInfo;
    }

    /// <summary>
    /// Renders an <c>IRenderable</c> into a string that can be printed to the console. 
    /// </summary>
    /// <remarks>
    /// Basically a wrapper function to Spectre.Console's AnsiBuilder.Build(IAnsiConsole, IRenderable) method.
    /// </remarks>
    /// https://github.com/spectreconsole/spectre.console/blob/main/src/Spectre.Console/Internal/Backends/Ansi/AnsiBuilder.cs
    public static string Build(IAnsiConsole console, IRenderable renderable)
    {
        string? result = BuildFunction.Invoke(null, new object[] { console, renderable }) as string;

        if (result is null)
        {
            const string errorMessage = "A call to 'AnsiBuild.Build(IAnsiConsole, IRenderable)' returned a null value.";
            throw new NullReferenceException(errorMessage);
        }

        return result;
    }
    
    /// <summary>
    /// Renders an <c>IRenderable</c> into a string that can be printed to the console. 
    /// </summary>
    /// <remarks>
    /// Basically a wrapper function to Spectre.Console's AnsiBuilder.Build(IAnsiConsole, IRenderable) method.
    /// </remarks>
    /// https://github.com/spectreconsole/spectre.console/blob/main/src/Spectre.Console/Internal/Backends/Ansi/AnsiBuilder.cs
    public static string Build(IRenderable renderable)
    {
        string? result = BuildFunction.Invoke(null, new object[] { AnsiConsole.Console, renderable }) as string;

        if (result is null)
        {
            const string errorMessage = "A call to 'AnsiBuild.Build(IAnsiConsole, IRenderable)' returned a null value.";
            throw new NullReferenceException(errorMessage);
        }

        return result;
    }

    /// <summary>
    /// Renders an <c>IRenderable</c> into a string, and then split it into lines. 
    /// </summary>
    public static IEnumerable<string> BuildLines(IAnsiConsole console, IRenderable renderable)
    {
        string result = Build(console, renderable);
        
        return result
            .Split('\n')
            .Select(line => line.Replace("\r", ""));
    }
}