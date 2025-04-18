using System.Diagnostics.CodeAnalysis;

namespace Spectre.Console.Prompts.Extensions;

/// <summary>
/// Represents a single list prompt from a type that is transformed and rendered to a <c>Spectre.Console.Tree</c> object.
/// </summary>
/// <typeparam name="SOURCE">The type to be transformed into a tree.</typeparam>
/// <typeparam name="OUTPUT">The type returned when choosing one of the tree options.</typeparam>
[SuppressMessage("ReSharper", "InconsistentNaming")]
public class TreeSelectionPrompt<SOURCE, OUTPUT> : IPrompt<OUTPUT>
    where SOURCE : notnull 
    where OUTPUT : notnull
{
    // List of choices as strings, is displayed to the user.
    private readonly List<string> choices;
    
    // List of objects to be returned. Each corresponds to a single prompt.
    private readonly List<OUTPUT> returnValues;

    /// <summary>
    /// Initializes a new instance of the <see cref="TreeSelectionPrompt{SOURCE,OUTPUT}"/> class.
    /// </summary>
    public TreeSelectionPrompt(
        Func<SOURCE, Tree> toTree,
        Func<SOURCE, List<OUTPUT>> toReturnValues, 
        SOURCE obj)
    {
        Tree asTree = toTree(obj);
        var unprocessedChoices = AnsiBuilder.BuildLines(AnsiConsole.Console, asTree).ToList();
        choices = unprocessedChoices.GetRange(0, unprocessedChoices.Count - 1);
        returnValues = toReturnValues(obj).ToList();
        
        // Validate 
        if (choices.Count != returnValues.Count)
        {
            string errorMessage = "The number of choices is unequal to the number of return values. " +
                                  $"The source was decomposed into a tree of \"{choices.Count}\" lines, while " +
                                  $"the source was decomposed into a list of options \"{returnValues.Count}\" in length. " +
                                  "The two lengths must be equal.";
            throw new ArgumentException(errorMessage);
        }
    }

    private static SelectionPrompt<(OUTPUT, string)> toSelectionPrompt(TreeSelectionPrompt<SOURCE, OUTPUT> tsp)
    {
        var pairs = tsp.returnValues.Zip(tsp.choices, (fst, snd) => (fst, snd));
        
        var selectionPrompt = new SelectionPrompt<(OUTPUT, string)>
        {
            PageSize = 30,
            Converter = pair => pair.Item2 
        };

        return selectionPrompt.AddChoices(pairs);
    }

    /// <inheritdoc/>
    public OUTPUT Show(IAnsiConsole console)
    {
        var asSelectionPrompt = toSelectionPrompt(this);
        var chosenPair = asSelectionPrompt.Show(console);
        return chosenPair.Item1;
    }
    
    /// <inheritdoc/>
    public async Task<OUTPUT> ShowAsync(IAnsiConsole console, CancellationToken cancellationToken)
    {
        var asSelectionPrompt = toSelectionPrompt(this);
        var chosenPair = await asSelectionPrompt.ShowAsync(console, cancellationToken);
        return chosenPair.Item1;
    }
}