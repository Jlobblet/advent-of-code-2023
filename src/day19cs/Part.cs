using System.Text.RegularExpressions;

namespace day19cs;

public readonly partial record struct Part(long X, long M, long A, long S)
{
    private static readonly Regex PartRegex = PartRegexS();

    public static Part Parse(string s)
    {
        var match = PartRegex.Match(s);
        return new Part(
            long.Parse(match.Groups["x"].Value),
            long.Parse(match.Groups["m"].Value),
            long.Parse(match.Groups["a"].Value),
            long.Parse(match.Groups["s"].Value)
        );
    }

    public long Extract(Category category)
    {
        return category switch
        {
            Category.ExtremelyCoolLooking => X,
            Category.Musical => M,
            Category.Aerodynamic => A,
            Category.Shiny => S,
            _ => throw new ArgumentException($"Unknown category: {category}")
        };
    }

    public bool IsAccepted(Dictionary<string, List<IRule>> rules)
    {
        var rs = rules["in"];
        while (true)
            switch (rs.Apply(this, out var dst))
            {
                case Action.Accept:
                    return true;
                case Action.Reject:
                    return false;
                case Action.Send:
                    rs = rules[dst!];
                    break;
            }
    }

    public long Sum()
    {
        return X + M + A + S;
    }

    [GeneratedRegex(@"{x=(?<x>\d+),m=(?<m>\d+),a=(?<a>\d+),s=(?<s>\d+)}")]
    private static partial Regex PartRegexS();
}
