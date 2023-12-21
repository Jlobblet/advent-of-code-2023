using System.Text.RegularExpressions;

namespace day19cs;

public partial interface IRule
{
    private static readonly Regex RuleRegex = RuleRegexS();
    public Action? Apply(Part p, out string? dst);

    public static IRule Parse(string s)
    {
        var match = RuleRegex.Match(s);
        var dst = match.Groups["dst"].Value;
        var action = dst switch
        {
            "A" => Action.Accept,
            "R" => Action.Reject,
            _ => Action.Send
        };

        if (!match.Groups["rule"].Success) return new FallbackRule(action, dst);
        var cat = CategoryExtensions.Parse(match.Groups["cat"].Value);
        var op = match.Groups["op"].Value;
        var val = long.Parse(match.Groups["val"].Value);
        return op switch
        {
            "<" => new LessThanRule(cat, val, action, dst),
            ">" => new GreaterThanRule(cat, val, action, dst),
            _ => throw new ArgumentException($"Unknown operator: {op}")
        };
    }

    [GeneratedRegex(@"(?<rule>(?<cat>[xmas])(?<op>[<>])(?<val>\d+):)?(?<dst>\w+)")]
    private static partial Regex RuleRegexS();
}
