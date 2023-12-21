namespace day19cs;

public static class RuleExtensions
{
    public static Action? Apply(this List<IRule> rules, Part p, out string? dst)
    {
        foreach (var rule in rules)
        {
            if (rule.Apply(p, out dst) is not { } action) continue;
            return action;
        }

        dst = null;
        return null;
    }
}
