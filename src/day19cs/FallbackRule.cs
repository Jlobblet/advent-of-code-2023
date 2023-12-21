namespace day19cs;

public readonly record struct FallbackRule(Action Action, string Destination) : IRule
{
    public Action? Apply(Part p, out string? dst)
    {
        dst = Destination;
        return Action;
    }
}
