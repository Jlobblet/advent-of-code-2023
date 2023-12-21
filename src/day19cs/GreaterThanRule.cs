namespace day19cs;

public readonly record struct GreaterThanRule
    (Category Category, long TestValue, Action Action, string? Destination) : IRule
{
    public Action? Apply(Part p, out string? dst)
    {
        if (p.Extract(Category) > TestValue)
        {
            dst = Destination;
            return Action;
        }

        dst = null;
        return null;
    }
}
