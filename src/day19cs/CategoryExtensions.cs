namespace day19cs;

public static class CategoryExtensions
{
    public static Category Parse(string s)
    {
        return s switch
        {
            "x" => Category.ExtremelyCoolLooking,
            "m" => Category.Musical,
            "a" => Category.Aerodynamic,
            "s" => Category.Shiny,
            _ => throw new ArgumentException($"Unknown category: {s}")
        };
    }

    public static Category Parse(char c)
    {
        return c switch
        {
            'x' => Category.ExtremelyCoolLooking,
            'm' => Category.Musical,
            'a' => Category.Aerodynamic,
            's' => Category.Shiny,
            _ => throw new ArgumentException($"Unknown category: {c}")
        };
    }

    public static string ToShortString(this Category category)
    {
        return category switch
        {
            Category.ExtremelyCoolLooking => "x",
            Category.Musical => "m",
            Category.Aerodynamic => "a",
            Category.Shiny => "s",
            _ => throw new ArgumentException($"Unknown category: {category}")
        };
    }

    public static char ToChar(this Category category)
    {
        return category switch
        {
            Category.ExtremelyCoolLooking => 'x',
            Category.Musical => 'm',
            Category.Aerodynamic => 'a',
            Category.Shiny => 's',
            _ => throw new ArgumentException($"Unknown category: {category}")
        };
    }
}
