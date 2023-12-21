using System.Diagnostics;

namespace day19cs;

public static class Program
{
    public static void Main(string[] argv)
    {
        var str = File.ReadAllText(Environment.GetEnvironmentVariable("INPUT")!);
        var halves = str.Trim().Split("\n\n");
        var rules = halves[0].Split('\n').AsParallel().Where(s => !string.IsNullOrWhiteSpace(s)).Select(s => ParseRule(s.Trim()))
            .ToDictionary(t => t.name, t => t.rules);
        var parts = halves[1].Split('\n').AsParallel().Where(s => !string.IsNullOrWhiteSpace(s)).Select(s => Part.Parse(s.Trim()))
            .ToList();
        
        // Part 1
        var watch = Stopwatch.StartNew();
        var p1 = parts.AsParallel().Where(p => p.IsAccepted(rules)).Sum(p => p.Sum());
        watch.Stop();
        Console.WriteLine($"Part 1: {p1}, {watch.ElapsedMilliseconds}ms");
    }

    private static (string name, List<IRule> rules) ParseRule(string s)
    {
        var i = s.IndexOf('{');
        var name = s[..i];
        var rules = s[(i + 1)..^1].Split(',').Select(IRule.Parse).ToList();
        return (name, rules);
    }
}
