namespace Day2;

public static class Program
{
    public static readonly Dictionary<char, int> ShapeScores = new()
    {
        // Rock
        { 'A', 1 },
        { 'X', 1 },

        // Paper
        { 'B', 2 },
        { 'Y', 2 },

        // Scissors
        { 'C', 3 },
        { 'Z', 3 }
    };

    public static readonly Dictionary<int, int> RoundScores = new()
    {
        // Draw
        { 0, 3 },

        // Loss
        { -1, 0 },

        // Win
        { 1, 6 }
    };

    public static void Main()
    {
        var rounds = InputData.GetClueRounds();
        var judge = new ShapeComparer();

        int totalScore = 0;

        foreach (var round in rounds)
        {
            var game = judge.Compare(round.Theirs, round.Mine);
            var shapeScore = ShapeScores[round.Mine];
            var roundScore = RoundScores[game];
            var score = shapeScore + roundScore;

            totalScore += score;

            Console.WriteLine($"Theirs: {round.Theirs} - Mine: {round.Mine} ({shapeScore}) - Result: {game} ({roundScore}) - Score: {score} - Total: {totalScore}");
        }
    }
}
