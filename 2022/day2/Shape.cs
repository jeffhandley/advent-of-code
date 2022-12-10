namespace Day2;

public class ShapeComparer : IComparer<char>
{
    public int Compare(char theirs, char mine)
    {
        return theirs switch
        {
            'A' => mine == 'X' ? 0 : (mine == 'Y' ? 1 : -1),
            'B' => mine == 'Y' ? 0 : (mine == 'Z' ? 1 : -1),
            'C' => mine == 'Z' ? 0 : (mine == 'X' ? 1 : -1),
            _ => throw new ArgumentException(nameof(theirs)),
        };
    }
}
