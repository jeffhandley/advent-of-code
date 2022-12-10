namespace Day2;

public class ShapeComparer : IComparer<char>
{
    public int Compare(char theirs, char mine)
    {
        return theirs switch
        {
            'A' => mine == 'A' ? 0 : (mine == 'B' ? 1 : -1),
            'B' => mine == 'B' ? 0 : (mine == 'C' ? 1 : -1),
            'C' => mine == 'C' ? 0 : (mine == 'A' ? 1 : -1),
            _ => throw new ArgumentException(nameof(theirs)),
        };
    }
}
