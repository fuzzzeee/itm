namespace LongleyRice;

/// <summary>
/// Criteria describing the care taken at each terminal to assure good propagation conditions
/// </summary>
public enum SiteCriteria
{
    /// <summary>
    /// The choice of antenna sites is dictated by factors other than radio reception
    /// </summary>
    Random = 0,
    /// <summary>
    /// Most of the terminals are located at elevated sites, but with no attempts to select points where signals are strong
    /// </summary>
    Careful = 1,
    /// <summary>
    /// The terminals of a system are usually sited on high ground and some effort is made to locate them at sites, where the signals appear to be particularly strong
    /// </summary>
    VeryCareful = 2,
}