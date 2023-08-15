namespace LongleyRice;

public enum VariabilityMode
{
    /// <summary>
    /// Confidence is Time/Situation/Location, Time, Location not used
    /// </summary>
    Single = 0,
    /// <summary>
    /// Time is Situation/Location, Confidence is Confidence, Location not used
    /// </summary>
    Individual = 1,
    /// <summary>
    /// Time is Time/Location (Reliability), Confidence is Confidence, Location not used
    /// </summary>
    Mobile = 2,
    /// <summary>
    /// Time is Time, Location is Location, Confidence is Confidence
    /// </summary>
    Broadcast = 3,
}