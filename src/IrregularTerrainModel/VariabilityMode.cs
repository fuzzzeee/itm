namespace LongleyRice;

public enum VariabilityMode
{
    /// <summary>
    /// Confidence is Time/Situation/Location, Time, Location not used
    /// </summary>
    /// <remarks>
    /// Typical use may be a single use communication link, where confidence would be a measure of the combined variability, or it could be a mobile-to-mobile system, where the statistics would be reliability
    /// </remarks>
    Single = 0,
    /// <summary>
    /// Time is Situation/Location, Confidence is Confidence, Location not used
    /// </summary>
    /// <remarks>
    /// Typical user would be the individual receiver of a broadcast station for whom reliability means the time availability and confidence measures the combined situation/location variability
    /// </remarks>
    Individual = 1,
    /// <summary>
    /// Time is Time/Location (Reliability), Confidence is Confidence, Location not used
    /// </summary>
    /// <remarks>
    /// Typical user is a mobile system with a single base station. Reliability would refer to the combined time/location variability and confidence means the situation variability
    /// </remarks>
    Mobile = 2,
    /// <summary>
    /// Time is Time, Location is Location, Confidence is Confidence
    /// </summary>
    /// <remarks>
    /// Time, location and situation variability treated separately. Typical user would be the broadcaster for whom reliability would measure both location and time and confidence would measure situation variability
    /// </remarks>
    Broadcast = 3,
}