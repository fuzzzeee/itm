namespace LongleyRice;

public enum ErrorCode
{
    /// <summary>
    /// No Error
    /// </summary>
    None = 0,
    /// <summary>
    /// Warning: Some parameters are nearly out of range
    /// </summary>
    /// <remarks>
    /// Results should be used with caution
    /// </remarks>
    NearlyOutOfRange = 1,
    /// <summary>
    /// Warning: A combination of parameters is out of range
    /// </summary>
    /// <remarks>
    /// Results are probably invalid
    /// </remarks>
    CombinationOutOfRange = 3,
    /// <summary>
    /// Warning: Some parameters are out of range
    /// </summary>
    /// <remarks>
    /// Results are probably invalid
    /// </remarks>
    SomeOutOfRange = 4,
}