using System;

namespace LongleyRice;

public class PointToPointModel : Model
{
    private double _distance = 1;
    private double[] _elevations;

    /// <summary>
    /// Creates a new <see cref="PointToPointModel"/>
    /// </summary>
    /// <param name="elevations">The terrain heights between transmitter and receiver in meters</param>
    /// <param name="distance">The distance between transmitter and receiver in meters</param>
    public PointToPointModel(double[] elevations, double distance)
    {
        Elevations = elevations;
        Distance = distance;
    }

    /// <summary>
    /// The terrain heights between transmitter and receiver in meters
    /// </summary>
    public double[] Elevations
    {
        get => _elevations;
        set
        {
            if (value == null)
                throw new ArgumentNullException(nameof(value), "Elevations cannot be null");
            if (value.Length < 2)
                throw new ArgumentException("At least 2 elevations must be provided", nameof(value));
            _elevations = value;
            OnPropertyChanged(nameof(Elevations));
        }
    }

    /// <summary>
    /// The distance between transmitter and receiver in meters
    /// </summary>
    public double Distance
    {
        get => _distance;
        set
        {
            if (Distance != value)
            {
                if (value <= 0)
                    throw new ArgumentOutOfRangeException(nameof(value), value, "Distance must be positive");
                _distance = value;
                OnPropertyChanged(nameof(Distance));
            }
        }
    }
}