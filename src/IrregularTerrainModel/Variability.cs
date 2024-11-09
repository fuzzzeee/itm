using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Runtime.CompilerServices;

namespace LongleyRice;

public class Variability : INotifyPropertyChanged
{
    private readonly Dictionary<PercentLevel, double> _levels = new Dictionary<PercentLevel, double>
    {
        {PercentLevel.Time, _defaultValue},
        {PercentLevel.Location, _defaultValue},
        {PercentLevel.Confidence, _defaultValue},
    };
    private const double _minValue = 0.01, _maxValue = 0.99, _defaultValue = 0.5;
    public VariabilityMode Mode { get; set; } = VariabilityMode.Broadcast;

    /// <summary>
    /// Represents the fraction of time during which the losses are less than the calculated loss
    /// </summary>
    public double Time
    {
        get => GetLevel(PercentLevel.Time);
        set => SetLevel(PercentLevel.Time, value);
    }

    /// <summary>
    /// Represents the fraction of locations at which the losses are less than the calculated loss
    /// </summary>
    public double Location
    {
        get => GetLevel(PercentLevel.Location);
        set => SetLevel(PercentLevel.Location, value);
    }

    public double Confidence
    {
        get => GetLevel(PercentLevel.Confidence);
        set => SetLevel(PercentLevel.Confidence, value);
    }

    private double GetLevel(PercentLevel level)
    {
        switch (Mode)
        {
            case VariabilityMode.Single:
                level = PercentLevel.Confidence;
                break;
            case VariabilityMode.Individual:
            case VariabilityMode.Mobile:
                if (level == PercentLevel.Location)
                    level = PercentLevel.Time;
                break;
        }
        return _levels[level];
    }

    private void SetLevel(PercentLevel level, double value)
    {
        if (GetLevel(level) != value)
        {
            if (value < _minValue || value > _maxValue)
                throw new ArgumentOutOfRangeException(nameof(value), value, $"{level} must be between {_minValue} and {_maxValue}");
            _levels[level] = value;
            OnPropertyChanged(level.ToString());
        }
    }

    private enum PercentLevel
    {
        Time,
        Location,
        Confidence,
    }

    /// <inheritdoc />
    public event PropertyChangedEventHandler PropertyChanged;

    /// <summary>
    /// A property value has changed
    /// </summary>
    protected virtual void OnPropertyChanged([CallerMemberName] string propertyName = null)
    {
        PropertyChanged?.Invoke(this, new PropertyChangedEventArgs(propertyName));
    }
}