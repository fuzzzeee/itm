using System;
using System.Collections.Generic;
using System.ComponentModel;

namespace LongleyRice;

public class Variability : INotifyPropertyChanged
{
    private readonly Dictionary<PercentLevel, double?> _levels = new Dictionary<PercentLevel, double?>
    {
        {PercentLevel.Time, null},
        {PercentLevel.Location, null},
        {PercentLevel.Confidence, null},
    };
    private const double _minValue = 0.01, _maxValue = 0.99, _defaultValue = 0.5;
    public VariabilityMode Mode { get; set; } = VariabilityMode.Broadcast;

    public double Time
    {
        get => GetLevel(PercentLevel.Time);
        set => SetLevel(PercentLevel.Time, value);
    }

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
        lock (_levels)
            return _levels[level] ?? _defaultValue;
    }

    private void SetLevel(PercentLevel level, double value)
    {
        if (GetLevel(level) != value)
        {
            if (value < _minValue || value > _maxValue)
                throw new ArgumentOutOfRangeException(nameof(value), value, $"{level} must be between {_minValue} and {_maxValue}");
            lock (_levels)
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

    public event PropertyChangedEventHandler PropertyChanged;

    protected virtual void OnPropertyChanged(string propertyName)
    {
        PropertyChanged?.Invoke(this, new PropertyChangedEventArgs(propertyName));
    }
}