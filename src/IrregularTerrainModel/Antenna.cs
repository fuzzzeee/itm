using System;
using System.ComponentModel;

namespace LongleyRice;

public class Antenna : INotifyPropertyChanged
{
    private double _height;
    private SiteCriteria _siteCriteria = SiteCriteria.Random;

    /// <summary>
    /// The antenna height in meters
    /// </summary>
    public double Height
    {
        get => _height;
        set
        {
            if (Height != value)
            {
                if (value < 0)
                    throw new ArgumentOutOfRangeException(nameof(value), value, "Height cannot be negative");
                _height = value;
                OnPropertyChanged(nameof(Height));
            }
        }
    }

    public SiteCriteria SiteCriteria
    {
        get => _siteCriteria;
        set
        {
            if (SiteCriteria != value)
            {
                _siteCriteria = value;
                OnPropertyChanged(nameof(SiteCriteria));
            }
        }
    }

    public event PropertyChangedEventHandler PropertyChanged;

    protected virtual void OnPropertyChanged(string propertyName)
    {
        PropertyChanged?.Invoke(this, new PropertyChangedEventArgs(propertyName));
    }
}