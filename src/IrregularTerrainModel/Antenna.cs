using System;
using System.ComponentModel;
using System.Runtime.CompilerServices;

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
                if (value < 0.5 || value > 3000)
                    throw new ArgumentOutOfRangeException(nameof(value), value, "Height must be between 50cm and 3km");
                _height = value;
                OnPropertyChanged();
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
                OnPropertyChanged();
            }
        }
    }

    public event PropertyChangedEventHandler PropertyChanged;

    protected virtual void OnPropertyChanged([CallerMemberName] string propertyName = null)
    {
        PropertyChanged?.Invoke(this, new PropertyChangedEventArgs(propertyName));
    }
}