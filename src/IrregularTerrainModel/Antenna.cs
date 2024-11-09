using System;
using System.ComponentModel;
using System.Runtime.CompilerServices;

namespace LongleyRice;

/// <summary>
/// A transmitting or receiving antenna
/// </summary>
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

    /// <summary>
    /// Criteria describing the care taken at each terminal to assure good propagation conditions
    /// </summary>
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