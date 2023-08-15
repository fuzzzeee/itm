using System;
using System.Collections.Generic;
using System.ComponentModel;

namespace LongleyRice;

public class Model : INotifyPropertyChanged
{
    private double? _frequency, _surfaceRefractivity, _groundConductivity, _groundDialectric;
    protected double? _deltaH;
    private double _dbLoss;
    private RadioClimate? _climate;
    private GroundQuality? _groundQuality;
    private Polarization? _polarization;
    private int _errorIndicator;

    private const Polarization _defaultPolarization = Polarization.Vertical;
    private const GroundQuality _defaultGroundQuality = GroundQuality.Average;
    private const RadioClimate _defaultClimate = RadioClimate.ContinentalTemperate;
    private const double _defaultFrequency = 20;
    private static readonly Dictionary<RadioClimate, int> _surfaceRefractivities = new Dictionary<RadioClimate, int>
    {
        {RadioClimate.Equatorial, 360},
        {RadioClimate.ContinentalSubtropical, 320},
        {RadioClimate.MaritimeSubtropical, 370},
        {RadioClimate.Desert, 280},
        {RadioClimate.ContinentalTemperate, 301},
        {RadioClimate.MaritimeOverLand, 320},
        {RadioClimate.MaritimeOverSea, 350},
    };
    private static readonly Dictionary<GroundQuality, double> _groundConductivities = new Dictionary<GroundQuality, double>
    {
        {GroundQuality.Average, 0.005},
        {GroundQuality.Poor, 0.001},
        {GroundQuality.Good, 0.02},
        {GroundQuality.FreshWater, 0.01},
        {GroundQuality.SeaWater, 5},
    };
    private static readonly Dictionary<GroundQuality, int> _groundDialectrics = new Dictionary<GroundQuality, int>
    {
        {GroundQuality.Average, 15},
        {GroundQuality.Poor, 4},
        {GroundQuality.Good, 25},
        {GroundQuality.FreshWater, 81},
        {GroundQuality.SeaWater, 81},
    };

    /// <summary>
    /// Specifies the ground quality. This can be used instead of <see cref="GroundConductivity"/> and <see cref="GroundDielectric"/>
    /// </summary>
    public GroundQuality GroundQuality
    {
        get => _groundQuality ?? _defaultGroundQuality;
        set
        {
            if (GroundQuality != value)
            {
                if (!Enum.IsDefined(typeof(GroundQuality), value))
                    throw new ArgumentException($"Undefined ground quality '{value}'", nameof(value));
                _groundQuality = value;
                OnPropertyChanged(nameof(GroundQuality));
            }
        }
    }

    public RadioClimate Climate
    {
        get => _climate ?? _defaultClimate;
        set
        {
            if (Climate != value)
            {
                if (!Enum.IsDefined(typeof(RadioClimate), value))
                    throw new ArgumentException($"Undefined climate '{value}'", nameof(value));
                _climate = value;
                OnPropertyChanged(nameof(Climate));
            }
        }
    }

    public Polarization Polarization
    {
        get => _polarization ?? _defaultPolarization;
        set
        {
            if (Polarization != value)
            {
                _polarization = value;
                OnPropertyChanged(nameof(Polarization));
            }
        }
    }

    /// <summary>
    /// Frequency in Megahertz
    /// </summary>
    public double Frequency
    {
        get => _frequency ?? _defaultFrequency;
        set
        {
            if (Frequency != value)
            {
                if (value < 20 || value > 20000)
                    throw new ArgumentOutOfRangeException(nameof(value), value, "Frequency must be between 20MHz and 20GHz");
                _frequency = value;
                OnPropertyChanged(nameof(Frequency));
            }
        }
    }

    /// <summary>
    /// Dielectric Constant of Ground
    /// </summary>
    public double GroundDielectric
    {
        get => _groundDialectric ?? _groundDialectrics[GroundQuality];
        set
        {
            if (GroundDielectric != value)
            {
                _groundDialectric = value;
                OnPropertyChanged(nameof(GroundDielectric));
            }
        }
    }

    /// <summary>
    /// Conductivity of Ground
    /// </summary>
    public double GroundConductivity
    {
        get => _groundConductivity ?? _groundConductivities[GroundQuality];
        set
        {
            if (GroundConductivity != value)
            {
                _groundConductivity = value;
                OnPropertyChanged(nameof(GroundConductivity));
            }
        }
    }

    public double SurfaceRefractivity
    {
        get => _surfaceRefractivity ?? _surfaceRefractivities[Climate];
        set
        {
            if (SurfaceRefractivity != value)
            {
                if (value < 250 || value > 400)
                    throw new ArgumentOutOfRangeException(nameof(value), value, "Surface Refractivity must be between 250 and 400");
                _surfaceRefractivity = value;
                OnPropertyChanged(nameof(SurfaceRefractivity));
            }
        }
    }

    public double DbLoss
    {
        get => _dbLoss;
        set
        {
            if (DbLoss != value)
            {
                _dbLoss = value;
                OnPropertyChanged(nameof(DbLoss));
            }
        }
    }

    public virtual double DeltaH
    {
        get => _deltaH ?? 0;
        set
        {
            if (DeltaH != value)
            {
                _deltaH = value;
                OnPropertyChanged(nameof(DeltaH));
            }
        }
    }

    /// <summary>
    /// <list type="table">
    /// <item><term>0</term><description>No Error</description></item>
    /// <item><term>1</term><description>Warning: Some parameters are nearly out of range. Results should be used with caution.</description></item>
    /// <item><term>2</term><description>Note: Default parameters have been substituted for impossible ones.</description></item>
    /// <item><term>3</term><description>Warning: A combination of parameters is out of range. Results are probably invalid.</description></item>
    /// <item><term>Other</term><description>Warning: Some parameters are out of range. Results are probably invalid.</description></item>
    /// </list>
    /// </summary>
    public int ErrorIndicator
    {
        get => _errorIndicator;
        set
        {
            if (ErrorIndicator != value)
            {
                if (value < 0 || value > 4)
                    throw new ArgumentOutOfRangeException(nameof(value), value, "Error indicator must be between 0 and 4");
                _errorIndicator = value;
                OnPropertyChanged(nameof(ErrorIndicator));
            }
        }
    }

    public PropMode PropMode { get; internal set; }

    public Antenna Transmitter { get; } = new Antenna { Height = 3.3 };
    public Antenna Receiver { get; } = new Antenna { Height = 1.3 };

    public Variability Variability { get; } = new Variability();

    public event PropertyChangedEventHandler PropertyChanged;

    protected virtual void OnPropertyChanged(string propertyName)
    {
        PropertyChanged?.Invoke(this, new PropertyChangedEventArgs(propertyName));
    }
}