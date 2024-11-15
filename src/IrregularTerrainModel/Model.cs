using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Runtime.CompilerServices;

namespace LongleyRice;

public class Model : INotifyPropertyChanged
{
    private double? _frequency, _surfaceRefractivity, _groundConductivity, _groundDialectric;
    protected double? _deltaH;
    private double _dbLoss;
    private RadioClimate? _climate;
    private GroundQuality? _groundQuality;
    private Polarization? _polarization;
    private ErrorCode _errorCode;

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
                OnPropertyChanged();
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
                OnPropertyChanged();
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
                if (!Enum.IsDefined(typeof(Polarization), value))
                    throw new ArgumentException($"Undefined polarization '{value}'", nameof(value));
                _polarization = value;
                OnPropertyChanged();
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
                OnPropertyChanged();
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
                OnPropertyChanged();
            }
        }
    }

    /// <summary>
    /// Electrical ground conductivity
    /// </summary>
    public double GroundConductivity
    {
        get => _groundConductivity ?? _groundConductivities[GroundQuality];
        set
        {
            if (GroundConductivity != value)
            {
                _groundConductivity = value;
                OnPropertyChanged();
            }
        }
    }

    /// <summary>
    /// Minimum monthly mean surface refractivity in N-units
    /// </summary>
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
                OnPropertyChanged();
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
                OnPropertyChanged();
            }
        }
    }

    /// <summary>
    /// Terrain irregularity
    /// </summary>
    public virtual double DeltaH
    {
        get => _deltaH ?? 0;
        set
        {
            if (DeltaH != value)
            {
                _deltaH = value;
                OnPropertyChanged();
            }
        }
    }

    public ErrorCode ErrorCode
    {
        get => _errorCode;
        set
        {
            if (ErrorCode != value)
            {
                _errorCode = value;
                OnPropertyChanged();
            }
        }
    }

    public PropMode PropMode { get; internal set; }

    public Antenna Transmitter { get; } = new Antenna { Height = 3.3 };
    public Antenna Receiver { get; } = new Antenna { Height = 1.3 };

    public Variability Variability { get; } = new Variability();

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