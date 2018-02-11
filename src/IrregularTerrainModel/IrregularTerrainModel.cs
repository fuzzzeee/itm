using System;
using System.Collections.Generic;
using System.ComponentModel;

namespace LongleyRice
{
    public class IrregularTerrainModel
    {
        public bool UseOriginal { get; set; }

        public void PointToPoint(PointToPointModel model)
        {
            var e = new double[model.Elevations.Length + 2];
            e[0] = model.Elevations.Length - 1;
            e[1] = model.Distance / e[0];
            Buffer.BlockCopy(model.Elevations, 0, e, sizeof(double) * 2, sizeof(double) * model.Elevations.Length);
            if (UseOriginal)
            {
                var itm = new Original();
                itm.point_to_pointMDH(e, model.Transmitter.Height, model.Receiver.Height, model.GroundDielectric, model.GroundConductivity, model.SurfaceRefractivity, model.Frequency,
                    (int)model.Climate, (int)model.Polarization, model.Variability.Time, model.Variability.Location, model.Variability.Confidence,
                    out var dbloss, out var pm, out var deltaH, out var errnum);
                model.DbLoss = dbloss;
                model.PropMode = (PropMode)pm;
                model.DeltaH = deltaH;
                model.ErrorIndicator = errnum;
            }
            else
            {
                var itm = new Refactored();
                itm.point_to_pointMDH(e, model.Transmitter.Height, model.Receiver.Height, model.GroundDielectric, model.GroundConductivity, model.SurfaceRefractivity, model.Frequency,
                    model.Climate, model.Polarization, model.Variability.Time, model.Variability.Location, model.Variability.Confidence,
                    out var dbloss, out var propmode, out var deltaH, out var errnum);
                model.DbLoss = dbloss;
                model.PropMode = propmode;
                model.DeltaH = deltaH;
                model.ErrorIndicator = errnum;
            }
        }

        public void Area(AreaModel model)
        {
            if (UseOriginal)
            {
                var itm = new Original();
                itm.area((int)model.Variability.Mode, model.DeltaH, model.Transmitter.Height, model.Receiver.Height, model.Distance, (int)model.Transmitter.SiteCriteria, (int)model.Receiver.SiteCriteria,
                    model.GroundDielectric, model.GroundConductivity, model.SurfaceRefractivity, model.Frequency, (int)model.Climate, (int)model.Polarization,
                    model.Variability.Time, model.Variability.Location, model.Variability.Confidence, out var dbloss, out _, out var errnum);
                model.DbLoss = dbloss;
                model.ErrorIndicator = errnum;
            }
            else
            {
                var itm = new Refactored();
                itm.area(model.Variability.Mode, model.DeltaH, model.Transmitter.Height, model.Receiver.Height, model.Distance, model.Transmitter.SiteCriteria, model.Receiver.SiteCriteria,
                    model.GroundDielectric, model.GroundConductivity, model.SurfaceRefractivity, model.Frequency, model.Climate, model.Polarization,
                    model.Variability.Time, model.Variability.Location, model.Variability.Confidence, out var dbloss, out var errnum);
                model.DbLoss = dbloss;
                model.ErrorIndicator = errnum;
            }
        }
    }

    public class Antenna : INotifyPropertyChanged
    {
        private double _height;
        private SiteCriteria _siteCriteria = SiteCriteria.Random;

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

        public Antenna Transmitter { get; } = new Antenna { Height = 3.3 };
        public Antenna Receiver { get; } = new Antenna { Height = 1.3 };

        public Variability Variability { get; } = new Variability();

        public event PropertyChangedEventHandler PropertyChanged;

        protected virtual void OnPropertyChanged(string propertyName)
        {
            PropertyChanged?.Invoke(this, new PropertyChangedEventArgs(propertyName));
        }
    }

    public class AreaModel : Model
    {
        private double _distance;
        private TerrainType? _terrainType;
        private const TerrainType _defaultTerrainType = TerrainType.Hills;
        private static readonly Dictionary<TerrainType, int> _terrainTypes = new Dictionary<TerrainType, int>
        {
            {TerrainType.Flat, 0},
            {TerrainType.Plains, 30},
            {TerrainType.Hills, 90},
            {TerrainType.Mountains, 200},
            {TerrainType.RuggedMountains, 500},
        };

        /// <summary>
        /// Creates a new <see cref="AreaModel"/>
        /// </summary>
        /// <param name="distance">Distance in kilometers</param>
        public AreaModel(double distance)
        {
            Distance = distance;
        }

        /// <summary>
        /// Distance in kilometers
        /// </summary>
        public double Distance
        {
            get => _distance;
            set
            {
                if (Distance != value)
                {
                    if (value <= 0)
                        throw new ArgumentException("Distance must be positive");
                    _distance = value;
                    OnPropertyChanged(nameof(Distance));
                }
            }
        }

        /// <inheritdoc />
        public override double DeltaH
        {
            get => _deltaH ?? _terrainTypes[TerrainType];
            set => base.DeltaH = value;
        }

        public TerrainType TerrainType
        {
            get => _terrainType ?? _defaultTerrainType;
            set
            {
                if (TerrainType != value)
                {
                    _terrainType = value;
                    OnPropertyChanged(nameof(TerrainType));
                }
            }
        }
    }

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

        public PropMode PropMode { get; internal set; }
    }

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
            return _levels[level] ?? _defaultValue;
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

        public event PropertyChangedEventHandler PropertyChanged;

        protected virtual void OnPropertyChanged(string propertyName)
        {
            PropertyChanged?.Invoke(this, new PropertyChangedEventArgs(propertyName));
        }
    }
}
