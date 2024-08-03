using System;
using System.Collections.Generic;

namespace LongleyRice;

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
                OnPropertyChanged();
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
                OnPropertyChanged();
            }
        }
    }
}