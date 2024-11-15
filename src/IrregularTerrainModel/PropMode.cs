using System;

namespace LongleyRice;

[Flags]
public enum PropMode
{
    LineOfSight = 0,
    SingleHorizon = 4,
    DoubleHorizon = 8,
    DiffractionDominant = 1,
    TroposcatterDominant = 2,
}