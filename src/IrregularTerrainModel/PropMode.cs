using System;

namespace LongleyRice;

[Flags]
public enum PropMode
{
    Undefined = -1,
    LineOfSight = 0,
    SingleHorizon = 4,
    DoubleHorizon = 8,
    DiffractionDominant = 1,
    TroposcatterDominant = 2,
}