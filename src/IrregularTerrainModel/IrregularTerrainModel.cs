namespace LongleyRice;

/// <summary>
/// The Irregular Terrain Model (ITM) estimates radio propagation losses for frequencies between 20 MHz and 20 GHz as a function of distance and the variability of the signal in time and space
/// </summary>
public class IrregularTerrainModel
{
#if DEBUG
    /// <summary>
    /// Only used for unit testing. Do not set to true unless you're testing or want the algorithm to run slower
    /// </summary>
    public bool UseOriginal { get; set; }
#endif

    public void PointToPoint(PointToPointModel model)
    {
        var e = new double[model.Elevations.Length + 2];
        e[0] = model.Elevations.Length - 1;
        e[1] = model.Distance / e[0];
        model.Elevations.CopyTo(e, 2);
#if DEBUG
        if (UseOriginal)
        {
            var itm = new Original();
            itm.point_to_pointMDH(e, model.Transmitter.Height, model.Receiver.Height, model.GroundDielectric, model.GroundConductivity, model.SurfaceRefractivity, model.Frequency,
                (int)model.Climate, (int)model.Polarization, (int)model.Variability.Mode, model.Variability.Time, model.Variability.Location, model.Variability.Confidence,
                out var dbloss, out var propmode, out var deltaH, out var errnum);
            model.DbLoss = dbloss;
            model.PropMode = (PropMode)propmode;
            model.DeltaH = deltaH;
            model.ErrorCode = (ErrorCode)errnum;
        }
        else
#endif
        {
            var itm = new Refactored();
            itm.point_to_pointMDH(e, model.Transmitter.Height, model.Receiver.Height, model.GroundDielectric, model.GroundConductivity, model.SurfaceRefractivity, model.Frequency,
                model.Climate, model.Polarization, model.Variability.Mode, model.Variability.Time, model.Variability.Location, model.Variability.Confidence,
                out var dbloss, out var propmode, out var deltaH, out var errnum);
            model.DbLoss = dbloss;
            model.PropMode = propmode;
            model.DeltaH = deltaH;
            model.ErrorCode = errnum;
        }
    }

    public void Area(AreaModel model)
    {
#if DEBUG
        if (UseOriginal)
        {
            var itm = new Original();
            itm.area((int)model.Variability.Mode, model.DeltaH, model.Transmitter.Height, model.Receiver.Height, model.Distance, (int)model.Transmitter.SiteCriteria, (int)model.Receiver.SiteCriteria,
                model.GroundDielectric, model.GroundConductivity, model.SurfaceRefractivity, model.Frequency, (int)model.Climate, (int)model.Polarization,
                model.Variability.Time, model.Variability.Location, model.Variability.Confidence, out var dbloss, out _, out var errnum);
            model.DbLoss = dbloss;
            model.ErrorCode = (ErrorCode)errnum;
        }
        else
#endif
        {
            var itm = new Refactored();
            itm.area(model.Variability.Mode, model.DeltaH, model.Transmitter.Height, model.Receiver.Height, model.Distance, model.Transmitter.SiteCriteria, model.Receiver.SiteCriteria,
                model.GroundDielectric, model.GroundConductivity, model.SurfaceRefractivity, model.Frequency, model.Climate, model.Polarization,
                model.Variability.Time, model.Variability.Location, model.Variability.Confidence, out var dbloss, out _, out var errnum);
            model.DbLoss = dbloss;
            model.ErrorCode = errnum;
        }
    }
}