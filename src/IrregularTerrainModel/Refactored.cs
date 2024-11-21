using System;
using System.Diagnostics.CodeAnalysis;
using static System.Math;
using System.Numerics;

namespace LongleyRice;

[SuppressMessage("ReSharper", "InconsistentNaming")]
[SuppressMessage("ReSharper", "IdentifierTypo")]
[SuppressMessage("ReSharper", "CompareOfFloatsByEqualityOperator")]
internal class Refactored
{
    private class Elevations
    {
        public Elevations(double[] elevations)
        {
            EndIndex = (int)elevations[0];
            DeltaDistance = elevations[1];
            Points = new double[elevations.Length - 2];
            Buffer.BlockCopy(elevations, sizeof(double) * 2, Points, 0, sizeof(double) * Points.Length);
        }

        public Elevations(int length, double deltaDistance)
        {
            EndIndex = length - 1;
            DeltaDistance = deltaDistance;
            Points = new double[length];
        }

        public readonly int EndIndex;
        public readonly double DeltaDistance;
        public readonly double[] Points;
        public double FirstPoint => Points[0];
        public double LastPoint => Points[EndIndex];
    }

    private class Antenna
    {
        /// <summary>
        /// Structural height
        /// </summary>
        public double StructuralHeight;
        /// <summary>
        /// Effective height
        /// </summary>
        public double EffectiveHeight;
        /// <summary>
        /// Horizon distance
        /// </summary>
        public double HorizonDistance;
        /// <summary>
        /// Horizon elevation angle
        /// </summary>
        public double HorizonElevationAngle;
        /// <summary>
        /// Smooth earth horizon distance
        /// </summary>
        public double SmoothEarthHorizonDistance;
        /// <summary>
        /// Degree of care taken in selecting the antenna site
        /// </summary>
        public SiteCriteria SiteCriteria;
        public double xl;
    }

    // *************************************
    // C++ routines for this program are taken from
    // a translation of the FORTRAN code written by
    // U.S. Department of Commerce NTIA/ITS
    // Institute for Telecommunication Sciences
    // *****************
    // Irregular Terrain Model (ITM) (Longley-Rice)
    // *************************************


    private const double Third = 1.0 / 3.0;

    private readonly Antenna Transmitter = new Antenna();
    private readonly Antenna Receiver = new Antenna();
    private readonly Antenna[] Antennae;

    public Refactored()
    {
        Antennae = [Transmitter, Receiver];
    }

    /// <summary>
    /// Reference attenuation
    /// </summary>
    private double _referenceAttenuation;
    /// <summary>
    /// Distance
    /// </summary>
    private double _distance;
    /// <summary>
    /// Wave number (radio frequency)
    /// </summary>
    private double _waveNumber;
    /// <summary>
    /// Terrain irregularity parameter
    /// </summary>
    private double _terrainIrregularity;
    /// <summary>
    /// Surface refractivity
    /// </summary>
    private double _surfaceRefractivity;
    /// <summary>
    /// Earth's effective curvature, measured in units of reciprocal length
    /// </summary>
    private double _earthsEffectiveCurvature;
    /// <summary>
    /// Surface transfer impedance of the ground
    /// </summary>
    private Complex _groundImpedance;
    /// <summary>
    /// Error indicator
    /// </summary>
    private ErrorCode _errorCode;
    /// <summary>
    /// Controlling mode
    /// </summary>
    private ControlFlow _controlFlow;
    /// <summary>
    /// Standard deviation of situation variability (with what confidence will a threshold signal level be exceeded)
    /// </summary>
    private double _sgc;
    /// <summary>
    /// A control switch
    /// </summary>
    private Changes _changes;
    /// <summary>
    /// Desired mode of variability
    /// </summary>
    private VariabilityMode _variabilityMode;
    /// <summary>
    /// Climate indicator
    /// </summary>
    private RadioClimate _radioClimate;
    /// <summary>
    /// Line-of-sight distance
    /// </summary>
    private double _lineOfSightDistance;
    /// <summary>
    /// Scatter distance
    /// </summary>
    private double _scatterDistance;
    /// <summary>
    /// Line-of-sight coefficient
    /// </summary>
    private double _lineOfSightCoefficientEL;
    /// <summary>
    /// Line-of-sight coefficient
    /// </summary>
    private double _lineOfSightCoefficientK1;
    /// <summary>
    /// Line-of-sight coefficient
    /// </summary>
    private double _lineOfSightCoefficientK2;
    /// <summary>
    /// Diffraction coefficient
    /// </summary>
    private double _diffractionCoefficientAED;
    /// <summary>
    /// Diffraction coefficient
    /// </summary>
    private double _diffractionCoefficientEMD;
    /// <summary>
    /// Scatter coefficient
    /// </summary>
    private double _scatterCoefficientAE;
    /// <summary>
    /// Scatter coefficient
    /// </summary>
    private double _scatterCoefficientEM;
    /// <summary>
    /// Total horizon distance
    /// </summary>
    private double _totalHorizonDistance;
    /// <summary>
    /// Total bending angle
    /// </summary>
    private double _totalBendingAngle;

    private double _ad, _rr, _etq, _h0s;

    private double _dexa, _de, _vmd, _vs0, _sglocation, _sgtime_m, _sgtime_p, _sgtime_d, _tgtd, _gm, _gp;

    private double _dmin, _xae;

    private double _wls;

    private ClimateSettings _cs;

    private bool _w1;

    private double _wd1, _xd1, _afo, _qk, _aht, _xht;

    private bool _wlos, _wscat;

    /// <summary>
    /// This performs the FORTRAN DIM function.
    /// </summary>
    /// <returns><paramref name="x"/> - <paramref name="y"/> if <paramref name="x"/> is greater than <paramref name="y"/>, otherwise 0</returns>
    private static double Dim(double x, double y) => x > y ? x - y : 0;

    /// <summary>
    /// The attenuation due to a single knife edge - the Fresnel integral (in decibels) as a function of <paramref name="v2"/>
    /// </summary>
    private static double KnifeEdgeAttenuation(double v2)
    {
        if (v2 < 5.76)
            return 6.02 + 9.11 * Sqrt(v2) - 1.27 * v2;
        else
            return 12.953 + 4.343 * Log(v2);
    }

    /// <summary>
    /// The height-gain over a smooth spherical earth - to be used in the "three radii" method.
    /// </summary>
    /// <param name="x"></param>
    /// <param name="pk"></param>
    /// <returns></returns>
    private static double HeightGain(double x, double pk)
    {
        double heightGain;
        if (x < 200)
        {
            var w = -Log(pk);
            if (pk < 1e-5 || x * Pow(w, 3) > 5495)
            {
                heightGain = -117;
                if (x > 1)
                    heightGain = 17.372 * Log(x) + heightGain;
            }
            else
            {
                heightGain = 2.5e-5 * x * x / pk - 8.686 * w - 15;
            }
        }
        else
        {
            heightGain = 0.05751 * x - 4.343 * Log(x);
            if (x < 2000)
            {
                var w = 0.0134 * x * Exp(-0.005 * x);
                heightGain = (1 - w) * heightGain + w * (17.372 * Log(x) - 117);
            }
        }
        return heightGain;
    }

    private static readonly int[] _h0f_a = [25, 80, 177, 395, 705];
    private static readonly int[] _h0f_b = [24, 45, 68, 80, 105];

    /// <summary>
    /// H01 function for scatter fields
    /// </summary>
    private static double ScatterFieldsH01(double r, double et)
    {
        double q;
        var it = (int)et;
        if (it <= 0)
        {
            it = 1;
            q = 0;
        }
        else if (it >= 5)
        {
            it = 5;
            q = 0;
        }
        else
            q = et - it;
        var x = Pow(1 / r, 2);
        var h0fv = 4.343 * Log((_h0f_a[it - 1] * x + _h0f_b[it - 1]) * x + 1);
        if (q != 0)
            h0fv = (1 - q) * h0fv + q * 4.343 * Log((_h0f_a[it] * x + _h0f_b[it]) * x + 1);
        return h0fv;
    }

    private static readonly double[] _ahd_a = [133.4, 104.6, 71.8];
    private static readonly double[] _ahd_b = [0.332e-3, 0.212e-3, 0.157e-3];
    private static readonly double[] _ahd_c = [-4.343, -1.086, 2.171];

    /// <summary>
    /// The F(0d) function for scatter fields
    /// </summary>
    private static double ScatterFieldsF0d(double distance)
    {
        var index = distance switch
        {
            <= 10000 => 0,
            <= 70000 => 1,
            _ => 2
        };
        return _ahd_a[index] + _ahd_b[index] * distance + _ahd_c[index] * Log(distance);
    }

    private void SetupDiffractionAttenuation()
    {
        var q = Transmitter.StructuralHeight * Receiver.StructuralHeight;
        _qk = Transmitter.EffectiveHeight * Receiver.EffectiveHeight - q;
        if (_controlFlow == ControlFlow.PointToPoint)
            q += 10;
        _wd1 = Sqrt(1 + _qk / q);
        _xd1 = _totalHorizonDistance + _totalBendingAngle / _earthsEffectiveCurvature;
        q = (1 - 0.8 * Exp(-_lineOfSightDistance / 50000)) * _terrainIrregularity;
        q *= 0.78 * Exp(-Pow(q / 16, 0.25));
        _afo = Min(15, 2.171 * Log(1 + 4.77e-4 * Transmitter.StructuralHeight * Receiver.StructuralHeight * _waveNumber * q));
        _qk = 1 / Complex.Abs(_groundImpedance);
        _aht = 20;
        _xht = 0;
        foreach (var antenna in Antennae)
        {
            var a = 0.5 * Pow(antenna.HorizonDistance, 2) / antenna.EffectiveHeight;
            var wa = Pow(a * _waveNumber, Third);
            var pk = _qk / wa;
            q = (1.607 - pk) * 151 * wa * antenna.HorizonDistance / a;
            _xht += q;
            _aht += HeightGain(q, pk);
        }
    }

    /// <summary>
    /// Finds the "diffraction attenuation" at the <paramref name="distance"/>
    /// </summary>
    /// <remarks>
    /// It uses a convex combination of smooth earth diffraction and double knife-edge diffraction
    /// </remarks>
    private double DiffractionAttenuation(double distance)
    {
        var th = _totalBendingAngle + distance * _earthsEffectiveCurvature;
        var ds = distance - _totalHorizonDistance;
        var q = 0.0795775 * _waveNumber * ds * Pow(th, 2);
        var adiffv = KnifeEdgeAttenuation(q * Transmitter.HorizonDistance / (ds + Transmitter.HorizonDistance)) + KnifeEdgeAttenuation(q * Receiver.HorizonDistance / (ds + Receiver.HorizonDistance));
        var a = ds / th;
        var wa = Pow(a * _waveNumber, Third);
        var pk = _qk / wa;
        q = (1.607 - pk) * 151 * wa * th + _xht;
        var ar = 0.05751 * q - 4.343 * Log(q) - _aht;
        q = (_wd1 + _xd1 / distance) * Min((1 - 0.8 * Exp(-distance / 50000)) * _terrainIrregularity * _waveNumber, 6283.2);
        var wd = 25.1 / (25.1 + Sqrt(q));
        return ar * wd + (1 - wd) * adiffv + _afo;
    }

    /// <summary>
    /// Finds the "scatter attenuation" at the <paramref name="distance"/>. It uses an approximation to the methods of NBS TN101 with checks for inadmissible situations
    /// </summary>
    /// <param name="distance"></param>
    /// <returns></returns>
    private double ScatterAttenuation(double distance)
    {
        double th;
        double h0;
        if (_h0s > 15)
            h0 = _h0s;
        else
        {
            th = Transmitter.HorizonElevationAngle + Receiver.HorizonElevationAngle + distance * _earthsEffectiveCurvature;
            var r2 = 2 * _waveNumber * th;
            var r1 = r2 * Transmitter.EffectiveHeight;
            r2 *= Receiver.EffectiveHeight;
            if (r1 < 0.2 && r2 < 0.2)
                return 1001;  // <==== early return
            var ss = (distance - _ad) / (distance + _ad);
            var q = _rr / ss;
            ss = Max(0.1, ss);
            q = Min(Max(0.1, q), 10);
            var z0 = (distance - _ad) * (distance + _ad) * th * 0.25 / distance;
            var et = (_etq * Exp(-Pow(Min(1.7, z0 / 8000), 6)) + 1) * z0 / 1.7556e3;
            var ett = Max(et, 1);
            h0 = (ScatterFieldsH01(r1, ett) + ScatterFieldsH01(r2, ett)) * 0.5;
            h0 += Min(h0, (1.38 - Log(ett)) * Log(ss) * Log(q) * 0.49);
            h0 = Dim(h0, 0);
            if (et < 1)
                h0 = et * h0 + (1 - et) * 4.343 * Log(Pow((1 + 1.4142 / r1) * (1 + 1.4142 / r2), 2) * (r1 + r2) / (r1 + r2 + 2.8284));
            if (h0 > 15 && _h0s >= 0)
                h0 = _h0s;
        }
        _h0s = h0;
        th = _totalBendingAngle + distance * _earthsEffectiveCurvature;
        return ScatterFieldsF0d(th * distance) + 4.343 * Log(47.7 * _waveNumber * Pow(th, 4)) - 0.1 * (_surfaceRefractivity - 301) * Exp(-th * distance / 40000) + h0;
    }

    private static double qerfi(double percent)
    {
        var x = 0.5 - percent;
        var t = Max(0.5 - Abs(x), 0.000001);
        t = Sqrt(-2 * Log(t));
        var v = t - ((0.010328 * t + 0.802853) * t + 2.515516698) / (((0.001308 * t + 0.189269) * t + 1.432788) * t + 1);
        if (x < 0)
            v = -v;
        return v;
    }

    private void qlrps(double fmhz, double zsys, double surfaceRefractivity, Polarization polarization, double epsDielect, double sgmConductivity)
    {
        _waveNumber = fmhz / 47.7;
        _surfaceRefractivity = surfaceRefractivity;
        if (zsys != 0)
            _surfaceRefractivity *= Exp(-zsys / 9460);
        _earthsEffectiveCurvature = 157e-9 * (1 - 0.04665 * Exp(_surfaceRefractivity / 179.3));
        var zq = new Complex(epsDielect, 376.62 * sgmConductivity / _waveNumber);
        var groundImpedance = Complex.Sqrt(zq - 1);
        if (polarization != Polarization.Horizontal)
            groundImpedance = groundImpedance / zq;
        _groundImpedance = groundImpedance;
    }

    private static double abq_alos(Complex r)
    {
        return r.Real * r.Real + r.Imaginary * r.Imaginary;
    }

    /// <summary>
    /// Finds the "line-of-sight" attenuation at the <paramref name="distance"/> It uses a convex combination of plane earth fields and diffracted fields
    /// </summary>
    private double LineOfSightAttenuation(double distance)
    {
        var q = (1 - 0.8 * Exp(-distance / 50000)) * _terrainIrregularity;
        var s = 0.78 * q * Exp(-Pow(q / 16, 0.25));
        q = Transmitter.EffectiveHeight + Receiver.EffectiveHeight;
        var sps = q / Sqrt(distance * distance + q * q);
        var r = (sps - _groundImpedance) / (sps + _groundImpedance) * Exp(-Min(10, _waveNumber * s * sps));
        q = abq_alos(r);
        if (q < 0.25 || q < sps)
            r = r * Sqrt(sps / q);
        var alosv = _diffractionCoefficientEMD * distance + _diffractionCoefficientAED;
        q = _waveNumber * Transmitter.EffectiveHeight * Receiver.EffectiveHeight * 2 / distance;
        if (q > 1.57)
            q = 3.14 - 2.4649 / q;
        return (-4.343 * Log(abq_alos(new Complex(Cos(q), -Sin(q)) + r)) - alosv) * _wls + alosv;
    }

    private void qlra(RadioClimate radioClimate, VariabilityMode variabilityMode)
    {
        foreach (var antenna in Antennae)
        {
            double q;
            if (antenna.SiteCriteria == SiteCriteria.Random)
                antenna.EffectiveHeight = antenna.StructuralHeight;
            else
            {
                q = antenna.SiteCriteria != SiteCriteria.Careful ? 9 : 4;
                if (antenna.StructuralHeight < 5)
                    q *= Sin(0.3141593 * antenna.StructuralHeight); // 0.3141593 should probably be (PI / 10)
                antenna.EffectiveHeight = antenna.StructuralHeight + (1 + q) * Exp(-Min(20, 2 * antenna.StructuralHeight / Max(1e-3, _terrainIrregularity)));
            }
            q = Sqrt(2 * antenna.EffectiveHeight / _earthsEffectiveCurvature);
            antenna.HorizonDistance = q * Exp(-0.07 * Sqrt(_terrainIrregularity / Max(antenna.EffectiveHeight, 5)));
            antenna.HorizonElevationAngle = (0.65 * _terrainIrregularity * (q / antenna.HorizonDistance - 1) - 2 * antenna.EffectiveHeight) / q;
        }
        _controlFlow = ControlFlow.AreaBegin;
        _variabilityMode = variabilityMode;
        _radioClimate = radioClimate;
        _changes = Changes.All;
    }

    /// <summary>
    /// The Longley-Rice propagation program. This is the basic program; it returns the reference attenuation
    /// </summary>
    /// <param name="distance"></param>
    private void ReferenceAttenuation(double distance)  // PaulM_lrprop
    {
        if (_controlFlow != ControlFlow.AreaContinue)
        {
            foreach (var antenna in Antennae)
            {
                antenna.SmoothEarthHorizonDistance = Sqrt(2 * antenna.EffectiveHeight / _earthsEffectiveCurvature);
            }
            _lineOfSightDistance = Transmitter.SmoothEarthHorizonDistance + Receiver.SmoothEarthHorizonDistance;
            _totalHorizonDistance = Transmitter.HorizonDistance + Receiver.HorizonDistance;
            _totalBendingAngle = Max(Transmitter.HorizonElevationAngle + Receiver.HorizonElevationAngle, -_totalHorizonDistance * _earthsEffectiveCurvature);
            _wlos = false;
            _wscat = false;
            if (_waveNumber < 0.838 || _waveNumber > 210)
                SetErrorCode(ErrorCode.NearlyOutOfRange);
            foreach (var antenna in Antennae)
            {
                if (antenna.StructuralHeight < 1 || antenna.StructuralHeight > 1000)
                {
                    SetErrorCode(ErrorCode.NearlyOutOfRange);
                }
            }
            foreach (var antenna in Antennae)
            {
                if (Abs(antenna.HorizonElevationAngle) > 200e-3 || antenna.HorizonDistance < 0.1 * antenna.SmoothEarthHorizonDistance ||
                    antenna.HorizonDistance > 3 * antenna.SmoothEarthHorizonDistance)
                {
                    SetErrorCode(ErrorCode.CombinationOutOfRange);
                }
            }
            if (_surfaceRefractivity < 250 || _surfaceRefractivity > 400 ||
                _earthsEffectiveCurvature < 75e-9 || _earthsEffectiveCurvature > 250e-9 ||
                _groundImpedance.Real <= Abs(_groundImpedance.Imaginary) ||
                _waveNumber < 0.419 || _waveNumber > 420)
            {
                SetErrorCode(ErrorCode.SomeOutOfRange);
            }
            foreach (var antenna in Antennae)
            {
                if (antenna.StructuralHeight < 0.5 || antenna.StructuralHeight > 3000)
                {
                    SetErrorCode(ErrorCode.SomeOutOfRange);
                }
            }
            _dmin = Abs(Transmitter.EffectiveHeight - Receiver.EffectiveHeight) / 200e-3;
            SetupDiffractionAttenuation();
            _xae = Pow(_waveNumber * Pow(_earthsEffectiveCurvature, 2), -Third);
            var d3 = Max(_lineOfSightDistance, 1.3787 * _xae + _totalHorizonDistance);
            var d4 = d3 + 2.7574 * _xae;
            var a3 = DiffractionAttenuation(d3);
            var a4 = DiffractionAttenuation(d4);
            _diffractionCoefficientEMD = (a4 - a3) / (d4 - d3);
            _diffractionCoefficientAED = a3 - _diffractionCoefficientEMD * d3;
        }
        if (_controlFlow != ControlFlow.PointToPoint)
        {
            _controlFlow = ControlFlow.PointToPoint;
            _distance = distance;
        }
        if (_distance > 0)
        {
            if (_distance > 1000000)
                SetErrorCode(ErrorCode.NearlyOutOfRange);
            if (_distance < _dmin)
                SetErrorCode(ErrorCode.CombinationOutOfRange);
            if (_distance < 1000 || _distance > 2000000)
                SetErrorCode(ErrorCode.SomeOutOfRange);
        }
        if (_distance < _lineOfSightDistance)
        {
            if (!_wlos)
            {
                _wls = 0.021 / (0.021 + _waveNumber * _terrainIrregularity / Max(10000, _lineOfSightDistance));
                var lineOfSightDistance = _lineOfSightDistance;
                var a2 = _diffractionCoefficientAED + lineOfSightDistance * _diffractionCoefficientEMD;
                var d0 = 1.908 * _waveNumber * Transmitter.EffectiveHeight * Receiver.EffectiveHeight;
                double d1;
                if (_diffractionCoefficientAED >= 0)
                {
                    d0 = Min(d0, 0.5 * _totalHorizonDistance);
                    d1 = d0 + 0.25 * (_totalHorizonDistance - d0);
                }
                else
                    d1 = Max(-_diffractionCoefficientAED / _diffractionCoefficientEMD, 0.25 * _totalHorizonDistance);
                var a1 = LineOfSightAttenuation(d1);
                var wq = false;
                if (d0 < d1)
                {
                    var a0 = LineOfSightAttenuation(d0);
                    var q = Log(lineOfSightDistance / d0);
                    _lineOfSightCoefficientK2 = Max(0, ((lineOfSightDistance - d0) * (a1 - a0) - (d1 - d0) * (a2 - a0)) / ((lineOfSightDistance - d0) * Log(d1 / d0) - (d1 - d0) * q));
                    wq = _diffractionCoefficientAED >= 0 || _lineOfSightCoefficientK2 > 0;
                    if (wq)
                    {
                        _lineOfSightCoefficientK1 = (a2 - a0 - _lineOfSightCoefficientK2 * q) / (lineOfSightDistance - d0);
                        if (_lineOfSightCoefficientK1 < 0)
                        {
                            _lineOfSightCoefficientK1 = 0;
                            _lineOfSightCoefficientK2 = Dim(a2, a0) / q;
                            if (_lineOfSightCoefficientK2 == 0)
                                _lineOfSightCoefficientK1 = _diffractionCoefficientEMD;
                        }
                    }
                }
                if (!wq)
                {
                    _lineOfSightCoefficientK1 = Dim(a2, a1) / (lineOfSightDistance - d1);
                    _lineOfSightCoefficientK2 = 0;
                    if (_lineOfSightCoefficientK1 == 0)
                        _lineOfSightCoefficientK1 = _diffractionCoefficientEMD;
                }
                _lineOfSightCoefficientEL = a2 - _lineOfSightCoefficientK1 * lineOfSightDistance - _lineOfSightCoefficientK2 * Log(lineOfSightDistance);
                _wlos = true;
            }
            if (_distance > 0)
                _referenceAttenuation = _lineOfSightCoefficientEL + _lineOfSightCoefficientK1 * _distance + _lineOfSightCoefficientK2 * Log(_distance);
        }
        if (_distance <= 0 || _distance >= _lineOfSightDistance)
        {
            if (!_wscat)
            {
                _ad = Transmitter.HorizonDistance - Receiver.HorizonDistance;
                _rr = Receiver.EffectiveHeight / Transmitter.EffectiveHeight;
                if (_ad < 0)
                {
                    _ad = -_ad;
                    _rr = 1 / _rr;
                }
                _etq = (5.67e-6 * _surfaceRefractivity - 2.32e-3) * _surfaceRefractivity + 0.031;
                _h0s = -15;

                var d5 = _totalHorizonDistance + 200000;
                var d6 = d5 + 200000;
                var a6 = ScatterAttenuation(d6);
                var a5 = ScatterAttenuation(d5);
                if (a5 < 1000)
                {
                    _scatterCoefficientEM = (a6 - a5) / 200000;
                    _scatterDistance = Max(_lineOfSightDistance, Max(_totalHorizonDistance + 0.3 * _xae * Log(47.7 * _waveNumber), (a5 - _diffractionCoefficientAED - _scatterCoefficientEM * d5) / (_diffractionCoefficientEMD - _scatterCoefficientEM)));
                    _scatterCoefficientAE = (_diffractionCoefficientEMD - _scatterCoefficientEM) * _scatterDistance + _diffractionCoefficientAED;
                }
                else
                {
                    _scatterCoefficientEM = _diffractionCoefficientEMD;
                    _scatterCoefficientAE = _diffractionCoefficientAED;
                    _scatterDistance = 10000000;
                }
                _wscat = true;
            }
            if (_distance > _scatterDistance)
                _referenceAttenuation = _scatterCoefficientAE + _scatterCoefficientEM * _distance;
            else
                _referenceAttenuation = _diffractionCoefficientAED + _diffractionCoefficientEMD * _distance;
        }
        _referenceAttenuation = Max(_referenceAttenuation, 0);
    }

    private static double curve(double c1, double c2, double x1, double x2, double x3, double de)
    {
        return (c1 + c2 / (1 + Pow((de - x2) / x3, 2))) * Pow(de / x1, 2) / (1 + Pow(de / x1, 2));
    }

    private class ClimateSettings
    {
        public ClimateSettings(double bv1, double bv2, double xv1, double xv2, double xv3, double bsm1, double bsm2,
            double xsm1, double xsm2, double xsm3, double bsp1, double bsp2, double xsp1, double xsp2, double xsp3,
            double bsd1, double bzd1, double bfm1, double bfm2, double bfm3, double bfp1, double bfp2, double bfp3)
        {
            cv1 = bv1; cv2 = bv2; yv1 = xv1; yv2 = xv2; yv3 = xv3; csm1 = bsm1; csm2 = bsm2; ysm1 = xsm1; ysm2 = xsm2;
            ysm3 = xsm3; csp1 = bsp1; csp2 = bsp2; ysp1 = xsp1; ysp2 = xsp2; ysp3 = xsp3; csd1 = bsd1; zd = bzd1;
            cfm1 = bfm1; cfm2 = bfm2; cfm3 = bfm3; cfp1 = bfp1; cfp2 = bfp2; cfp3 = bfp3;
        }
        public readonly double cv1, cv2, yv1, yv2, yv3, csm1, csm2, ysm1, ysm2, ysm3, csp1, csp2, ysp1, ysp2, ysp3,
            csd1, zd, cfm1, cfm2, cfm3, cfp1, cfp2, cfp3;
    }

    private static readonly ClimateSettings[] _climateSettings =
    [
        new ClimateSettings (-9.67, 12.7, 144.9e3, 190.3e3, 133.8e3, 2.13, 159.5, 762.2e3, 123.6e3, 94.5e3, 2.11, 102.3, 636.9e3, 134.8e3, 95.6e3, 1.224, 1.282, 1, 0, 0, 1, 0, 0 ),
        new ClimateSettings (-0.62, 9.19, 228.9e3, 205.2e3, 143.6e3, 2.66, 7.67, 100.4e3, 172.5e3, 136.4e3, 6.87, 15.53, 138.7e3, 143.7e3, 98.6e3, 0.801, 2.161, 1, 0, 0, 0.93, 0.31, 2 ),
        new ClimateSettings (1.26, 15.5, 262.6e3, 185.2e3, 99.8e3, 6.11, 6.65, 138.2e3, 242.2e3, 178.6e3, 10.08, 9.60, 165.3e3, 225.7e3, 129.7e3, 1.380, 1.282, 1, 0, 0, 1, 0, 0 ),
        new ClimateSettings (-9.21, 9.05, 84.1e3, 101.1e3, 98.6e3, 1.98, 13.11, 139.1e3, 132.7e3, 193.5e3, 3.68, 159.3, 464.4e3, 93.1e3, 94.2e3, 1, 20, 1, 0, 0, 0.93, 0.19, 1.79 ),
        new ClimateSettings (-0.62, 9.19, 228.9e3, 205.2e3, 143.6e3, 2.68, 7.16, 93.7e3, 186.8e3, 133.5e3, 4.75, 8.12, 93.2e3, 135.9e3, 113.4e3, 1.224, 1.282, 0.92, 0.25, 1.77, 0.93, 0.31, 2 ),
        new ClimateSettings (-0.39, 2.86, 141.7e3, 315.9e3, 167.4e3, 6.86, 10.38, 187.8e3, 169.6e3, 108.9e3, 8.58, 13.97, 216.0e3, 152.0e3, 122.7e3, 1.518, 1.282, 1, 0, 0, 1, 0, 0 ),
        new ClimateSettings (3.15, 857.9, 2222.0e3, 164.8e3, 116.3e3, 8.51, 169.8, 609.8e3, 119.9e3, 106.6e3, 8.43, 8.19, 136.2e3, 188.5e3, 122.9e3, 1.518, 1.282, 1, 0, 0, 1, 0, 0 )
    ];

    private double avar(double timePercent, double locationPercent, double confidencePercent)
    {
        if (_changes > Changes.None)
        {
            switch (_changes)
            {
                case Changes.All:
                    _cs = _climateSettings[(int)_radioClimate - 1];
                    goto case Changes.VariabilityMode;
                case Changes.VariabilityMode:
                case Changes.Frequency:
                    var q = Log(0.133 * _waveNumber);
                    _gm = _cs.cfm1 + _cs.cfm2 / (Pow(_cs.cfm3 * q, 2) + 1);
                    _gp = _cs.cfp1 + _cs.cfp2 / (Pow(_cs.cfp3 * q, 2) + 1);
                    goto case Changes.AntennaHeight;
                case Changes.AntennaHeight:
                    _dexa = Sqrt(18000000 * Transmitter.EffectiveHeight) + Sqrt(18000000 * Receiver.EffectiveHeight) + Pow(575.7e12 / _waveNumber, Third);
                    goto case Changes.Distance;
                case Changes.Distance:
                    if (_distance < _dexa)
                        _de = 130000 * _distance / _dexa;
                    else
                        _de = 130000 + _distance - _dexa;
                    break;
            }
            _vmd = curve(_cs.cv1, _cs.cv2, _cs.yv1, _cs.yv2, _cs.yv3, _de);
            _sgtime_m = curve(_cs.csm1, _cs.csm2, _cs.ysm1, _cs.ysm2, _cs.ysm3, _de) * _gm;
            _sgtime_p = curve(_cs.csp1, _cs.csp2, _cs.ysp1, _cs.ysp2, _cs.ysp3, _de) * _gp;
            _sgtime_d = _sgtime_p * _cs.csd1;
            _tgtd = (_sgtime_p - _sgtime_d) * _cs.zd;
            if (_w1)
            {
                _sglocation = 0;
            }
            else
            {
                var q = (1 - 0.8 * Exp(-_distance / 50000)) * _terrainIrregularity * _waveNumber;
                _sglocation = 10 * q / (q + 13);
            }
            _vs0 = Pow(5 + 3 * Exp(-_de / 100000), 2);
            _changes = Changes.None;
        }
        var ztime = timePercent;
        var zlocation = locationPercent;
        var zconfidence = confidencePercent;
        switch (_variabilityMode)
        {
            case VariabilityMode.Single:
                ztime = zconfidence;
                zlocation = zconfidence;
                break;
            case VariabilityMode.Individual:
                zlocation = zconfidence;
                break;
            case VariabilityMode.Mobile:
                zlocation = ztime;
                break;
        }
        if (Abs(ztime) > 3.1 || Abs(zlocation) > 3.1 || Abs(zconfidence) > 3.1)
        {
            SetErrorCode(ErrorCode.NearlyOutOfRange);
        }
        double sgtime;
        if (ztime < 0)
            sgtime = _sgtime_m;
        else if (ztime <= _cs.zd)
            sgtime = _sgtime_p;
        else
            sgtime = _sgtime_d + _tgtd / ztime;
        var vs = _vs0 + Pow(sgtime * ztime, 2) / (7.8 + zconfidence * zconfidence) + Pow(_sglocation * zlocation, 2) / (24 + zconfidence * zconfidence);
        double yr;
        switch (_variabilityMode)
        {
            case VariabilityMode.Single:
                yr = 0;
                _sgc = Sqrt(sgtime * sgtime + _sglocation * _sglocation + vs);
                break;
            case VariabilityMode.Individual:
                yr = sgtime * ztime;
                _sgc = Sqrt(_sglocation * _sglocation + vs);
                break;
            case VariabilityMode.Mobile:
                yr = Sqrt(sgtime * sgtime + _sglocation * _sglocation) * ztime;
                _sgc = Sqrt(vs);
                break;
            default: // VariabilityMode.Broadcast
                yr = sgtime * ztime + _sglocation * zlocation;
                _sgc = Sqrt(vs);
                break;
        }
        var avarv = _referenceAttenuation - _vmd - yr - _sgc * zconfidence;
        if (avarv < 0)
            avarv = avarv * (29 - avarv) / (29 - 10 * avarv);
        return avarv;
    }

    private void hzns(Elevations elevations)
    {
        var endIndex = elevations.EndIndex;
        var deltaDistance = elevations.DeltaDistance;
        var transmitterHeight = elevations.FirstPoint + Transmitter.StructuralHeight;
        var receiverHeight = elevations.LastPoint + Receiver.StructuralHeight;
        var halfCurvature = 0.5 * _earthsEffectiveCurvature;
        var q = halfCurvature * _distance;
        Receiver.HorizonElevationAngle = (receiverHeight - transmitterHeight) / _distance;
        Transmitter.HorizonElevationAngle = Receiver.HorizonElevationAngle - q;
        Receiver.HorizonElevationAngle = -Receiver.HorizonElevationAngle - q;
        Transmitter.HorizonDistance = _distance;
        Receiver.HorizonDistance = _distance;
        if (endIndex >= 2)
        {
            var sa = 0.0;
            var distance = _distance;
            var wq = true;
            for (var i = 1; i < endIndex; i++)
            {
                sa += deltaDistance;
                distance -= deltaDistance;
                q = elevations.Points[i] - (halfCurvature * sa + Transmitter.HorizonElevationAngle) * sa - transmitterHeight;
                if (q > 0)
                {
                    Transmitter.HorizonElevationAngle += q / sa;
                    Transmitter.HorizonDistance = sa;
                    wq = false;
                }
                if (!wq)
                {
                    q = elevations.Points[i] - (halfCurvature * distance + Receiver.HorizonElevationAngle) * distance - receiverHeight;
                    if (q > 0)
                    {
                        Receiver.HorizonElevationAngle += q / distance;
                        Receiver.HorizonDistance = distance;
                    }
                }
            }
        }
    }

    private static void z1sq1(Elevations elevations, double x1, double x2, out double z0, out double zn)
    {
        double endIndex = elevations.EndIndex;
        double xa = (int)Dim(x1 / elevations.DeltaDistance, 0);
        var xb = endIndex - (int)Dim(endIndex, x2 / elevations.DeltaDistance);
        if (xb <= xa)
        {
            xa = Dim(xa, 1);
            xb = endIndex - Dim(endIndex, xb + 1);
        }
        var ja = (int)xa;
        var jb = (int)xb;
        var n = jb - ja;
        xa = xb - xa;
        var x = -0.5 * xa;
        xb += x;
        var a = 0.5 * (elevations.Points[ja] + elevations.Points[jb]);
        var b = 0.5 * (elevations.Points[ja] - elevations.Points[jb]) * x;
        for (var i = 2; i <= n; i++)
        {
            ja++;
            x += 1;
            a += elevations.Points[ja];
            b += elevations.Points[ja] * x;
        }
        a /= xa;
        b = b * 12 / ((xa * xa + 2) * xa);
        z0 = a - b * xb;
        zn = a + b * (endIndex - xb);
    }

    private static double qtile(int n, double[] a, int ir)
    {
        double q = 0;
        int j1 = 0, i0 = 0;
        var done = false;
        var goto10 = true;

        var m = 0;
        var k = Min(Max(0, ir), n);
        while (!done)
        {
            if (goto10)
            {
                q = a[k];
                i0 = m;
                j1 = n;
            }
            var i = i0;
            while (i <= n && a[i] >= q)
                i++;
            if (i > n)
                i = n;
            var j = j1;
            while (j >= m && a[j] <= q)
                j--;
            if (j < m)
                j = m;
            if (i < j)
            {
                (a[i], a[j]) = (a[j], a[i]);
                i0 = i + 1;
                j1 = j - 1;
                goto10 = false;
            }
            else if (i < k)
            {
                a[k] = a[i];
                a[i] = q;
                m = i + 1;
                goto10 = true;
            }
            else if (j > k)
            {
                a[k] = a[j];
                a[j] = q;
                n = j - 1;
                goto10 = true;
            }
            else
            {
                done = true;
            }
        }
        return q;
    }

    private static double d1thx(Elevations elevations, double x1, double x2)
    {
        var endIndex = elevations.EndIndex;
        var xa = x1 / elevations.DeltaDistance;
        var xb = x2 / elevations.DeltaDistance;
        if (xb - xa < 2)  // exit out
            return 0;
        var ka = (int)(0.1 * (xb - xa + 8));
        ka = Min(Max(4, ka), 25);
        var n = 10 * ka - 5;
        var kb = n - ka + 1;
        double sn = n - 1;
        xb = (xb - xa) / sn;
        var k = (int)(xa + 1);
        xa -= k;
        var s = new Elevations(n, 1);
        for (var j = 0; j < n; j++)
        {
            while (xa > 0 && k < endIndex)
            {
                xa -= 1;
                k++;
            }
            s.Points[j] = elevations.Points[k] + (elevations.Points[k] - elevations.Points[k - 1]) * xa;
            xa += xb;
        }
        z1sq1(s, 0, sn, out xa, out xb);
        xb = (xb - xa) / sn;
        for (var j = 0; j < n; j++)
        {
            s.Points[j] -= xa;
            xa += xb;
        }
        var d1thxv = qtile(n - 1, s.Points, ka - 1) - qtile(n - 1, s.Points, kb - 1);
        d1thxv /= 1 - 0.8 * Exp(-(x2 - x1) / 50000);
        return d1thxv;
    }

    private void qlrpfl(Elevations elevations, RadioClimate radioClimate, VariabilityMode variabilityMode)
    {
        _distance = elevations.EndIndex * elevations.DeltaDistance;
        hzns(elevations);
        foreach (var antenna in Antennae)
        {
            antenna.xl = Min(15 * antenna.StructuralHeight, 0.1 * antenna.HorizonDistance);
        }
        Receiver.xl = _distance - Receiver.xl;
        _terrainIrregularity = d1thx(elevations, Transmitter.xl, Receiver.xl);
        if (Transmitter.HorizonDistance + Receiver.HorizonDistance > 1.5 * _distance)
        {
            z1sq1(elevations, Transmitter.xl, Receiver.xl, out var za, out var zb);
            Transmitter.EffectiveHeight = Transmitter.StructuralHeight + Dim(elevations.FirstPoint, za);
            Receiver.EffectiveHeight = Receiver.StructuralHeight + Dim(elevations.LastPoint, zb);
            foreach (var antenna in Antennae)
            {
                antenna.HorizonDistance = Sqrt(2 * antenna.EffectiveHeight / _earthsEffectiveCurvature) * Exp(-0.07 * Sqrt(_terrainIrregularity / Max(antenna.EffectiveHeight, 5)));
            }
            var distance = Transmitter.HorizonDistance + Receiver.HorizonDistance;
            if (distance <= _distance)
            {
                distance = Pow(_distance / distance, 2);
                foreach (var antenna in Antennae)
                {
                    antenna.EffectiveHeight *= distance;
                    antenna.HorizonDistance = Sqrt(2 * antenna.EffectiveHeight / _earthsEffectiveCurvature) * Exp(-0.07 * Sqrt(_terrainIrregularity / Max(antenna.EffectiveHeight, 5)));
                }
            }
            foreach (var antenna in Antennae)
            {
                distance = Sqrt(2 * antenna.EffectiveHeight / _earthsEffectiveCurvature);
                antenna.HorizonElevationAngle = (0.65 * _terrainIrregularity * (distance / antenna.HorizonDistance - 1) - 2 * antenna.EffectiveHeight) / distance;
            }
        }
        else
        {
            z1sq1(elevations, Transmitter.xl, 0.9 * Transmitter.HorizonDistance, out var za, out _);
            z1sq1(elevations, _distance - 0.9 * Receiver.HorizonDistance, Receiver.xl, out _, out var zb);
            Transmitter.EffectiveHeight = Transmitter.StructuralHeight + Dim(elevations.FirstPoint, za);
            Receiver.EffectiveHeight = Receiver.StructuralHeight + Dim(elevations.LastPoint, zb);
        }
        _controlFlow = ControlFlow.PointToPoint;
        _variabilityMode = variabilityMode;
        _radioClimate = radioClimate;
        _changes = Changes.All;
        ReferenceAttenuation(0);
    }

    /// <summary>
    /// Point-To-Point Mode Calculations
    /// </summary>
    /// <param name="elevations"></param>
    /// <param name="transmitterHeight">Transmitter height in meters</param>
    /// <param name="receiverHeight">Receiver height in meters</param>
    /// <param name="eps_dielect">Dielectric constant of ground</param>
    /// <param name="sgm_conductivity">Conductivity of ground in S/m</param>
    /// <param name="eno_ns_surfref">Surface refractivity in N-units</param>
    /// <param name="frq_mhz">Frequency in MHz</param>
    /// <param name="radioClimate"></param>
    /// <param name="polarization">Transmitting polarization</param>
    /// <param name="variabilityMode"></param>
    /// <param name="timepct">0.01 to 0.99</param>
    /// <param name="locpct">0.01 to 0.99</param>
    /// <param name="confpct">0.01 to 0.99</param>
    /// <param name="dbLoss"></param>
    /// <param name="propmode"></param>
    /// <param name="deltaH">Terrain roughness in meters</param>
    /// <param name="errorCode"></param>
    public void point_to_pointMDH(double[] elevations, double transmitterHeight, double receiverHeight,
            double eps_dielect, double sgm_conductivity, double eno_ns_surfref,
            double frq_mhz, RadioClimate radioClimate, Polarization polarization, VariabilityMode variabilityMode, double timepct, double locpct, double confpct,
            out double dbLoss, out PropMode propmode, out double deltaH, out ErrorCode errorCode)
    // elevations[]: [num points - 1], [delta dist(meters)], [height(meters) point 1], ..., [height(meters) point n]
    {
        _radioClimate = radioClimate;
        _changes = Changes.All;
        _controlFlow = ControlFlow.PointToPoint;
        _w1 = true; // possible bugfix: original embedded this value in variabilityMode
        _variabilityMode = variabilityMode; // bugfix: original used 'mobile'

        Transmitter.StructuralHeight = transmitterHeight;
        Receiver.StructuralHeight = receiverHeight;

        var e = new Elevations(elevations);
        qlrps(frq_mhz, GetAverage(elevations), eno_ns_surfref, polarization, eps_dielect, sgm_conductivity);
        qlrpfl(e, _radioClimate, _variabilityMode);
        var fs = 32.45 + 20 * Log10(frq_mhz) + 20 * Log10(_distance / 1000);
        deltaH = _terrainIrregularity;
        propmode = GetPropMode();
        dbLoss = avar(qerfi(timepct), qerfi(locpct), qerfi(confpct)) + fs;
        errorCode = _errorCode;
    }

    private void SetErrorCode(ErrorCode value) => _errorCode = (ErrorCode)Max((int)_errorCode, (int)value);

    private double GetAverage(double[] elevations)
    {
        var points = (int)elevations[0];
        var start = 3 + (int)(0.1 * points);
        var end = points - start + 6;
        double average = 0;
        for (var i = start - 1; i < end; i++)
        {
            average += elevations[i];
        }
        average /= (end - start + 1);
        return average;
    }

    private PropMode GetPropMode()
    {
        PropMode propmode;
        var distance = (int)(_distance - _totalHorizonDistance);

        if (distance < 0)
            propmode = PropMode.LineOfSight;
        else if (distance == 0)
            propmode = PropMode.SingleHorizon;
        else // q > 0
            propmode = PropMode.DoubleHorizon;

        if (propmode != PropMode.LineOfSight)
        {
            if (_distance <= _lineOfSightDistance || _distance <= _scatterDistance)
                propmode |= PropMode.DiffractionDominant;
            else if (_distance > _scatterDistance)
                propmode |= PropMode.TroposcatterDominant;
        }

        return propmode;
    }

    /// <summary>
    /// Area Mode Calculations
    /// </summary>
    /// <param name="variabilityMode"></param>
    /// <param name="deltaH"></param>
    /// <param name="transmitterHeight"></param>
    /// <param name="receiverHeight"></param>
    /// <param name="distance"></param>
    /// <param name="transmitterSiteCriteria"></param>
    /// <param name="receiverSiteCriteria"></param>
    /// <param name="eps_dielect"></param>
    /// <param name="sgm_conductivity"></param>
    /// <param name="eno_ns_surfref"></param>
    /// <param name="frq_mhz"></param>
    /// <param name="radioClimate"></param>
    /// <param name="polarization"></param>
    /// <param name="pctTime">0.01 to 0.99</param>
    /// <param name="pctLoc">0.01 to 0.99</param>
    /// <param name="pctConf">0.01 to 0.99</param>
    /// <param name="dbLoss"></param>
    /// <param name="propmode"></param>
    /// <param name="errorCode"></param>
    public void area(VariabilityMode variabilityMode, double deltaH, double transmitterHeight, double receiverHeight,
        double distance, SiteCriteria transmitterSiteCriteria, SiteCriteria receiverSiteCriteria,
        double eps_dielect, double sgm_conductivity, double eno_ns_surfref,
        double frq_mhz, RadioClimate radioClimate, Polarization polarization, double pctTime, double pctLoc,
        double pctConf, out double dbLoss, out PropMode propmode, out ErrorCode errorCode)
    {
        _terrainIrregularity = deltaH;
        _radioClimate = radioClimate;
        _surfaceRefractivity = eno_ns_surfref;
        Transmitter.StructuralHeight = transmitterHeight;
        Transmitter.SiteCriteria = transmitterSiteCriteria;
        Receiver.StructuralHeight = receiverHeight;
        Receiver.SiteCriteria = receiverSiteCriteria;
        qlrps(frq_mhz, 0, eno_ns_surfref, polarization, eps_dielect, sgm_conductivity);
        qlra(_radioClimate, variabilityMode);
        if (_changes < Changes.Distance)
            _changes = Changes.Distance;
        ReferenceAttenuation(distance * 1000);
        var fs = 32.45 + 20 * Log10(frq_mhz) + 20 * Log10(_distance / 1000);
        dbLoss = fs + avar(qerfi(pctTime), qerfi(pctLoc), qerfi(pctConf)); ;
        errorCode = _errorCode;
        propmode = GetPropMode();
    }

    private enum ControlFlow
    {
        PointToPoint = -1,
        AreaContinue = 0,
        AreaBegin = 1,
    }

    private enum Changes
    {
        None = 0,
        Distance = 1,
        AntennaHeight = 2,
        Frequency = 3,
        VariabilityMode = 4,
        All = 5,
    }
}