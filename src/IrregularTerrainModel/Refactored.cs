using System;
using System.Diagnostics.CodeAnalysis;
using System.Linq;
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
        public double hg;
        /// <summary>
        /// Effective height
        /// </summary>
        public double he;
        /// <summary>
        /// Horizon distance
        /// </summary>
        public double dl;
        /// <summary>
        /// Horizon elevation angle
        /// </summary>
        public double the;
        /// <summary>
        /// Smooth earth horizon distance
        /// </summary>
        public double dls;
        public SiteCriteria kst;
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


    private const double THIRD = 1.0 / 3.0;

    private readonly Antenna Transmitter = new Antenna();
    private readonly Antenna Receiver = new Antenna();
    private readonly Antenna[] Antennae;

    public Refactored()
    {
        Antennae = new[] { Transmitter, Receiver };
    }

    /// <summary>
    /// Reference attenuation
    /// </summary>
    private double _aref;
    /// <summary>
    /// Distance
    /// </summary>
    private double _dist;
    /// <summary>
    /// Wave number (radio frequency)
    /// </summary>
    private double _wn;
    /// <summary>
    /// Terrain irregularity parameter
    /// </summary>
    private double _dh;
    /// <summary>
    /// Surface refractivity
    /// </summary>
    private double _ens;
    /// <summary>
    /// Earth's effective curvature
    /// </summary>
    private double _gme;
    /// <summary>
    /// Surface transfer impedance of the ground
    /// </summary>
    private Complex _zgnd;
    /// <summary>
    /// Error indicator
    /// </summary>
    private int _kwx;
    /// <summary>
    /// Controlling mode
    /// </summary>
    private ControlFlow _mdp;
    /// <summary>
    /// Standard deviation of situation variability (with what confidence will a threshold signal level be exceeded)
    /// </summary>
    private double _sgc;
    /// <summary>
    /// A control switch
    /// </summary>
    private Changes _lvar;
    /// <summary>
    /// Desired mode of variability
    /// </summary>
    private VariabilityMode _mdvar;
    /// <summary>
    /// Climate indicator
    /// </summary>
    private RadioClimate _klim;
    /// <summary>
    /// Line-of-sight distance
    /// </summary>
    private double _dlsa;
    /// <summary>
    /// Scatter distance
    /// </summary>
    private double _dx;
    /// <summary>
    /// Line-of-sight coefficient
    /// </summary>
    private double _ael;
    /// <summary>
    /// Line-of-sight coefficient
    /// </summary>
    private double _ak1;
    /// <summary>
    /// Line-of-sight coefficient
    /// </summary>
    private double _ak2;
    /// <summary>
    /// Diffraction coefficient
    /// </summary>
    private double _aed;
    /// <summary>
    /// Diffraction coefficient
    /// </summary>
    private double _emd;
    /// <summary>
    /// Scatter coefficient
    /// </summary>
    private double _aes;
    /// <summary>
    /// Scatter coefficient
    /// </summary>
    private double _ems;
    /// <summary>
    /// Total horizon distance
    /// </summary>
    private double _dla;
    /// <summary>
    /// Total bending angle
    /// </summary>
    private double _tha;

    private double _ad, _rr, _etq, _h0s;

    private double _dexa, _de, _vmd, _vs0, _sgl, _sgtm, _sgtp, _sgtd, _tgtd, _gm, _gp;

    private double _dmin, _xae;

    private double _wls;

    private ClimateSettings _cs;

    private bool _w1;

    private double _wd1, _xd1, _afo, _qk, _aht, _xht;

    private bool _wlos, _wscat;

    /// <summary>
    /// This performs the FORTRAN DIM function.
    /// </summary>
    /// <param name="x"></param>
    /// <param name="y"></param>
    /// <returns>x-y if x is greater than y, otherwise 0</returns>
    private static double Dim(double x, double y)
    {
        if (x > y)
            return x - y;
        else
            return 0;
    }

    /// <summary>
    /// The attenuation due to a single knife edge - the Fresnel integral (in decibels) as a function of <paramref name="v2"/>
    /// </summary>
    /// <param name="v2"></param>
    /// <returns></returns>
    private static double aknfe(double v2)
    {
        if (v2 < 5.76)
            return 6.02 + 9.11 * Math.Sqrt(v2) - 1.27 * v2;
        else
            return 12.953 + 4.343 * Math.Log(v2);
    }

    /// <summary>
    /// The height-gain over a smooth spherical earth - to be used in the "three radii" method.
    /// </summary>
    /// <param name="x"></param>
    /// <param name="pk"></param>
    /// <returns></returns>
    private static double fht(double x, double pk)
    {
        double fhtv;
        if (x < 200)
        {
            var w = -Math.Log(pk);
            if (pk < 1e-5 || x * Math.Pow(w, 3) > 5495)
            {
                fhtv = -117;
                if (x > 1)
                    fhtv = 17.372 * Math.Log(x) + fhtv;
            }
            else
            {
                fhtv = 2.5e-5 * x * x / pk - 8.686 * w - 15;
            }
        }
        else
        {
            fhtv = 0.05751 * x - 4.343 * Math.Log(x);
            if (x < 2000)
            {
                var w = 0.0134 * x * Math.Exp(-0.005 * x);
                fhtv = (1 - w) * fhtv + w * (17.372 * Math.Log(x) - 117);
            }
        }
        return fhtv;
    }

    private static readonly int[] _h0f_a = { 25, 80, 177, 395, 705 };
    private static readonly int[] _h0f_b = { 24, 45, 68, 80, 105 };

    /// <summary>
    /// H01 function for scatter fields
    /// </summary>
    /// <param name="r"></param>
    /// <param name="et"></param>
    /// <returns></returns>
    private static double h0f(double r, double et)
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
        var x = Math.Pow(1 / r, 2);
        var h0fv = 4.343 * Math.Log((_h0f_a[it - 1] * x + _h0f_b[it - 1]) * x + 1);
        if (q != 0)
            h0fv = (1 - q) * h0fv + q * 4.343 * Math.Log((_h0f_a[it] * x + _h0f_b[it]) * x + 1);
        return h0fv;
    }

    private static readonly double[] _ahd_a = { 133.4, 104.6, 71.8 };
    private static readonly double[] _ahd_b = { 0.332e-3, 0.212e-3, 0.157e-3 };
    private static readonly double[] _ahd_c = { -4.343, -1.086, 2.171 };

    /// <summary>
    /// The F(0d) function for scatter fields
    /// </summary>
    /// <param name="td"></param>
    /// <returns></returns>
    private static double ahd(double td)
    {
        int i;
        if (td <= 10000)
            i = 0;
        else if (td <= 70000)
            i = 1;
        else
            i = 2;
        return _ahd_a[i] + _ahd_b[i] * td + _ahd_c[i] * Math.Log(td);
    }

    private void init_adiff()
    {
        var q = Transmitter.hg * Receiver.hg;
        _qk = Transmitter.he * Receiver.he - q;
        if (_mdp == ControlFlow.PointToPoint)
            q += 10;
        _wd1 = Math.Sqrt(1 + _qk / q);
        _xd1 = _dla + _tha / _gme;
        q = (1 - 0.8 * Math.Exp(-_dlsa / 50000.0)) * _dh;
        q *= 0.78 * Math.Exp(-Math.Pow(q / 16.0, 0.25));
        _afo = Math.Min(15, 2.171 * Math.Log(1 + 4.77e-4 * Transmitter.hg * Receiver.hg * _wn * q));
        _qk = 1 / Complex.Abs(_zgnd);
        _aht = 20;
        _xht = 0;
        foreach (var antenna in Antennae)
        {
            var a = 0.5 * Math.Pow(antenna.dl, 2) / antenna.he;
            var wa = Math.Pow(a * _wn, THIRD);
            var pk = _qk / wa;
            q = (1.607 - pk) * 151 * wa * antenna.dl / a;
            _xht += q;
            _aht += fht(q, pk);
        }
    }

    /// <summary>
    /// Finds the "diffraction attenuation" at the distance <paramref name="d"/>
    /// </summary>
    /// <remarks>
    /// It uses a convex combination of smooth earth diffraction and double knife-edge diffraction
    /// </remarks>
    /// <param name="d"></param>
    /// <returns></returns>
    private double adiff(double d)
    {
        var th = _tha + d * _gme;
        var ds = d - _dla;
        var q = 0.0795775 * _wn * ds * Math.Pow(th, 2);
        var adiffv = aknfe(q * Transmitter.dl / (ds + Transmitter.dl)) + aknfe(q * Receiver.dl / (ds + Receiver.dl));
        var a = ds / th;
        var wa = Math.Pow(a * _wn, THIRD);
        var pk = _qk / wa;
        q = (1.607 - pk) * 151 * wa * th + _xht;
        var ar = 0.05751 * q - 4.343 * Math.Log(q) - _aht;
        q = (_wd1 + _xd1 / d) * Math.Min((1 - 0.8 * Math.Exp(-d / 50000.0)) * _dh * _wn, 6283.2);
        var wd = 25.1 / (25.1 + Math.Sqrt(q));
        return ar * wd + (1 - wd) * adiffv + _afo;
    }

    private void init_ascat()
    {
        _ad = Transmitter.dl - Receiver.dl;
        _rr = Receiver.he / Transmitter.he;
        if (_ad < 0)
        {
            _ad = -_ad;
            _rr = 1 / _rr;
        }
        _etq = (5.67e-6 * _ens - 2.32e-3) * _ens + 0.031;
        _h0s = -15;
    }

    /// <summary>
    /// Finds the "scatter attenuation" at the distance <paramref name="d"/>. It uses an approximation to the methods of NBS TN101 with checks for inadmissible situations
    /// </summary>
    /// <param name="d"></param>
    /// <returns></returns>
    private double ascat(double d)
    {
        double th;
        double h0;
        if (_h0s > 15)
            h0 = _h0s;
        else
        {
            th = Transmitter.the + Receiver.the + d * _gme;
            var r2 = 2 * _wn * th;
            var r1 = r2 * Transmitter.he;
            r2 *= Receiver.he;
            if (r1 < 0.2 && r2 < 0.2)
                return 1001;  // <==== early return
            var ss = (d - _ad) / (d + _ad);
            var q = _rr / ss;
            ss = Math.Max(0.1, ss);
            q = Math.Min(Math.Max(0.1, q), 10);
            var z0 = (d - _ad) * (d + _ad) * th * 0.25 / d;
            var et = (_etq * Math.Exp(-Math.Pow(Math.Min(1.7, z0 / 8000.0), 6)) + 1) * z0 / 1.7556e3;
            var ett = Math.Max(et, 1);
            h0 = (h0f(r1, ett) + h0f(r2, ett)) * 0.5;
            h0 += Math.Min(h0, (1.38 - Math.Log(ett)) * Math.Log(ss) * Math.Log(q) * 0.49);
            h0 = Dim(h0, 0);
            if (et < 1)
                h0 = et * h0 + (1 - et) * 4.343 * Math.Log(Math.Pow((1 + 1.4142 / r1) * (1 + 1.4142 / r2), 2) * (r1 + r2) / (r1 + r2 + 2.8284));
            if (h0 > 15 && _h0s >= 0)
                h0 = _h0s;
        }
        _h0s = h0;
        th = _tha + d * _gme;
        return ahd(th * d) + 4.343 * Math.Log(47.7 * _wn * Math.Pow(th, 4)) - 0.1 * (_ens - 301) * Math.Exp(-th * d / 40000.0) + h0;
    }

    private static double qerfi(double q)
    {
        const double c0 = 2.515516698;
        const double c1 = 0.802853;
        const double c2 = 0.010328;
        const double d1 = 1.432788;
        const double d2 = 0.189269;
        const double d3 = 0.001308;

        var x = 0.5 - q;
        var t = Math.Max(0.5 - Math.Abs(x), 0.000001);
        t = Math.Sqrt(-2 * Math.Log(t));
        var v = t - ((c2 * t + c1) * t + c0) / (((d3 * t + d2) * t + d1) * t + 1);
        if (x < 0)
            v = -v;
        return v;
    }

    private void qlrps(double fmhz, double zsys, double en0, Polarization ipol, double eps, double sgm)
    {
        const double gma = 157e-9;
        _wn = fmhz / 47.7;
        _ens = en0;
        if (zsys != 0)
            _ens *= Math.Exp(-zsys / 9460.0);
        _gme = gma * (1 - 0.04665 * Math.Exp(_ens / 179.3));
        var zq = new Complex(eps, 376.62 * sgm / _wn);
        var prop_zgnd = Complex.Sqrt(zq - 1);
        if (ipol != Polarization.Horizontal)
            prop_zgnd = prop_zgnd / zq;

        _zgnd = prop_zgnd;
    }

    private static double abq_alos(Complex r)
    {
        return r.Real * r.Real + r.Imaginary * r.Imaginary;
    }

    private void init_alos()
    {
        _wls = 0.021 / (0.021 + _wn * _dh / Math.Max(10000, _dlsa));
    }

    /// <summary>
    /// Finds the "line-of-sight" attenuation at the distance <paramref name="d"/> It uses a convex combination of plane earth fields and diffracted fields
    /// </summary>
    /// <param name="d"></param>
    private double alos(double d)
    {
        var q = (1 - 0.8 * Math.Exp(-d / 50000.0)) * _dh;
        var s = 0.78 * q * Math.Exp(-Math.Pow(q / 16.0, 0.25));
        q = Transmitter.he + Receiver.he;
        var sps = q / Math.Sqrt(d * d + q * q);
        var r = (sps - _zgnd) / (sps + _zgnd) * Math.Exp(-Math.Min(10, _wn * s * sps));
        q = abq_alos(r);
        if (q < 0.25 || q < sps)
            r = r * Math.Sqrt(sps / q);
        var alosv = _emd * d + _aed;
        q = _wn * Transmitter.he * Receiver.he * 2 / d;
        if (q > 1.57)
            q = 3.14 - 2.4649 / q;
        return (-4.343 * Math.Log(abq_alos(new Complex(Math.Cos(q), -Math.Sin(q)) + r)) - alosv) * _wls + alosv;
    }

    private void qlra(RadioClimate klimx, VariabilityMode mdvarx)
    {
        foreach (var antenna in Antennae)
        {
            double q;
            if (antenna.kst == SiteCriteria.Random)
                antenna.he = antenna.hg;
            else
            {
                if (antenna.kst != SiteCriteria.Careful)
                    q = 9;
                else
                    q = 4;
                if (antenna.hg < 5)
                    q *= Math.Sin(0.3141593 * antenna.hg);
                antenna.he = antenna.hg + (1 + q) * Math.Exp(-Math.Min(20, 2 * antenna.hg / Math.Max(1e-3, _dh)));
            }
            q = Math.Sqrt(2 * antenna.he / _gme);
            antenna.dl = q * Math.Exp(-0.07 * Math.Sqrt(_dh / Math.Max(antenna.he, 5)));
            antenna.the = (0.65 * _dh * (q / antenna.dl - 1) - 2 * antenna.he) / q;
        }
        _mdp = ControlFlow.AreaBegin;
        _mdvar = mdvarx;
        _klim = klimx;
        _lvar = Changes.All;
    }

    /// <summary>
    /// The Longley-Rice propagation program. This is the basic program; it returns the reference attenuation
    /// </summary>
    /// <param name="d"></param>
    private void lrprop(double d)  // PaulM_lrprop
    {
        if (_mdp != ControlFlow.AreaContinue)
        {
            foreach (var antenna in Antennae)
                antenna.dls = Math.Sqrt(2 * antenna.he / _gme);
            _dlsa = Transmitter.dls + Receiver.dls;
            _dla = Transmitter.dl + Receiver.dl;
            _tha = Math.Max(Transmitter.the + Receiver.the, -_dla * _gme);
            _wlos = false;
            _wscat = false;
            if (_wn < 0.838 || _wn > 210)
            {
                _kwx = Math.Max(_kwx, 1);
            }
            foreach (var antenna in Antennae)
                if (antenna.hg < 1 || antenna.hg > 1000)
                {
                    _kwx = Math.Max(_kwx, 1);
                }
            foreach (var antenna in Antennae)
                if (Math.Abs(antenna.the) > 200e-3 || antenna.dl < 0.1 * antenna.dls ||
                    antenna.dl > 3 * antenna.dls)
                {
                    _kwx = Math.Max(_kwx, 3);
                }
            if (_ens < 250 || _ens > 400 ||
                _gme < 75e-9 || _gme > 250e-9 ||
                _zgnd.Real <= Math.Abs(_zgnd.Imaginary) ||
                _wn < 0.419 || _wn > 420)
            {
                _kwx = 4;
            }
            foreach (var antenna in Antennae)
                if (antenna.hg < 0.5 || antenna.hg > 3000)
                {
                    _kwx = 4;
                }
            _dmin = Math.Abs(Transmitter.he - Receiver.he) / 200e-3;
            init_adiff();
            _xae = Math.Pow(_wn * Math.Pow(_gme, 2), -THIRD);
            var d3 = Math.Max(_dlsa, 1.3787 * _xae + _dla);
            var d4 = d3 + 2.7574 * _xae;
            var a3 = adiff(d3);
            var a4 = adiff(d4);
            _emd = (a4 - a3) / (d4 - d3);
            _aed = a3 - _emd * d3;
        }
        if (_mdp != ControlFlow.PointToPoint)
        {
            _mdp = ControlFlow.PointToPoint;
            _dist = d;
        }
        if (_dist > 0)
        {
            if (_dist > 1000000)
            {
                _kwx = Math.Max(_kwx, 1);
            }
            if (_dist < _dmin)
            {
                _kwx = Math.Max(_kwx, 3);
            }
            if (_dist < 1000 || _dist > 2000000)
            {
                _kwx = 4;
            }
        }
        if (_dist < _dlsa)
        {
            if (!_wlos)
            {
                init_alos();
                var d2 = _dlsa;
                var a2 = _aed + d2 * _emd;
                var d0 = 1.908 * _wn * Transmitter.he * Receiver.he;
                double d1;
                if (_aed >= 0)
                {
                    d0 = Math.Min(d0, 0.5 * _dla);
                    d1 = d0 + 0.25 * (_dla - d0);
                }
                else
                    d1 = Math.Max(-_aed / _emd, 0.25 * _dla);
                var a1 = alos(d1);
                var wq = false;
                if (d0 < d1)
                {
                    var a0 = alos(d0);
                    var q = Math.Log(d2 / d0);
                    _ak2 = Math.Max(0, ((d2 - d0) * (a1 - a0) - (d1 - d0) * (a2 - a0)) / ((d2 - d0) * Math.Log(d1 / d0) - (d1 - d0) * q));
                    wq = _aed >= 0 || _ak2 > 0;
                    if (wq)
                    {
                        _ak1 = (a2 - a0 - _ak2 * q) / (d2 - d0);
                        if (_ak1 < 0)
                        {
                            _ak1 = 0;
                            _ak2 = Dim(a2, a0) / q;
                            if (_ak2 == 0)
                                _ak1 = _emd;
                        }
                    }
                }
                if (!wq)
                {
                    _ak1 = Dim(a2, a1) / (d2 - d1);
                    _ak2 = 0;
                    if (_ak1 == 0)
                        _ak1 = _emd;
                }
                _ael = a2 - _ak1 * d2 - _ak2 * Math.Log(d2);
                _wlos = true;
            }
            if (_dist > 0)
                _aref = _ael + _ak1 * _dist + _ak2 * Math.Log(_dist);
        }
        if (_dist <= 0 || _dist >= _dlsa)
        {
            if (!_wscat)
            {
                init_ascat();
                var d5 = _dla + 200000;
                var d6 = d5 + 200000;
                var a6 = ascat(d6);
                var a5 = ascat(d5);
                if (a5 < 1000)
                {
                    _ems = (a6 - a5) / 200000.0;
                    _dx = Math.Max(_dlsa, Math.Max(_dla + 0.3 * _xae * Math.Log(47.7 * _wn), (a5 - _aed - _ems * d5) / (_emd - _ems)));
                    _aes = (_emd - _ems) * _dx + _aed;
                }
                else
                {
                    _ems = _emd;
                    _aes = _aed;
                    _dx = 10000000;
                }
                _wscat = true;
            }
            if (_dist > _dx)
                _aref = _aes + _ems * _dist;
            else
                _aref = _aed + _emd * _dist;
        }
        _aref = Math.Max(_aref, 0);
    }

    private static double curve(double c1, double c2, double x1, double x2, double x3, double de)
    {
        return (c1 + c2 / (1 + Math.Pow((de - x2) / x3, 2))) * Math.Pow(de / x1, 2) / (1 + Math.Pow(de / x1, 2));
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

    private static readonly ClimateSettings[] _climateSettings = new[]{
        new ClimateSettings (-9.67, 12.7, 144.9e3, 190.3e3, 133.8e3, 2.13, 159.5, 762.2e3, 123.6e3, 94.5e3, 2.11, 102.3, 636.9e3, 134.8e3, 95.6e3, 1.224, 1.282, 1.0, 0.0, 0.0, 1.0, 0.0, 0.0 ),
        new ClimateSettings (-0.62, 9.19, 228.9e3, 205.2e3, 143.6e3, 2.66, 7.67, 100.4e3, 172.5e3, 136.4e3, 6.87, 15.53, 138.7e3, 143.7e3, 98.6e3, 0.801, 2.161, 1.0, 0.0, 0.0, 0.93, 0.31, 2.00 ),
        new ClimateSettings (1.26, 15.5, 262.6e3, 185.2e3, 99.8e3, 6.11, 6.65, 138.2e3, 242.2e3, 178.6e3, 10.08, 9.60, 165.3e3, 225.7e3, 129.7e3, 1.380, 1.282, 1.0, 0.0, 0.0, 1.0, 0.0, 0.0 ),
        new ClimateSettings (-9.21, 9.05, 84.1e3, 101.1e3, 98.6e3, 1.98, 13.11, 139.1e3, 132.7e3, 193.5e3, 3.68, 159.3, 464.4e3, 93.1e3, 94.2e3, 1.000, 20.0, 1.0, 0.0, 0.0, 0.93, 0.19, 1.79 ),
        new ClimateSettings (-0.62, 9.19, 228.9e3, 205.2e3, 143.6e3, 2.68, 7.16, 93.7e3, 186.8e3, 133.5e3, 4.75, 8.12, 93.2e3, 135.9e3, 113.4e3, 1.224, 1.282, 0.92, 0.25, 1.77, 0.93, 0.31, 2.00 ),
        new ClimateSettings (-0.39, 2.86, 141.7e3, 315.9e3, 167.4e3, 6.86, 10.38, 187.8e3, 169.6e3, 108.9e3, 8.58, 13.97, 216.0e3, 152.0e3, 122.7e3, 1.518, 1.282, 1.0, 0.0, 0.0, 1.0, 0.0, 0.0 ),
        new ClimateSettings (3.15, 857.9, 2222.0e3, 164.8e3, 116.3e3, 8.51, 169.8, 609.8e3, 119.9e3, 106.6e3, 8.43, 8.19, 136.2e3, 188.5e3, 122.9e3, 1.518, 1.282, 1.0, 0.0, 0.0, 1.0, 0.0, 0.0 )};

    private double avar(double zzt, double zzl, double zzc)
    {
        const double rt = 7.8, rl = 24.0;
        double sgt, yr;

        if (_lvar > Changes.None)
        {
            double q;
            switch (_lvar)
            {
                case Changes.All:
                    _cs = _climateSettings[(int)_klim - 1];
                    goto case Changes.VariabilityMode;
                case Changes.VariabilityMode:
                    goto case Changes.Frequency;
                case Changes.Frequency:
                    q = Math.Log(0.133 * _wn);
                    _gm = _cs.cfm1 + _cs.cfm2 / (Math.Pow(_cs.cfm3 * q, 2) + 1);
                    _gp = _cs.cfp1 + _cs.cfp2 / (Math.Pow(_cs.cfp3 * q, 2) + 1);
                    goto case Changes.AntennaHeight;
                case Changes.AntennaHeight:
                    _dexa = Math.Sqrt(18000000 * Transmitter.he) + Math.Sqrt(18000000 * Receiver.he) + Math.Pow(575.7e12 / _wn, THIRD);
                    goto case Changes.Distance;
                case Changes.Distance:
                    if (_dist < _dexa)
                        _de = 130000 * _dist / _dexa;
                    else
                        _de = 130000 + _dist - _dexa;
                    break;
            }
            _vmd = curve(_cs.cv1, _cs.cv2, _cs.yv1, _cs.yv2, _cs.yv3, _de);
            _sgtm = curve(_cs.csm1, _cs.csm2, _cs.ysm1, _cs.ysm2, _cs.ysm3, _de) * _gm;
            _sgtp = curve(_cs.csp1, _cs.csp2, _cs.ysp1, _cs.ysp2, _cs.ysp3, _de) * _gp;
            _sgtd = _sgtp * _cs.csd1;
            _tgtd = (_sgtp - _sgtd) * _cs.zd;
            if (_w1)
                _sgl = 0;
            else
            {
                q = (1 - 0.8 * Math.Exp(-_dist / 50000.0)) * _dh * _wn;
                _sgl = 10 * q / (q + 13);
            }
            _vs0 = Math.Pow(5 + 3 * Math.Exp(-_de / 100000.0), 2);
            _lvar = Changes.None;
        }
        var zt = zzt;
        var zl = zzl;
        var zc = zzc;
        switch (_mdvar)
        {
            case VariabilityMode.Single:
                zt = zc;
                zl = zc;
                break;
            case VariabilityMode.Individual:
                zl = zc;
                break;
            case VariabilityMode.Mobile:
                zl = zt;
                break;
        }
        if (Math.Abs(zt) > 3.1 || Math.Abs(zl) > 3.1 || Math.Abs(zc) > 3.1)
        {
            _kwx = Math.Max(_kwx, 1);
        }
        if (zt < 0)
            sgt = _sgtm;
        else if (zt <= _cs.zd)
            sgt = _sgtp;
        else
            sgt = _sgtd + _tgtd / zt;
        var vs = _vs0 + Math.Pow(sgt * zt, 2) / (rt + zc * zc) + Math.Pow(_sgl * zl, 2) / (rl + zc * zc);
        switch (_mdvar)
        {
            case VariabilityMode.Single:
                yr = 0;
                _sgc = Math.Sqrt(sgt * sgt + _sgl * _sgl + vs);
                break;
            case VariabilityMode.Individual:
                yr = sgt * zt;
                _sgc = Math.Sqrt(_sgl * _sgl + vs);
                break;
            case VariabilityMode.Mobile:
                yr = Math.Sqrt(sgt * sgt + _sgl * _sgl) * zt;
                _sgc = Math.Sqrt(vs);
                break;
            default:
                yr = sgt * zt + _sgl * zl;
                _sgc = Math.Sqrt(vs);
                break;
        }
        var avarv = _aref - _vmd - yr - _sgc * zc;
        if (avarv < 0)
            avarv = avarv * (29 - avarv) / (29 - 10 * avarv);
        return avarv;

    }

    private void hzns(Elevations pfl)
    {
        var np = pfl.EndIndex;
        var xi = pfl.DeltaDistance;
        var za = pfl.FirstPoint + Transmitter.hg;
        var zb = pfl.LastPoint + Receiver.hg;
        var qc = 0.5 * _gme;
        var q = qc * _dist;
        Receiver.the = (zb - za) / _dist;
        Transmitter.the = Receiver.the - q;
        Receiver.the = -Receiver.the - q;
        Transmitter.dl = _dist;
        Receiver.dl = _dist;
        if (np >= 2)
        {
            var sa = 0.0;
            var sb = _dist;
            var wq = true;
            for (var i = 1; i < np; i++)
            {
                sa += xi;
                sb -= xi;
                q = pfl.Points[i] - (qc * sa + Transmitter.the) * sa - za;
                if (q > 0)
                {
                    Transmitter.the += q / sa;
                    Transmitter.dl = sa;
                    wq = false;
                }
                if (!wq)
                {
                    q = pfl.Points[i] - (qc * sb + Receiver.the) * sb - zb;
                    if (q > 0)
                    {
                        Receiver.the += q / sb;
                        Receiver.dl = sb;
                    }
                }
            }
        }
    }

    private static void z1sq1(Elevations z, double x1, double x2, out double z0, out double zn)
    {
        double xn = z.EndIndex;
        double xa = (int)Dim(x1 / z.DeltaDistance, 0);
        var xb = xn - (int)Dim(xn, x2 / z.DeltaDistance);
        if (xb <= xa)
        {
            xa = Dim(xa, 1);
            xb = xn - Dim(xn, xb + 1);
        }
        var ja = (int)xa;
        var jb = (int)xb;
        var n = jb - ja;
        xa = xb - xa;
        var x = -0.5 * xa;
        xb += x;
        var a = 0.5 * (z.Points[ja] + z.Points[jb]);
        var b = 0.5 * (z.Points[ja] - z.Points[jb]) * x;
        for (var i = 2; i <= n; i++)
        {
            ja++;
            x += 1;
            a += z.Points[ja];
            b += z.Points[ja] * x;
        }
        a /= xa;
        b = b * 12 / ((xa * xa + 2) * xa);
        z0 = a - b * xb;
        zn = a + b * (xn - xb);
    }

    private static double qtile(int nn, double[] a, int ir)
    {
        double q = 0;
        int j1 = 0, i0 = 0;
        var done = false;
        var goto10 = true;

        var m = 0;
        var n = nn;
        var k = Math.Min(Math.Max(0, ir), n);
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
                done = true;
        }
        return q;
    }

    private static double d1thx(Elevations pfl, double x1, double x2)
    {
        var np = pfl.EndIndex;
        var xa = x1 / pfl.DeltaDistance;
        var xb = x2 / pfl.DeltaDistance;
        if (xb - xa < 2)  // exit out
            return 0;
        var ka = (int)(0.1 * (xb - xa + 8));
        ka = Math.Min(Math.Max(4, ka), 25);
        var n = 10 * ka - 5;
        var kb = n - ka + 1;
        double sn = n - 1;
        xb = (xb - xa) / sn;
        var k = (int)(xa + 1);
        xa -= k;
        var s = new Elevations(n, 1);
        for (var j = 0; j < n; j++)
        {
            while (xa > 0 && k < np)
            {
                xa -= 1;
                k++;
            }
            s.Points[j] = pfl.Points[k] + (pfl.Points[k] - pfl.Points[k - 1]) * xa;
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
        d1thxv /= 1 - 0.8 * Math.Exp(-(x2 - x1) / 50000.0);
        return d1thxv;
    }

    private void qlrpfl(Elevations pfl, RadioClimate klimx, VariabilityMode mdvarx)
    {
        _dist = pfl.EndIndex * pfl.DeltaDistance;
        hzns(pfl);
        foreach (var antenna in Antennae)
            antenna.xl = Math.Min(15 * antenna.hg, 0.1 * antenna.dl);
        Receiver.xl = _dist - Receiver.xl;
        _dh = d1thx(pfl, Transmitter.xl, Receiver.xl);
        if (Transmitter.dl + Receiver.dl > 1.5 * _dist)
        {
            z1sq1(pfl, Transmitter.xl, Receiver.xl, out var za, out var zb);
            Transmitter.he = Transmitter.hg + Dim(pfl.FirstPoint, za);
            Receiver.he = Receiver.hg + Dim(pfl.LastPoint, zb);
            foreach (var antenna in Antennae)
                antenna.dl = Math.Sqrt(2 * antenna.he / _gme) * Math.Exp(-0.07 * Math.Sqrt(_dh / Math.Max(antenna.he, 5)));
            var q = Transmitter.dl + Receiver.dl;

            if (q <= _dist)
            {
                q = Math.Pow(_dist / q, 2);
                foreach (var antenna in Antennae)
                {
                    antenna.he *= q;
                    antenna.dl = Math.Sqrt(2 * antenna.he / _gme) * Math.Exp(-0.07 * Math.Sqrt(_dh / Math.Max(antenna.he, 5)));
                }
            }
            foreach (var antenna in Antennae)
            {
                q = Math.Sqrt(2 * antenna.he / _gme);
                antenna.the = (0.65 * _dh * (q / antenna.dl - 1) - 2 * antenna.he) / q;
            }
        }
        else
        {
            z1sq1(pfl, Transmitter.xl, 0.9 * Transmitter.dl, out var za, out _);
            z1sq1(pfl, _dist - 0.9 * Receiver.dl, Receiver.xl, out _, out var zb);
            Transmitter.he = Transmitter.hg + Dim(pfl.FirstPoint, za);
            Receiver.he = Receiver.hg + Dim(pfl.LastPoint, zb);
        }
        _mdp = ControlFlow.PointToPoint;
        _mdvar = mdvarx;
        _klim = klimx;
        _lvar = Changes.All;
        lrprop(0);
    }

    //********************************************************
    //* Point-To-Point Mode Calculations                     *
    //********************************************************

    public void point_to_pointMDH(double[] elev, double tht_m, double rht_m,
            double eps_dielect, double sgm_conductivity, double eno_ns_surfref,
            double frq_mhz, RadioClimate radio_climate, Polarization pol, VariabilityMode mdvar, double timepct, double locpct, double confpct,
            out double dbloss, out PropMode propmode, out double deltaH, out int errnum)
        // timepct, locpct, confpct: .01 to .99
        // elev[]: [num points - 1], [delta dist(meters)], [height(meters) point 1], ..., [height(meters) point n]
        // errnum: 0- No Error.
        //         1- Warning: Some parameters are nearly out of range.
        //                     Results should be used with caution.
        //         2- Note: Default parameters have been substituted for impossible ones.
        //         3- Warning: A combination of parameters is out of range.
        //                     Results are probably invalid.
        //         Other-  Warning: Some parameters are out of range.
        //                          Results are probably invalid.
    {
        _klim = radio_climate;
        _lvar = Changes.All;
        _mdp = ControlFlow.PointToPoint;
        _w1 = true; // possible bugfix: original embedded this value in mdvar
        _mdvar = mdvar; // bugfix: original used 'mobile'

        Transmitter.hg = tht_m;
        Receiver.hg = rht_m;

        var e = new Elevations(elev);
        qlrps(frq_mhz, GetAverage(elev), eno_ns_surfref, pol, eps_dielect, sgm_conductivity);
        qlrpfl(e, _klim, _mdvar);
        var fs = 32.45 + 20 * Math.Log10(frq_mhz) + 20 * Math.Log10(_dist / 1000.0);
        deltaH = _dh;
        propmode = GetPropMode();
        dbloss = avar(qerfi(timepct), qerfi(locpct), qerfi(confpct)) + fs;
        errnum = _kwx;
    }

    private double GetAverage(double[] elev)
    {
        var np = (int)elev[0];
        var ja = (int)(3.0 + 0.1 * elev[0]);
        var jb = np - ja + 6;
        double zsys = 0;
        for (var i = ja - 1; i < jb; ++i)
            zsys += elev[i];
        zsys /= (jb - ja + 1);
        return zsys;
    }

    private PropMode GetPropMode()
    {
        PropMode propmode;
        var q = (int)(_dist - _dla);

        if (q < 0)
            propmode = PropMode.LineOfSight;
        else if (q == 0)
            propmode = PropMode.SingleHorizon;
        else // q > 0
            propmode = PropMode.DoubleHorizon;

        if (propmode != PropMode.LineOfSight)
        {
            if (_dist <= _dlsa || _dist <= _dx)
                propmode |= PropMode.DiffractionDominant;
            else if (_dist > _dx)
                propmode |= PropMode.TroposcatterDominant;
        }

        return propmode;
    }


    //********************************************************
    //* Area Mode Calculations                               *
    //********************************************************

    public void area(VariabilityMode ModVar, double deltaH, double tht_m, double rht_m,
        double dist_km, SiteCriteria TSiteCriteria, SiteCriteria RSiteCriteria,
        double eps_dielect, double sgm_conductivity, double eno_ns_surfref,
        double frq_mhz, RadioClimate radio_climate, Polarization pol, double pctTime, double pctLoc,
        double pctConf, out double dbloss, out PropMode propmode, out int errnum)
    {
        // pctTime, pctLoc, pctConf: .01 to .99
        // errnum: 0- No Error.
        //         1- Warning: Some parameters are nearly out of range.
        //                     Results should be used with caution.
        //         2- Note: Default parameters have been substituted for impossible ones.
        //         3- Warning: A combination of parameters is out of range.
        //                     Results are probably invalid.
        //         Other-  Warning: Some parameters are out of range.
        //                          Results are probably invalid.
        _dh = deltaH;
        _klim = radio_climate;
        _ens = eno_ns_surfref;
        Transmitter.hg = tht_m;
        Transmitter.kst = TSiteCriteria;
        Receiver.hg = rht_m;
        Receiver.kst = RSiteCriteria;
        qlrps(frq_mhz, 0, eno_ns_surfref, pol, eps_dielect, sgm_conductivity);
        qlra(_klim, ModVar);
        if (_lvar < Changes.Distance)
            _lvar = Changes.Distance;
        lrprop(dist_km * 1000);
        var fs = 32.45 + 20 * Math.Log10(frq_mhz) + 20 * Math.Log10(_dist / 1000.0);
        var xlb = fs + avar(qerfi(pctTime), qerfi(pctLoc), qerfi(pctConf));
        dbloss = xlb;
        errnum = _kwx;
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