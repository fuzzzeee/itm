using System;
using System.Collections.Generic;
using System.Linq;
using System.Numerics;
using static System.Math;

namespace LongleyRice
{
    class Refactored
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

        // *************************************
        // C++ routines for this program are taken from
        // a translation of the FORTRAN code written by
        // U.S. Department of Commerce NTIA/ITS
        // Institute for Telecommunication Sciences
        // *****************
        // Irregular Terrain Model (ITM) (Longley-Rice)
        // *************************************


        // ReSharper disable once InconsistentNaming
        private const double THIRD = 1.0 / 3.0;

        // ReSharper disable once InconsistentNaming
        private class prop_type
        {
            public double aref;
            public double dist;
            public readonly double[] hg = new double[2];
            public double wn;
            public double dh;
            public double ens;
            public double gme;
            public Complex zgnd;
            public readonly double[] he = new double[2];
            public readonly double[] dl = new double[2];
            public readonly double[] the = new double[2];
            public int kwx;
            public ControlFlow mdp;
            public double sgc;
            public ControlSwitch lvar;
            public VariabilityMode mdvar;
            public RadioClimate klim;
            public double dlsa;
            public double dx;
            public double ael;
            public double ak1;
            public double ak2;
            public double aed;
            public double emd;
            public double aes;
            public double ems;
            public readonly double[] dls = new double[2];
            public double dla;
            public double tha;
        };

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

        // ReSharper disable once InconsistentNaming
        private static double aknfe(double v2)
        {
            if (v2 < 5.76)
                return 6.02 + 9.11 * Sqrt(v2) - 1.27 * v2;
            else
                return 12.953 + 4.343 * Log(v2);
        }

        // ReSharper disable once InconsistentNaming
        private static double fht(double x, double pk)
        {
            double fhtv;
            if (x < 200)
            {
                var w = -Log(pk);
                if (pk < 1e-5 || x * Pow(w, 3) > 5495)
                {
                    fhtv = -117;
                    if (x > 1)
                        fhtv = 17.372 * Log(x) + fhtv;
                }
                else
                {
                    fhtv = 2.5e-5 * x * x / pk - 8.686 * w - 15;
                }
            }
            else
            {
                fhtv = 0.05751 * x - 4.343 * Log(x);
                if (x < 2000)
                {
                    var w = 0.0134 * x * Exp(-0.005 * x);
                    fhtv = (1 - w) * fhtv + w * (17.372 * Log(x) - 117);
                }
            }
            return fhtv;
        }

        // ReSharper disable once InconsistentNaming
        private static readonly int[] _h0f_a = { 25, 80, 177, 395, 705 };
        // ReSharper disable once InconsistentNaming
        private static readonly int[] _h0f_b = { 24, 45, 68, 80, 105 };

        // ReSharper disable once InconsistentNaming
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
            var x = Pow(1 / r, 2);
            // ReSharper disable once InconsistentNaming
            var h0fv = 4.343 * Log((_h0f_a[it - 1] * x + _h0f_b[it - 1]) * x + 1);
            // ReSharper disable once CompareOfFloatsByEqualityOperator
            if (q != 0)
                h0fv = (1 - q) * h0fv + q * 4.343 * Log((_h0f_a[it] * x + _h0f_b[it]) * x + 1);
            return h0fv;
        }

        // ReSharper disable once InconsistentNaming
        private static readonly double[] _ahd_a = { 133.4, 104.6, 71.8 };
        // ReSharper disable once InconsistentNaming
        private static readonly double[] _ahd_b = { 0.332e-3, 0.212e-3, 0.157e-3 };
        // ReSharper disable once InconsistentNaming
        private static readonly double[] _ahd_c = { -4.343, -1.086, 2.171 };
        // ReSharper disable once InconsistentNaming
        private static double ahd(double td)
        {
            int i;
            if (td <= 10000)
                i = 0;
            else if (td <= 70000)
                i = 1;
            else
                i = 2;
            return _ahd_a[i] + _ahd_b[i] * td + _ahd_c[i] * Log(td);
        }

        private void init_adiff(prop_type prop)
        {
            var q = prop.hg[0] * prop.hg[1];
            _qk = prop.he[0] * prop.he[1] - q;
            if (prop.mdp == ControlFlow.PointToPoint)
                q += 10;
            _wd1 = Sqrt(1 + _qk / q);
            _xd1 = prop.dla + prop.tha / prop.gme;
            q = (1 - 0.8 * Exp(-prop.dlsa / 50000)) * prop.dh;
            q *= 0.78 * Exp(-Pow(q / 16.0, 0.25));
            _afo = Min(15, 2.171 * Log(1 + 4.77e-4 * prop.hg[0] * prop.hg[1] * prop.wn * q));
            _qk = 1 / Complex.Abs(prop.zgnd);
            _aht = 20;
            _xht = 0;
            for (var j = 0; j < 2; j++)
            {
                var a = 0.5 * Pow(prop.dl[j], 2) / prop.he[j];
                var wa = Pow(a * prop.wn, THIRD);
                var pk = _qk / wa;
                q = (1.607 - pk) * 151 * wa * prop.dl[j] / a;
                _xht += q;
                _aht += fht(q, pk);
            }
        }

        private double _wd1, _xd1, _afo, _qk, _aht, _xht;
        // ReSharper disable once InconsistentNaming
        private double adiff(double d, prop_type prop)
        {
            var th = prop.tha + d * prop.gme;
            var ds = d - prop.dla;
            var q = 0.0795775 * prop.wn * ds * Pow(th, 2);
            var adiffv = aknfe(q * prop.dl[0] / (ds + prop.dl[0])) + aknfe(q * prop.dl[1] / (ds + prop.dl[1]));
            var a = ds / th;
            var wa = Pow(a * prop.wn, THIRD);
            var pk = _qk / wa;
            q = (1.607 - pk) * 151 * wa * th + _xht;
            var ar = 0.05751 * q - 4.343 * Log(q) - _aht;
            q = (_wd1 + _xd1 / d) * Min((1 - 0.8 * Exp(-d / 50000)) * prop.dh * prop.wn, 6283.2);
            var wd = 25.1 / (25.1 + Sqrt(q));
            return ar * wd + (1 - wd) * adiffv + _afo;
        }

        private void init_ascat(prop_type prop)
        {
            _ad = prop.dl[0] - prop.dl[1];
            _rr = prop.he[1] / prop.he[0];
            if (_ad < 0)
            {
                _ad = -_ad;
                _rr = 1 / _rr;
            }
            _etq = (5.67e-6 * prop.ens - 2.32e-3) * prop.ens + 0.031;
            _h0s = -15;
        }

        private double _ad, _rr, _etq, _h0s;
        // ReSharper disable once InconsistentNaming
        private double ascat(double d, prop_type prop)
        {
            double th;
            double h0;
            if (_h0s > 15)
                h0 = _h0s;
            else
            {
                th = prop.the[0] + prop.the[1] + d * prop.gme;
                var r2 = 2 * prop.wn * th;
                var r1 = r2 * prop.he[0];
                r2 *= prop.he[1];
                if (r1 < 0.2 && r2 < 0.2)
                    return 1001;  // <==== early return
                var ss = (d - _ad) / (d + _ad);
                var q = _rr / ss;
                ss = Max(0.1, ss);
                q = Min(Max(0.1, q), 10);
                var z0 = (d - _ad) * (d + _ad) * th * 0.25 / d;
                var et = (_etq * Exp(-Pow(Min(1.7, z0 / 8000.0), 6)) + 1) * z0 / 1.7556e3;
                var ett = Max(et, 1);
                h0 = (h0f(r1, ett) + h0f(r2, ett)) * 0.5;
                h0 += Min(h0, (1.38 - Log(ett)) * Log(ss) * Log(q) * 0.49);
                h0 = Dim(h0, 0);
                if (et < 1)
                    h0 = et * h0 + (1 - et) * 4.343 * Log(Pow((1 + 1.4142 / r1) * (1 + 1.4142 / r2), 2) * (r1 + r2) / (r1 + r2 + 2.8284));
                if (h0 > 15 && _h0s >= 0)
                    h0 = _h0s;
            }
            _h0s = h0;
            th = prop.tha + d * prop.gme;
            return ahd(th * d) + 4.343 * Log(47.7 * prop.wn * Pow(th, 4)) - 0.1 * (prop.ens - 301) * Exp(-th * d / 40000.0) + h0;
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
            var t = Max(0.5 - Abs(x), 0.000001);
            t = Sqrt(-2 * Log(t));
            var v = t - ((c2 * t + c1) * t + c0) / (((d3 * t + d2) * t + d1) * t + 1);
            if (x < 0)
                v = -v;
            return v;
        }

        // ReSharper disable once InconsistentNaming
        private static void qlrps(double fmhz, double zsys, double en0, Polarization ipol, double eps, double sgm, prop_type prop)
        {
            const double gma = 157e-9;
            prop.wn = fmhz / 47.7;
            prop.ens = en0;
            // ReSharper disable once CompareOfFloatsByEqualityOperator
            if (zsys != 0)
                prop.ens *= Exp(-zsys / 9460.0);
            prop.gme = gma * (1 - 0.04665 * Exp(prop.ens / 179.3));
            var zq = new Complex(eps, 376.62 * sgm / prop.wn);
            // ReSharper disable once InconsistentNaming
            var prop_zgnd = Complex.Sqrt(zq - 1);
            if (ipol != Polarization.Horizontal)
                prop_zgnd = prop_zgnd / zq;

            prop.zgnd = prop_zgnd;
        }

        private static double abq_alos(Complex r)
        {
            return r.Real * r.Real + r.Imaginary * r.Imaginary;
        }

        private void init_alos(prop_type prop)
        {
            _wls = 0.021 / (0.021 + prop.wn * prop.dh / Max(10000, prop.dlsa));
        }

        private double _wls;
        // ReSharper disable once InconsistentNaming
        private double alos(double d, prop_type prop)
        {
            var q = (1 - 0.8 * Exp(-d / 50000.0)) * prop.dh;
            var s = 0.78 * q * Exp(-Pow(q / 16.0, 0.25));
            q = prop.he[0] + prop.he[1];
            var sps = q / Sqrt(d * d + q * q);
            var r = (sps - prop.zgnd) / (sps + prop.zgnd) * Exp(-Min(10, prop.wn * s * sps));
            q = abq_alos(r);
            if (q < 0.25 || q < sps)
                r = r * Sqrt(sps / q);
            var alosv = prop.emd * d + prop.aed;
            q = prop.wn * prop.he[0] * prop.he[1] * 2 / d;
            if (q > 1.57)
                q = 3.14 - 2.4649 / q;
            return (-4.343 * Log(abq_alos(new Complex(Cos(q), -Sin(q)) + r)) - alosv) * _wls + alosv;
        }


        private static void qlra(SiteCriteria[] kst, RadioClimate klimx, VariabilityMode mdvarx, prop_type prop)
        {
            for (var j = 0; j < 2; ++j)
            {
                double q;
                if (kst[j] == SiteCriteria.Random)
                    prop.he[j] = prop.hg[j];
                else
                {
                    if (kst[j] != SiteCriteria.Careful)
                        q = 9;
                    else
                        q = 4;
                    if (prop.hg[j] < 5)
                        q *= Sin(0.3141593 * prop.hg[j]);
                    prop.he[j] = prop.hg[j] + (1 + q) * Exp(-Min(20, 2 * prop.hg[j] / Max(1e-3, prop.dh)));
                }
                q = Sqrt(2 * prop.he[j] / prop.gme);
                prop.dl[j] = q * Exp(-0.07 * Sqrt(prop.dh / Max(prop.he[j], 5)));
                prop.the[j] = (0.65 * prop.dh * (q / prop.dl[j] - 1) - 2 * prop.he[j]) / q;
            }
            prop.mdp = ControlFlow.AreaBegin;
            prop.mdvar = mdvarx;
            prop.klim = klimx;
            prop.lvar = ControlSwitch.New;
        }

        private bool _wlos, _wscat;
        private double _dmin, _xae;
        // ReSharper disable once InconsistentNaming
        private void lrprop(double d, prop_type prop)  // PaulM_lrprop
        {
            if (prop.mdp != ControlFlow.AreaContinue)
            {
                for (var j = 0; j < 2; j++)
                    prop.dls[j] = Sqrt(2 * prop.he[j] / prop.gme);
                prop.dlsa = prop.dls[0] + prop.dls[1];
                prop.dla = prop.dl[0] + prop.dl[1];
                prop.tha = Max(prop.the[0] + prop.the[1], -prop.dla * prop.gme);
                _wlos = false;
                _wscat = false;
                if (prop.wn < 0.838 || prop.wn > 210)
                {
                    prop.kwx = Max(prop.kwx, 1);
                }
                for (var j = 0; j < 2; j++)
                    if (prop.hg[j] < 1 || prop.hg[j] > 1000)
                    {
                        prop.kwx = Max(prop.kwx, 1);
                    }
                for (var j = 0; j < 2; j++)
                    if (Abs(prop.the[j]) > 200e-3 || prop.dl[j] < 0.1 * prop.dls[j] ||
                       prop.dl[j] > 3 * prop.dls[j])
                    {
                        prop.kwx = Max(prop.kwx, 3);
                    }
                if (prop.ens < 250 || prop.ens > 400 ||
                    prop.gme < 75e-9 || prop.gme > 250e-9 ||
                    prop.zgnd.Real <= Abs(prop.zgnd.Imaginary) ||
                    prop.wn < 0.419 || prop.wn > 420)
                {
                    prop.kwx = 4;
                }
                for (var j = 0; j < 2; j++)
                    if (prop.hg[j] < 0.5 || prop.hg[j] > 3000)
                    {
                        prop.kwx = 4;
                    }
                _dmin = Abs(prop.he[0] - prop.he[1]) / 200e-3;
                init_adiff(prop);
                _xae = Pow(prop.wn * Pow(prop.gme, 2), -THIRD);
                var d3 = Max(prop.dlsa, 1.3787 * _xae + prop.dla);
                var d4 = d3 + 2.7574 * _xae;
                var a3 = adiff(d3, prop);
                var a4 = adiff(d4, prop);
                prop.emd = (a4 - a3) / (d4 - d3);
                prop.aed = a3 - prop.emd * d3;
            }
            if (prop.mdp != ControlFlow.PointToPoint)
            {
                prop.mdp = ControlFlow.PointToPoint;
                prop.dist = d;
            }
            if (prop.dist > 0)
            {
                if (prop.dist > 1000000)
                {
                    prop.kwx = Max(prop.kwx, 1);
                }
                if (prop.dist < _dmin)
                {
                    prop.kwx = Max(prop.kwx, 3);
                }
                if (prop.dist < 1000 || prop.dist > 2000000)
                {
                    prop.kwx = 4;
                }
            }
            if (prop.dist < prop.dlsa)
            {
                if (!_wlos)
                {
                    init_alos(prop);
                    var d2 = prop.dlsa;
                    var a2 = prop.aed + d2 * prop.emd;
                    var d0 = 1.908 * prop.wn * prop.he[0] * prop.he[1];
                    double d1;
                    if (prop.aed >= 0)
                    {
                        d0 = Min(d0, 0.5 * prop.dla);
                        d1 = d0 + 0.25 * (prop.dla - d0);
                    }
                    else
                        d1 = Max(-prop.aed / prop.emd, 0.25 * prop.dla);
                    var a1 = alos(d1, prop);
                    var wq = false;
                    if (d0 < d1)
                    {
                        var a0 = alos(d0, prop);
                        var q = Log(d2 / d0);
                        prop.ak2 = Max(0, ((d2 - d0) * (a1 - a0) - (d1 - d0) * (a2 - a0)) / ((d2 - d0) * Log(d1 / d0) - (d1 - d0) * q));
                        wq = prop.aed >= 0 || prop.ak2 > 0;
                        if (wq)
                        {
                            prop.ak1 = (a2 - a0 - prop.ak2 * q) / (d2 - d0);
                            if (prop.ak1 < 0)
                            {
                                prop.ak1 = 0;
                                prop.ak2 = Dim(a2, a0) / q;
                                // ReSharper disable once CompareOfFloatsByEqualityOperator
                                if (prop.ak2 == 0)
                                    prop.ak1 = prop.emd;
                            }
                        }
                    }
                    if (!wq)
                    {
                        prop.ak1 = Dim(a2, a1) / (d2 - d1);
                        prop.ak2 = 0;
                        // ReSharper disable once CompareOfFloatsByEqualityOperator
                        if (prop.ak1 == 0)
                            prop.ak1 = prop.emd;
                    }
                    prop.ael = a2 - prop.ak1 * d2 - prop.ak2 * Log(d2);
                    _wlos = true;
                }
                if (prop.dist > 0)
                    prop.aref = prop.ael + prop.ak1 * prop.dist + prop.ak2 * Log(prop.dist);
            }
            if (prop.dist <= 0 || prop.dist >= prop.dlsa)
            {
                if (!_wscat)
                {
                    init_ascat(prop);
                    var d5 = prop.dla + 200000;
                    var d6 = d5 + 200000;
                    var a6 = ascat(d6, prop);
                    var a5 = ascat(d5, prop);
                    if (a5 < 1000)
                    {
                        prop.ems = (a6 - a5) / 200000.0;
                        prop.dx = Max(prop.dlsa, Max(prop.dla + 0.3 * _xae * Log(47.7 * prop.wn), (a5 - prop.aed - prop.ems * d5) / (prop.emd - prop.ems)));
                        prop.aes = (prop.emd - prop.ems) * prop.dx + prop.aed;
                    }
                    else
                    {
                        prop.ems = prop.emd;
                        prop.aes = prop.aed;
                        prop.dx = 10000000;
                    }
                    _wscat = true;
                }
                if (prop.dist > prop.dx)
                    prop.aref = prop.aes + prop.ems * prop.dist;
                else
                    prop.aref = prop.aed + prop.emd * prop.dist;
            }
            prop.aref = Max(prop.aref, 0);
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
                cv1 = bv1;
                cv2 = bv2;
                yv1 = xv1;
                yv2 = xv2;
                yv3 = xv3;
                csm1 = bsm1;
                csm2 = bsm2;
                ysm1 = xsm1;
                ysm2 = xsm2;
                ysm3 = xsm3;
                csp1 = bsp1;
                csp2 = bsp2;
                ysp1 = xsp1;
                ysp2 = xsp2;
                ysp3 = xsp3;
                csd1 = bsd1;
                zd = bzd1;
                cfm1 = bfm1;
                cfm2 = bfm2;
                cfm3 = bfm3;
                cfp1 = bfp1;
                cfp2 = bfp2;
                cfp3 = bfp3;
            }
            public readonly double cv1;
            public readonly double cv2;
            public readonly double yv1;
            public readonly double yv2;
            public readonly double yv3;
            public readonly double csm1;
            public readonly double csm2;
            public readonly double ysm1;
            public readonly double ysm2;
            public readonly double ysm3;
            public readonly double csp1;
            public readonly double csp2;
            public readonly double ysp1;
            public readonly double ysp2;
            public readonly double ysp3;
            public readonly double csd1;
            public readonly double zd;
            public readonly double cfm1;
            public readonly double cfm2;
            public readonly double cfm3;
            public readonly double cfp1;
            public readonly double cfp2;
            public readonly double cfp3;
        }

        private static readonly Dictionary<RadioClimate, ClimateSettings> _climateSettings = new Dictionary<RadioClimate, ClimateSettings> {
            { RadioClimate.Equatorial, new ClimateSettings (-9.67, 12.7, 144.9e3, 190.3e3, 133.8e3, 2.13, 159.5, 762.2e3, 123.6e3, 94.5e3, 2.11, 102.3, 636.9e3, 134.8e3, 95.6e3, 1.224, 1.282, 1.0, 0.0, 0.0, 1.0, 0.0, 0.0 ) },
            { RadioClimate.ContinentalSubtropical, new ClimateSettings (-0.62, 9.19, 228.9e3, 205.2e3, 143.6e3, 2.66, 7.67, 100.4e3, 172.5e3, 136.4e3, 6.87, 15.53, 138.7e3, 143.7e3, 98.6e3, 0.801, 2.161, 1.0, 0.0, 0.0, 0.93, 0.31, 2.00 ) },
            { RadioClimate.MaritimeSubtropical, new ClimateSettings (1.26, 15.5, 262.6e3, 185.2e3, 99.8e3, 6.11, 6.65, 138.2e3, 242.2e3, 178.6e3, 10.08, 9.60, 165.3e3, 225.7e3, 129.7e3, 1.380, 1.282, 1.0, 0.0, 0.0, 1.0, 0.0, 0.0 ) },
            { RadioClimate.Desert, new ClimateSettings (-9.21, 9.05, 84.1e3, 101.1e3, 98.6e3, 1.98, 13.11, 139.1e3, 132.7e3, 193.5e3, 3.68, 159.3, 464.4e3, 93.1e3, 94.2e3, 1.000, 20.0, 1.0, 0.0, 0.0, 0.93, 0.19, 1.79 ) },
            { RadioClimate.ContinentalTemperate, new ClimateSettings (-0.62, 9.19, 228.9e3, 205.2e3, 143.6e3, 2.68, 7.16, 93.7e3, 186.8e3, 133.5e3, 4.75, 8.12, 93.2e3, 135.9e3, 113.4e3, 1.224, 1.282, 0.92, 0.25, 1.77, 0.93, 0.31, 2.00 ) },
            { RadioClimate.MaritimeOverLand, new ClimateSettings (-0.39, 2.86, 141.7e3, 315.9e3, 167.4e3, 6.86, 10.38, 187.8e3, 169.6e3, 108.9e3, 8.58, 13.97, 216.0e3, 152.0e3, 122.7e3, 1.518, 1.282, 1.0, 0.0, 0.0, 1.0, 0.0, 0.0 ) },
            { RadioClimate.MaritimeOverSea, new ClimateSettings (3.15, 857.9, 2222.0e3, 164.8e3, 116.3e3, 8.51, 169.8, 609.8e3, 119.9e3, 106.6e3, 8.43, 8.19, 136.2e3, 188.5e3, 122.9e3, 1.518, 1.282, 1.0, 0.0, 0.0, 1.0, 0.0, 0.0 ) },
        };

        private VariabilityMode _kdv;
        private double _dexa, _de, _vmd, _vs0, _sgl, _sgtm, _sgtp, _sgtd, _tgtd, _gm, _gp;
        private ClimateSettings _cs;

        private bool _w1;
        // ReSharper disable once InconsistentNaming
        private double avar(double zzt, double zzl, double zzc, prop_type prop)
        {
            const double rt = 7.8, rl = 24.0;
            double sgt, yr;

            if (prop.lvar > ControlSwitch.Complete)
            {
                double q;
                switch (prop.lvar)
                {
                    default:
                        _cs = _climateSettings[prop.klim];
                        _kdv = prop.mdvar;
                        q = Log(0.133 * prop.wn);
                        _gm = _cs.cfm1 + _cs.cfm2 / (Pow(_cs.cfm3 * q, 2) + 1);
                        _gp = _cs.cfp1 + _cs.cfp2 / (Pow(_cs.cfp3 * q, 2) + 1);
                        _dexa = Sqrt(18e6 * prop.he[0]) + Sqrt(18e6 * prop.he[1]) + Pow(575.7e12 / prop.wn, THIRD);
                        goto case ControlSwitch.InProgress;
                    case ControlSwitch.InProgress:
                        if (prop.dist < _dexa)
                            _de = 130000 * prop.dist / _dexa;
                        else
                            _de = 130000 + prop.dist - _dexa;
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
                    q = (1 - 0.8 * Exp(-prop.dist / 50000.0)) * prop.dh * prop.wn;
                    _sgl = 10 * q / (q + 13);
                }
                _vs0 = Pow(5 + 3 * Exp(-_de / 100000.0), 2);
                prop.lvar = ControlSwitch.Complete;
            }
            var zt = zzt;
            var zl = zzl;
            var zc = zzc;
            switch (_kdv)
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
            if (Abs(zt) > 3.1 || Abs(zl) > 3.1 || Abs(zc) > 3.1)
            {
                prop.kwx = Max(prop.kwx, 1);
            }
            if (zt < 0)
                sgt = _sgtm;
            else if (zt <= _cs.zd)
                sgt = _sgtp;
            else
                sgt = _sgtd + _tgtd / zt;
            var vs = _vs0 + Pow(sgt * zt, 2) / (rt + zc * zc) + Pow(_sgl * zl, 2) / (rl + zc * zc);
            switch (_kdv)
            {
                case VariabilityMode.Single:
                    yr = 0;
                    prop.sgc = Sqrt(sgt * sgt + _sgl * _sgl + vs);
                    break;
                case VariabilityMode.Individual:
                    yr = sgt * zt;
                    prop.sgc = Sqrt(_sgl * _sgl + vs);
                    break;
                case VariabilityMode.Mobile:
                    yr = Sqrt(sgt * sgt + _sgl * _sgl) * zt;
                    prop.sgc = Sqrt(vs);
                    break;
                default:
                    yr = sgt * zt + _sgl * zl;
                    prop.sgc = Sqrt(vs);
                    break;
            }
            var avarv = prop.aref - _vmd - yr - prop.sgc * zc;
            if (avarv < 0)
                avarv = avarv * (29 - avarv) / (29 - 10 * avarv);
            return avarv;

        }

        // ReSharper disable once InconsistentNaming
        private static void hzns(Elevations pfl, prop_type prop)
        {
            var np = pfl.EndIndex;
            var xi = pfl.DeltaDistance;
            var za = pfl.FirstPoint + prop.hg[0];
            var zb = pfl.LastPoint + prop.hg[1];
            var qc = 0.5 * prop.gme;
            var q = qc * prop.dist;
            prop.the[1] = (zb - za) / prop.dist;
            prop.the[0] = prop.the[1] - q;
            prop.the[1] = -prop.the[1] - q;
            prop.dl[0] = prop.dist;
            prop.dl[1] = prop.dist;
            if (np >= 2)
            {
                var sa = 0.0;
                var sb = prop.dist;
                var wq = true;
                for (var i = 1; i < np; i++)
                {
                    sa += xi;
                    sb -= xi;
                    q = pfl.Points[i] - (qc * sa + prop.the[0]) * sa - za;
                    if (q > 0)
                    {
                        prop.the[0] += q / sa;
                        prop.dl[0] = sa;
                        wq = false;
                    }
                    if (!wq)
                    {
                        q = pfl.Points[i] - (qc * sb + prop.the[1]) * sb - zb;
                        if (q > 0)
                        {
                            prop.the[1] += q / sb;
                            prop.dl[1] = sb;
                        }
                    }
                }
            }
        }

        // ReSharper disable once InconsistentNaming
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
            bool done = false;
            bool goto10 = true;

            var m = 0;
            var n = nn;
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
                    var r = a[i]; a[i] = a[j]; a[j] = r;
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

        // ReSharper disable once InconsistentNaming
        private static double d1thx(Elevations pfl, double x1, double x2)
        {
            var np = pfl.EndIndex;
            var xa = x1 / pfl.DeltaDistance;
            var xb = x2 / pfl.DeltaDistance;
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
                while (xa > 0 && k < np)
                {
                    xa -= 1;
                    k++;
                }
                s.Points[j] = pfl.Points[k] + (pfl.Points[k] - pfl.Points[k - 1]) * xa;
                xa = xa + xb;
            }
            z1sq1(s, 0, sn, out xa, out xb);
            xb = (xb - xa) / sn;
            for (var j = 0; j < n; j++)
            {
                s.Points[j] -= xa;
                xa = xa + xb;
            }
            // ReSharper disable once InconsistentNaming
            var d1thxv = qtile(n - 1, s.Points, ka - 1) - qtile(n - 1, s.Points, kb - 1);
            d1thxv /= 1 - 0.8 * Exp(-(x2 - x1) / 50000.0);
            return d1thxv;
        }

        // ReSharper disable once InconsistentNaming
        private void qlrpfl(Elevations pfl, RadioClimate klimx, VariabilityMode mdvarx, prop_type prop)
        {
            var xl = new double[2];

            prop.dist = pfl.EndIndex * pfl.DeltaDistance;
            hzns(pfl, prop);
            for (var j = 0; j < 2; j++)
                xl[j] = Min(15 * prop.hg[j], 0.1 * prop.dl[j]);
            xl[1] = prop.dist - xl[1];
            prop.dh = d1thx(pfl, xl[0], xl[1]);
            if (prop.dl[0] + prop.dl[1] > 1.5 * prop.dist)
            {
                z1sq1(pfl, xl[0], xl[1], out var za, out var zb);
                prop.he[0] = prop.hg[0] + Dim(pfl.FirstPoint, za);
                prop.he[1] = prop.hg[1] + Dim(pfl.LastPoint, zb);
                for (var j = 0; j < 2; j++)
                    prop.dl[j] = Sqrt(2 * prop.he[j] / prop.gme) *
                                Exp(-0.07 * Sqrt(prop.dh / Max(prop.he[j], 5)));
                var q = prop.dl[0] + prop.dl[1];

                if (q <= prop.dist)
                {
                    q = Pow(prop.dist / q, 2);
                    for (var j = 0; j < 2; j++)
                    {
                        prop.he[j] *= q;
                        prop.dl[j] = Sqrt(2 * prop.he[j] / prop.gme) *
                              Exp(-0.07 * Sqrt(prop.dh / Max(prop.he[j], 5)));
                    }
                }
                for (var j = 0; j < 2; j++)
                {
                    q = Sqrt(2 * prop.he[j] / prop.gme);
                    prop.the[j] = (0.65 * prop.dh * (q / prop.dl[j] - 1) - 2 * prop.he[j]) / q;
                }
            }
            else
            {
                z1sq1(pfl, xl[0], 0.9 * prop.dl[0], out var za, out _);
                z1sq1(pfl, prop.dist - 0.9 * prop.dl[1], xl[1], out _, out var zb);
                prop.he[0] = prop.hg[0] + Dim(pfl.FirstPoint, za);
                prop.he[1] = prop.hg[1] + Dim(pfl.LastPoint, zb);
            }
            prop.mdp = ControlFlow.PointToPoint;
            prop.mdvar = mdvarx;
            prop.klim = klimx;
            prop.lvar = ControlSwitch.New;
            lrprop(0, prop);
        }

        //********************************************************
        //* Point-To-Point Mode Calculations                     *
        //********************************************************

        public void point_to_pointMDH(double[] elev, double tht_m, double rht_m,
                  double eps_dielect, double sgm_conductivity, double eno_ns_surfref,
                  double frq_mhz, RadioClimate radio_climate, Polarization pol, double timepct, double locpct, double confpct,
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
            var prop = new prop_type();
            prop.hg[0] = tht_m;
            prop.hg[1] = rht_m;
            prop.klim = radio_climate;
            prop.lvar = ControlSwitch.New;
            prop.mdp = ControlFlow.PointToPoint;
            _w1 = true; // possible bugfix: original embedded this value in mdvar
            prop.mdvar = VariabilityMode.Broadcast; // bugfix: original used 'mobile'

            var e = new Elevations(elev);
            qlrps(frq_mhz, e.Points.Average(), eno_ns_surfref, pol, eps_dielect, sgm_conductivity, prop);
            qlrpfl(e, prop.klim, prop.mdvar, prop);
            var fs = 32.45 + 20 * Log10(frq_mhz) + 20 * Log10(prop.dist / 1000.0);
            deltaH = prop.dh;
            propmode = GetPropMode(prop);
            dbloss = avar(qerfi(timepct), qerfi(locpct), qerfi(confpct), prop) + fs;      //avar(time,location,confidence)
            errnum = prop.kwx;
        }

        private static PropMode GetPropMode(prop_type prop)
        {
            PropMode propmode;
            var q = (int)(prop.dist - prop.dla);

            if (q < 0)
                propmode = PropMode.LineOfSight;
            else if (q == 0)
                propmode = PropMode.SingleHorizon;
            else // q > 0
                propmode = PropMode.DoubleHorizon;

            if (propmode != 0)
            {
                if (prop.dist <= prop.dlsa || prop.dist <= prop.dx)
                    propmode |= PropMode.DiffractionDominant;
                else if (prop.dist > prop.dx)
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
                  double pctConf, out double dbloss, out int errnum)
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
            var prop = new prop_type();
            prop.dh = deltaH;
            prop.hg[0] = tht_m;
            prop.hg[1] = rht_m;
            prop.klim = radio_climate;
            prop.ens = eno_ns_surfref;
            qlrps(frq_mhz, 0, eno_ns_surfref, pol, eps_dielect, sgm_conductivity, prop);
            var kst = new[] { TSiteCriteria, RSiteCriteria };
            qlra(kst, prop.klim, ModVar, prop);
            if (prop.lvar < ControlSwitch.InProgress)
                prop.lvar = ControlSwitch.InProgress;
            lrprop(dist_km * 1000, prop);
            var fs = 32.45 + 20 * Log10(frq_mhz) + 20 * Log10(prop.dist / 1000.0);
            var xlb = fs + avar(qerfi(pctTime), qerfi(pctLoc), qerfi(pctConf), prop);
            dbloss = xlb;
            errnum = prop.kwx;
        }

        private enum ControlFlow
        {
            PointToPoint = -1,
            AreaContinue = 0,
            AreaBegin = 1,
        }

        private enum ControlSwitch
        {
            Complete = 0,
            InProgress = 1,
            New = 5,
        }
    }

    public enum Polarization
    {
        Horizontal = 0,
        Vertical = 1,
    }

    public enum SiteCriteria
    {
        Random = 0,
        Careful = 1,
        VeryCareful = 2,
    }

    public enum RadioClimate
    {
        /// <summary>
        /// Congo
        /// </summary>
        Equatorial = 1,
        /// <summary>
        /// Sudan
        /// </summary>
        ContinentalSubtropical = 2,
        /// <summary>
        /// West Coast of Africa
        /// </summary>
        MaritimeSubtropical = 3,
        /// <summary>
        /// Sahara
        /// </summary>
        Desert = 4,
        /// <summary>
        /// Use for Avg. Atmospheric Conditions
        /// </summary>
        ContinentalTemperate = 5,
        /// <summary>
        /// UK and Continental West Coast
        /// </summary>
        MaritimeOverLand = 6,
        MaritimeOverSea = 7,
    }

    public enum GroundQuality
    {
        Average,
        Poor,
        Good,
        FreshWater,
        SeaWater,
    }

    public enum TerrainType
    {
        Flat = 0,
        Plains = 30,
        /// <summary>
        /// Use for Avg. Terrain
        /// </summary>
        Hills = 90,
        Mountains = 200,
        RuggedMountains = 500,
    }
    
    public enum VariabilityMode
    {
        /// <summary>
        /// Confidence is Time/Situation/Location, Time, Location not used
        /// </summary>
        Single = 0,
        /// <summary>
        /// Time is Situation/Location, Confidence is Confidence, Location not used
        /// </summary>
        Individual = 1,
        /// <summary>
        /// Time is Time/Location (Reliability), Confidence is Confidence, Location not used
        /// </summary>
        Mobile = 2,
        /// <summary>
        /// Time is Time, Location is Location, Confidence is Confidence
        /// </summary>
        Broadcast = 3,
    }

    [Flags]
    public enum PropMode
    {
        LineOfSight = 0,
        SingleHorizon = 4,
        DoubleHorizon = 8,
        DiffractionDominant = 1,
        TroposcatterDominant = 2,
    }
}
