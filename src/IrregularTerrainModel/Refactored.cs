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


        // ReSharper disable once InconsistentNaming
        private const double THIRD = 1.0 / 3.0;

        // ReSharper disable once InconsistentNaming
        private class prop_type
        {
            public readonly Antenna Transmitter = new Antenna();
            public readonly Antenna Receiver = new Antenna();
            public readonly Antenna[] Antennae;

            public prop_type()
            {
                Antennae = new[] { Transmitter, Receiver };
            }

            /// <summary>
            /// Reference attenuation
            /// </summary>
            public double aref;
            /// <summary>
            /// Distance
            /// </summary>
            public double dist;
            /// <summary>
            /// Wave number (radio frequency)
            /// </summary>
            public double wn;
            /// <summary>
            /// Terrain irregularity parameter
            /// </summary>
            public double dh;
            /// <summary>
            /// Surface refractivity
            /// </summary>
            public double ens;
            /// <summary>
            /// Earth's effective curvature
            /// </summary>
            public double gme;
            /// <summary>
            /// Surface transfer impedance of the ground
            /// </summary>
            public Complex zgnd;
            /// <summary>
            /// Error indicator
            /// </summary>
            public int kwx;
            /// <summary>
            /// Controlling mode
            /// </summary>
            public ControlFlow mdp;
            /// <summary>
            /// Standard deviation of situation variability (with what confidence will a threshold signal level be exceeded)
            /// </summary>
            public double sgc;
            /// <summary>
            /// A control switch
            /// </summary>
            public Changes lvar;
            /// <summary>
            /// Desired mode of variability
            /// </summary>
            public VariabilityMode mdvar;
            /// <summary>
            /// Climate indicator
            /// </summary>
            public RadioClimate klim;
            /// <summary>
            /// Line-of-sight distance
            /// </summary>
            public double dlsa;
            /// <summary>
            /// Scatter distance
            /// </summary>
            public double dx;
            /// <summary>
            /// Line-of-sight coefficient
            /// </summary>
            public double ael;
            /// <summary>
            /// Line-of-sight coefficient
            /// </summary>
            public double ak1;
            /// <summary>
            /// Line-of-sight coefficient
            /// </summary>
            public double ak2;
            /// <summary>
            /// Diffraction coefficient
            /// </summary>
            public double aed;
            /// <summary>
            /// Diffraction coefficient
            /// </summary>
            public double emd;
            /// <summary>
            /// Scatter coefficient
            /// </summary>
            public double aes;
            /// <summary>
            /// Scatter coefficient
            /// </summary>
            public double ems;
            /// <summary>
            /// Total horizon distance
            /// </summary>
            public double dla;
            /// <summary>
            /// Total bending angle
            /// </summary>
            public double tha;

            public double ad, rr, etq, h0s;

            public double dexa, de, vmd, vs0, sgl, sgtm, sgtp, sgtd, tgtd, gm, gp;

            public double dmin, xae;

            public double wls;

            public ClimateSettings cs;

            public bool w1;

            public double wd1, xd1, afo, qk, aht, xht;

            public bool wlos, wscat;
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

        /// <summary>
        /// The attenuation due to a single knife edge - the Fresnel integral (in decibels) as a function of <paramref name="v2"/>
        /// </summary>
        /// <param name="v2"></param>
        /// <returns></returns>
        // ReSharper disable once InconsistentNaming
        private static double aknfe(double v2)
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

        /// <summary>
        /// H01 function for scatter fields
        /// </summary>
        /// <param name="r"></param>
        /// <param name="et"></param>
        /// <returns></returns>
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

        /// <summary>
        /// The F(0d) function for scatter fields
        /// </summary>
        /// <param name="td"></param>
        /// <returns></returns>
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
            var q = prop.Transmitter.hg * prop.Receiver.hg;
            prop.qk = prop.Transmitter.he * prop.Receiver.he - q;
            if (prop.mdp == ControlFlow.PointToPoint)
                q += 10;
            prop.wd1 = Sqrt(1 + prop.qk / q);
            prop.xd1 = prop.dla + prop.tha / prop.gme;
            q = (1 - 0.8 * Exp(-prop.dlsa / 50000.0)) * prop.dh;
            q *= 0.78 * Exp(-Pow(q / 16.0, 0.25));
            prop.afo = Min(15, 2.171 * Log(1 + 4.77e-4 * prop.Transmitter.hg * prop.Receiver.hg * prop.wn * q));
            prop.qk = 1 / Complex.Abs(prop.zgnd);
            prop.aht = 20;
            prop.xht = 0;
            foreach (var antenna in prop.Antennae)
            {
                var a = 0.5 * Pow(antenna.dl, 2) / antenna.he;
                var wa = Pow(a * prop.wn, THIRD);
                var pk = prop.qk / wa;
                q = (1.607 - pk) * 151 * wa * antenna.dl / a;
                prop.xht += q;
                prop.aht += fht(q, pk);
            }
        }

        /// <summary>
        /// Finds the "diffraction attenuation" at the distance <paramref name="d"/>
        /// </summary>
        /// <remarks>
        /// It uses a convex combination of smooth earth diffraction and double knife-edge diffraction
        /// </remarks>
        /// <param name="d"></param>
        /// <param name="prop"></param>
        /// <returns></returns>
        // ReSharper disable once InconsistentNaming
        private double adiff(double d, prop_type prop)
        {
            var th = prop.tha + d * prop.gme;
            var ds = d - prop.dla;
            var q = 0.0795775 * prop.wn * ds * Pow(th, 2);
            var adiffv = aknfe(q * prop.Transmitter.dl / (ds + prop.Transmitter.dl)) + aknfe(q * prop.Receiver.dl / (ds + prop.Receiver.dl));
            var a = ds / th;
            var wa = Pow(a * prop.wn, THIRD);
            var pk = prop.qk / wa;
            q = (1.607 - pk) * 151 * wa * th + prop.xht;
            var ar = 0.05751 * q - 4.343 * Log(q) - prop.aht;
            q = (prop.wd1 + prop.xd1 / d) * Min((1 - 0.8 * Exp(-d / 50000.0)) * prop.dh * prop.wn, 6283.2);
            var wd = 25.1 / (25.1 + Sqrt(q));
            return ar * wd + (1 - wd) * adiffv + prop.afo;
        }

        private void init_ascat(prop_type prop)
        {
            prop.ad = prop.Transmitter.dl - prop.Receiver.dl;
            prop.rr = prop.Receiver.he / prop.Transmitter.he;
            if (prop.ad < 0)
            {
                prop.ad = -prop.ad;
                prop.rr = 1 / prop.rr;
            }
            prop.etq = (5.67e-6 * prop.ens - 2.32e-3) * prop.ens + 0.031;
            prop.h0s = -15;
        }

        /// <summary>
        /// Finds the "scatter attenuation" at the distance <paramref name="d"/>. It uses an approximation to the methods of NBS TN101 with checks for inadmissable situations
        /// </summary>
        /// <param name="d"></param>
        /// <param name="prop"></param>
        /// <returns></returns>
        // ReSharper disable once InconsistentNaming
        private double ascat(double d, prop_type prop)
        {
            double th;
            double h0;
            if (prop.h0s > 15)
                h0 = prop.h0s;
            else
            {
                th = prop.Transmitter.the + prop.Receiver.the + d * prop.gme;
                var r2 = 2 * prop.wn * th;
                var r1 = r2 * prop.Transmitter.he;
                r2 *= prop.Receiver.he;
                if (r1 < 0.2 && r2 < 0.2)
                    return 1001;  // <==== early return
                var ss = (d - prop.ad) / (d + prop.ad);
                var q = prop.rr / ss;
                ss = Max(0.1, ss);
                q = Min(Max(0.1, q), 10);
                var z0 = (d - prop.ad) * (d + prop.ad) * th * 0.25 / d;
                var et = (prop.etq * Exp(-Pow(Min(1.7, z0 / 8000.0), 6)) + 1) * z0 / 1.7556e3;
                var ett = Max(et, 1);
                h0 = (h0f(r1, ett) + h0f(r2, ett)) * 0.5;
                h0 += Min(h0, (1.38 - Log(ett)) * Log(ss) * Log(q) * 0.49);
                h0 = Dim(h0, 0);
                if (et < 1)
                    h0 = et * h0 + (1 - et) * 4.343 * Log(Pow((1 + 1.4142 / r1) * (1 + 1.4142 / r2), 2) * (r1 + r2) / (r1 + r2 + 2.8284));
                if (h0 > 15 && prop.h0s >= 0)
                    h0 = prop.h0s;
            }
            prop.h0s = h0;
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

        private static void init_alos(prop_type prop)
        {
            prop.wls = 0.021 / (0.021 + prop.wn * prop.dh / Max(10000, prop.dlsa));
        }

        /// <summary>
        /// Finds the "line-of-sight" attenuation at the distance <paramref name="d"/> It uses a convex combination of plane earth fields and diffracted fields
        /// </summary>
        /// <param name="d"></param>
        /// <param name="prop"></param>
        /// <returns></returns>
        // ReSharper disable once InconsistentNaming
        private double alos(double d, prop_type prop)
        {
            var q = (1 - 0.8 * Exp(-d / 50000.0)) * prop.dh;
            var s = 0.78 * q * Exp(-Pow(q / 16.0, 0.25));
            q = prop.Transmitter.he + prop.Receiver.he;
            var sps = q / Sqrt(d * d + q * q);
            var r = (sps - prop.zgnd) / (sps + prop.zgnd) * Exp(-Min(10, prop.wn * s * sps));
            q = abq_alos(r);
            if (q < 0.25 || q < sps)
                r = r * Sqrt(sps / q);
            var alosv = prop.emd * d + prop.aed;
            q = prop.wn * prop.Transmitter.he * prop.Receiver.he * 2 / d;
            if (q > 1.57)
                q = 3.14 - 2.4649 / q;
            return (-4.343 * Log(abq_alos(new Complex(Cos(q), -Sin(q)) + r)) - alosv) * prop.wls + alosv;
        }


        private static void qlra(RadioClimate klimx, VariabilityMode mdvarx, prop_type prop)
        {
            foreach (var antenna in prop.Antennae)
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
                        q *= Sin(0.3141593 * antenna.hg);
                    antenna.he = antenna.hg + (1 + q) * Exp(-Min(20, 2 * antenna.hg / Max(1e-3, prop.dh)));
                }
                q = Sqrt(2 * antenna.he / prop.gme);
                antenna.dl = q * Exp(-0.07 * Sqrt(prop.dh / Max(antenna.he, 5)));
                antenna.the = (0.65 * prop.dh * (q / antenna.dl - 1) - 2 * antenna.he) / q;
            }
            prop.mdp = ControlFlow.AreaBegin;
            prop.mdvar = mdvarx;
            prop.klim = klimx;
            prop.lvar = Changes.All;
        }

        /// <summary>
        /// The Longley-Rice propagation program. This is the basic program; it returns the reference attenuation
        /// </summary>
        /// <param name="d"></param>
        /// <param name="prop"></param>
        // ReSharper disable once InconsistentNaming
        private void lrprop(double d, prop_type prop)  // PaulM_lrprop
        {
            if (prop.mdp != ControlFlow.AreaContinue)
            {
                foreach (var antenna in prop.Antennae)
                    antenna.dls = Sqrt(2 * antenna.he / prop.gme);
                prop.dlsa = prop.Transmitter.dls + prop.Receiver.dls;
                prop.dla = prop.Transmitter.dl + prop.Receiver.dl;
                prop.tha = Max(prop.Transmitter.the + prop.Receiver.the, -prop.dla * prop.gme);
                prop.wlos = false;
                prop.wscat = false;
                if (prop.wn < 0.838 || prop.wn > 210)
                {
                    prop.kwx = Max(prop.kwx, 1);
                }
                foreach (var antenna in prop.Antennae)
                    if (antenna.hg < 1 || antenna.hg > 1000)
                    {
                        prop.kwx = Max(prop.kwx, 1);
                    }
                foreach (var antenna in prop.Antennae)
                    if (Abs(antenna.the) > 200e-3 || antenna.dl < 0.1 * antenna.dls ||
                        antenna.dl > 3 * antenna.dls)
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
                foreach (var antenna in prop.Antennae)
                    if (antenna.hg < 0.5 || antenna.hg > 3000)
                    {
                        prop.kwx = 4;
                    }
                prop.dmin = Abs(prop.Transmitter.he - prop.Receiver.he) / 200e-3;
                init_adiff(prop);
                prop.xae = Pow(prop.wn * Pow(prop.gme, 2), -THIRD);
                var d3 = Max(prop.dlsa, 1.3787 * prop.xae + prop.dla);
                var d4 = d3 + 2.7574 * prop.xae;
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
                if (prop.dist < prop.dmin)
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
                if (!prop.wlos)
                {
                    init_alos(prop);
                    var d2 = prop.dlsa;
                    var a2 = prop.aed + d2 * prop.emd;
                    var d0 = 1.908 * prop.wn * prop.Transmitter.he * prop.Receiver.he;
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
                    prop.wlos = true;
                }
                if (prop.dist > 0)
                    prop.aref = prop.ael + prop.ak1 * prop.dist + prop.ak2 * Log(prop.dist);
            }
            if (prop.dist <= 0 || prop.dist >= prop.dlsa)
            {
                if (!prop.wscat)
                {
                    init_ascat(prop);
                    var d5 = prop.dla + 200000;
                    var d6 = d5 + 200000;
                    var a6 = ascat(d6, prop);
                    var a5 = ascat(d5, prop);
                    if (a5 < 1000)
                    {
                        prop.ems = (a6 - a5) / 200000.0;
                        prop.dx = Max(prop.dlsa, Max(prop.dla + 0.3 * prop.xae * Log(47.7 * prop.wn), (a5 - prop.aed - prop.ems * d5) / (prop.emd - prop.ems)));
                        prop.aes = (prop.emd - prop.ems) * prop.dx + prop.aed;
                    }
                    else
                    {
                        prop.ems = prop.emd;
                        prop.aes = prop.aed;
                        prop.dx = 10000000;
                    }
                    prop.wscat = true;
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
                cv1 = bv1; cv2 = bv2; yv1 = xv1; yv2 = xv2; yv3 = xv3; csm1 = bsm1; csm2 = bsm2; ysm1 = xsm1; ysm2 = xsm2;
                ysm3 = xsm3; csp1 = bsp1; csp2 = bsp2; ysp1 = xsp1; ysp2 = xsp2; ysp3 = xsp3; csd1 = bsd1; zd = bzd1;
                cfm1 = bfm1; cfm2 = bfm2; cfm3 = bfm3; cfp1 = bfp1; cfp2 = bfp2; cfp3 = bfp3;
            }
            public readonly double cv1, cv2, yv1, yv2, yv3, csm1, csm2, ysm1, ysm2, ysm3, csp1, csp2, ysp1, ysp2, ysp3,
                csd1, zd, cfm1, cfm2, cfm3, cfp1, cfp2, cfp3;
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

        // ReSharper disable once InconsistentNaming
        private double avar(double zzt, double zzl, double zzc, prop_type prop)
        {
            const double rt = 7.8, rl = 24.0;
            double sgt, yr;

            if (prop.lvar > Changes.None)
            {
                double q;
                switch (prop.lvar)
                {
                    case Changes.All:
                        prop.cs = _climateSettings[prop.klim];
                        goto case Changes.VariabilityMode;
                    case Changes.VariabilityMode:
                        goto case Changes.Frequency;
                    case Changes.Frequency:
                        q = Log(0.133 * prop.wn);
                        prop.gm = prop.cs.cfm1 + prop.cs.cfm2 / (Pow(prop.cs.cfm3 * q, 2) + 1);
                        prop.gp = prop.cs.cfp1 + prop.cs.cfp2 / (Pow(prop.cs.cfp3 * q, 2) + 1);
                        goto case Changes.AntennaHeight;
                    case Changes.AntennaHeight:
                        prop.dexa = Sqrt(18000000 * prop.Transmitter.he) + Sqrt(18000000 * prop.Receiver.he) + Pow(575.7e12 / prop.wn, THIRD);
                        goto case Changes.Distance;
                    case Changes.Distance:
                        if (prop.dist < prop.dexa)
                            prop.de = 130000 * prop.dist / prop.dexa;
                        else
                            prop.de = 130000 + prop.dist - prop.dexa;
                        break;
                }
                prop.vmd = curve(prop.cs.cv1, prop.cs.cv2, prop.cs.yv1, prop.cs.yv2, prop.cs.yv3, prop.de);
                prop.sgtm = curve(prop.cs.csm1, prop.cs.csm2, prop.cs.ysm1, prop.cs.ysm2, prop.cs.ysm3, prop.de) * prop.gm;
                prop.sgtp = curve(prop.cs.csp1, prop.cs.csp2, prop.cs.ysp1, prop.cs.ysp2, prop.cs.ysp3, prop.de) * prop.gp;
                prop.sgtd = prop.sgtp * prop.cs.csd1;
                prop.tgtd = (prop.sgtp - prop.sgtd) * prop.cs.zd;
                if (prop.w1)
                    prop.sgl = 0;
                else
                {
                    q = (1 - 0.8 * Exp(-prop.dist / 50000.0)) * prop.dh * prop.wn;
                    prop.sgl = 10 * q / (q + 13);
                }
                prop.vs0 = Pow(5 + 3 * Exp(-prop.de / 100000.0), 2);
                prop.lvar = Changes.None;
            }
            var zt = zzt;
            var zl = zzl;
            var zc = zzc;
            switch (prop.mdvar)
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
                sgt = prop.sgtm;
            else if (zt <= prop.cs.zd)
                sgt = prop.sgtp;
            else
                sgt = prop.sgtd + prop.tgtd / zt;
            var vs = prop.vs0 + Pow(sgt * zt, 2) / (rt + zc * zc) + Pow(prop.sgl * zl, 2) / (rl + zc * zc);
            switch (prop.mdvar)
            {
                case VariabilityMode.Single:
                    yr = 0;
                    prop.sgc = Sqrt(sgt * sgt + prop.sgl * prop.sgl + vs);
                    break;
                case VariabilityMode.Individual:
                    yr = sgt * zt;
                    prop.sgc = Sqrt(prop.sgl * prop.sgl + vs);
                    break;
                case VariabilityMode.Mobile:
                    yr = Sqrt(sgt * sgt + prop.sgl * prop.sgl) * zt;
                    prop.sgc = Sqrt(vs);
                    break;
                default:
                    yr = sgt * zt + prop.sgl * zl;
                    prop.sgc = Sqrt(vs);
                    break;
            }
            var avarv = prop.aref - prop.vmd - yr - prop.sgc * zc;
            if (avarv < 0)
                avarv = avarv * (29 - avarv) / (29 - 10 * avarv);
            return avarv;

        }

        // ReSharper disable once InconsistentNaming
        private static void hzns(Elevations pfl, prop_type prop)
        {
            var np = pfl.EndIndex;
            var xi = pfl.DeltaDistance;
            var za = pfl.FirstPoint + prop.Transmitter.hg;
            var zb = pfl.LastPoint + prop.Receiver.hg;
            var qc = 0.5 * prop.gme;
            var q = qc * prop.dist;
            prop.Receiver.the = (zb - za) / prop.dist;
            prop.Transmitter.the = prop.Receiver.the - q;
            prop.Receiver.the = -prop.Receiver.the - q;
            prop.Transmitter.dl = prop.dist;
            prop.Receiver.dl = prop.dist;
            if (np >= 2)
            {
                var sa = 0.0;
                var sb = prop.dist;
                var wq = true;
                for (var i = 1; i < np; i++)
                {
                    sa += xi;
                    sb -= xi;
                    q = pfl.Points[i] - (qc * sa + prop.Transmitter.the) * sa - za;
                    if (q > 0)
                    {
                        prop.Transmitter.the += q / sa;
                        prop.Transmitter.dl = sa;
                        wq = false;
                    }
                    if (!wq)
                    {
                        q = pfl.Points[i] - (qc * sb + prop.Receiver.the) * sb - zb;
                        if (q > 0)
                        {
                            prop.Receiver.the += q / sb;
                            prop.Receiver.dl = sb;
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
            prop.dist = pfl.EndIndex * pfl.DeltaDistance;
            hzns(pfl, prop);
            foreach (var antenna in prop.Antennae)
                antenna.xl = Min(15 * antenna.hg, 0.1 * antenna.dl);
            prop.Receiver.xl = prop.dist - prop.Receiver.xl;
            prop.dh = d1thx(pfl, prop.Transmitter.xl, prop.Receiver.xl);
            if (prop.Transmitter.dl + prop.Receiver.dl > 1.5 * prop.dist)
            {
                z1sq1(pfl, prop.Transmitter.xl, prop.Receiver.xl, out var za, out var zb);
                prop.Transmitter.he = prop.Transmitter.hg + Dim(pfl.FirstPoint, za);
                prop.Receiver.he = prop.Receiver.hg + Dim(pfl.LastPoint, zb);
                foreach (var antenna in prop.Antennae)
                    antenna.dl = Sqrt(2 * antenna.he / prop.gme) *
                                Exp(-0.07 * Sqrt(prop.dh / Max(antenna.he, 5)));
                var q = prop.Transmitter.dl + prop.Receiver.dl;

                if (q <= prop.dist)
                {
                    q = Pow(prop.dist / q, 2);
                    foreach (var antenna in prop.Antennae)
                    {
                        antenna.he *= q;
                        antenna.dl = Sqrt(2 * antenna.he / prop.gme) *
                              Exp(-0.07 * Sqrt(prop.dh / Max(antenna.he, 5)));
                    }
                }
                foreach (var antenna in prop.Antennae)
                {
                    q = Sqrt(2 * antenna.he / prop.gme);
                    antenna.the = (0.65 * prop.dh * (q / antenna.dl - 1) - 2 * antenna.he) / q;
                }
            }
            else
            {
                z1sq1(pfl, prop.Transmitter.xl, 0.9 * prop.Transmitter.dl, out var za, out _);
                z1sq1(pfl, prop.dist - 0.9 * prop.Receiver.dl, prop.Receiver.xl, out _, out var zb);
                prop.Transmitter.he = prop.Transmitter.hg + Dim(pfl.FirstPoint, za);
                prop.Receiver.he = prop.Receiver.hg + Dim(pfl.LastPoint, zb);
            }
            prop.mdp = ControlFlow.PointToPoint;
            prop.mdvar = mdvarx;
            prop.klim = klimx;
            prop.lvar = Changes.All;
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
            prop.Transmitter.hg = tht_m;
            prop.Receiver.hg = rht_m;
            prop.klim = radio_climate;
            prop.lvar = Changes.All;
            prop.mdp = ControlFlow.PointToPoint;
            prop.w1 = true; // possible bugfix: original embedded this value in mdvar
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

            if (propmode != PropMode.LineOfSight)
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
            prop.Transmitter.hg = tht_m;
            prop.Receiver.hg = rht_m;
            prop.klim = radio_climate;
            prop.ens = eno_ns_surfref;
            prop.Transmitter.kst = TSiteCriteria;
            prop.Receiver.kst = RSiteCriteria;
            qlrps(frq_mhz, 0, eno_ns_surfref, pol, eps_dielect, sgm_conductivity, prop);
            qlra(prop.klim, ModVar, prop);
            if (prop.lvar < Changes.Distance)
                prop.lvar = Changes.Distance;
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
