using System;
using System.Collections.Generic;
using System.Linq;
using System.Numerics;
using static System.Math;
#pragma warning disable IDE1006

namespace LongleyRice
{
    class RefactoredTesting
    {
        // *************************************
        // C++ routines for this program are taken from
        // a translation of the FORTRAN code written by
        // U.S. Department of Commerce NTIA/ITS
        // Institute for Telecommunication Sciences
        // *****************
        // Irregular Terrain Model (ITM) (Longley-Rice)
        // *************************************
        private const double _third = (1.0 / 3.0);
        private double _aref;
        private double _dist;
        private readonly double[] _hg = new double[2];
        private double _wn;
        private double _dh;
        private double _ens;
        private double _gme;
        private double _zgndreal;
        private double _zgndimag;
        private readonly double[] _he = new double[2];
        private readonly double[] _dl = new double[2];
        private readonly double[] _the = new double[2];
        private int _kwx;
        private int _mdp;
        private double _sgc;
        private int _lvar;
        private int _mdvar;
        private int _klim;
        private double _dlsa;
        private double _dx;
        private double _ael;
        private double _ak1;
        private double _ak2;
        private double _aed;
        private double _emd;
        private double _aes;
        private double _ems;
        private readonly double[] _dls = new double[2];
        private double _dla;
        private double _tha;

        int mymin(int i, int j)
        {
            if (i < j)
                return i;
            else
                return j;
        }

        int mymax(int i, int j)
        {
            if (i > j)
                return i;
            else
                return j;
        }

        double mymin(double a, double b)
        {
            if (a < b)
                return a;
            else
                return b;
        }

        double mymax(double a, double b)
        {
            if (a > b)
                return a;
            else
                return b;
        }

        double FORTRAN_DIM(double x, double y)
        { // This performs the FORTRAN DIM function.
          // result is x-y if x is greater than y; otherwise result is 0.0
            if (x > y)
                return x - y;
            else
                return 0.0;
        }

        double aknfe(double v2)
        {
            double a;
            if (v2 < 5.76)
                a = 6.02 + 9.11 * Sqrt(v2) - 1.27 * v2;
            else
                a = 12.953 + 4.343 * Log(v2);
            return a;
        }

        double fht(double x, double pk)
        {
            double w, fhtv;
            if (x < 200.0)
            {
                w = -Log(pk);
                if (pk < 1e-5 || x * Pow(w, 3.0) > 5495.0)
                {
                    fhtv = -117.0;
                    if (x > 1.0)
                        fhtv = 17.372 * Log(x) + fhtv;
                }
                else
                    fhtv = 2.5e-5 * x * x / pk - 8.686 * w - 15.0;
            }
            else
            {
                fhtv = 0.05751 * x - 4.343 * Log(x);
                if (x < 2000.0)
                {
                    w = 0.0134 * x * Exp(-0.005 * x);
                    fhtv = (1.0 - w) * fhtv + w * (17.372 * Log(x) - 117.0);
                }
            }
            return fhtv;
        }

        private static readonly double[] _h0f_a = { 25.0, 80.0, 177.0, 395.0, 705.0 };
        private static readonly double[] _h0f_b = { 24.0, 45.0, 68.0, 80.0, 105.0 };

        double h0f(double r, double et)
        {
            double q;
            var it = (int)et;
            if (it <= 0)
            {
                it = 1;
                q = 0.0;
            }
            else if (it >= 5)
            {
                it = 5;
                q = 0.0;
            }
            else
                q = et - it;
            var x = Pow(1.0 / r, 2.0);
            var h0fv = 4.343 * Log((_h0f_a[it - 1] * x + _h0f_b[it - 1]) * x + 1.0);
            if (q != 0.0)
                h0fv = (1.0 - q) * h0fv + q * 4.343 * Log((_h0f_a[it] * x + _h0f_b[it]) * x + 1.0);
            return h0fv;
        }

        private static readonly double[] _ahd_a = { 133.4, 104.6, 71.8 };
        private static readonly double[] _ahd_b = { 0.332e-3, 0.212e-3, 0.157e-3 };
        private static readonly double[] _ahd_c = { -4.343, -1.086, 2.171 };

        double ahd(double td)
        {
            int i;
            if (td <= 10e3)
                i = 0;
            else if (td <= 70e3)
                i = 1;
            else
                i = 2;
            return _ahd_a[i] + _ahd_b[i] * td + _ahd_c[i] * Log(td);
        }

        double _wd1, _xd1, _afo, _qk, _aht, _xht;
        double adiff(double d)
        {
            Complex prop_zgnd = new Complex(_zgndreal, _zgndimag);
            double a, q, pk, wa, adiffv;
            if (d == 0)
            {
                q = _hg[0] * _hg[1];
                _qk = _he[0] * _he[1] - q;
                if (_mdp < 0.0)
                    q += 10.0;
                _wd1 = Sqrt(1.0 + _qk / q);
                _xd1 = _dla + _tha / _gme;
                q = (1.0 - 0.8 * Exp(-_dlsa / 50e3)) * _dh;
                q *= 0.78 * Exp(-Pow(q / 16.0, 0.25));
                _afo = mymin(15.0, 2.171 * Log(1.0 + 4.77e-4 * _hg[0] * _hg[1] *
                        _wn * q));
                _qk = 1.0 / Complex.Abs(prop_zgnd);
                _aht = 20.0;
                _xht = 0.0;
                for (int j = 0; j < 2; ++j)
                {
                    a = 0.5 * Pow(_dl[j], 2.0) / _he[j];
                    wa = Pow(a * _wn, _third);
                    pk = _qk / wa;
                    q = (1.607 - pk) * 151.0 * wa * _dl[j] / a;
                    _xht += q;
                    _aht += fht(q, pk);
                }
                adiffv = 0.0;
            }
            else
            {
                var th = _tha + d * _gme;
                var ds = d - _dla;
                q = 0.0795775 * _wn * ds * Pow(th, 2.0);
                adiffv = aknfe(q * _dl[0] / (ds + _dl[0])) + aknfe(q * _dl[1] / (ds + _dl[1]));
                a = ds / th;
                wa = Pow(a * _wn, _third);
                pk = _qk / wa;
                q = (1.607 - pk) * 151.0 * wa * th + _xht;
                var ar = 0.05751 * q - 4.343 * Log(q) - _aht;
                q = (_wd1 + _xd1 / d) * mymin(((1.0 - 0.8 * Exp(-d / 50e3)) * _dh * _wn), 6283.2);
                var wd = 25.1 / (25.1 + Sqrt(q));
                adiffv = ar * wd + (1.0 - wd) * adiffv + _afo;
            }
            return adiffv;
        }

        double _ad, _rr, _etq, _h0s;
        double ascat(double d)
        {
            double ascatv;
            if (d == 0.0)
            {
                _ad = _dl[0] - _dl[1];
                _rr = _he[1] / _he[0];
                if (_ad < 0.0)
                {
                    _ad = -_ad;
                    _rr = 1.0 / _rr;
                }
                _etq = (5.67e-6 * _ens - 2.32e-3) * _ens + 0.031;
                _h0s = -15.0;
                ascatv = 0.0;
            }
            else
            {
                double th, h0;
                if (_h0s > 15.0)
                    h0 = _h0s;
                else
                {
                    th = _the[0] + _the[1] + d * _gme;
                    var r2 = 2.0 * _wn * th;
                    var r1 = r2 * _he[0];
                    r2 *= _he[1];
                    if (r1 < 0.2 && r2 < 0.2)
                        return 1001.0;  // <==== early return
                    var ss = (d - _ad) / (d + _ad);
                    var q = _rr / ss;
                    ss = mymax(0.1, ss);
                    q = mymin(mymax(0.1, q), 10.0);
                    var z0 = (d - _ad) * (d + _ad) * th * 0.25 / d;
                    var et = (_etq * Exp(-Pow(mymin(1.7, z0 / 8.0e3), 6.0)) + 1.0) * z0 / 1.7556e3;
                    var ett = mymax(et, 1.0);
                    h0 = (h0f(r1, ett) + h0f(r2, ett)) * 0.5;
                    h0 += mymin(h0, (1.38 - Log(ett)) * Log(ss) * Log(q) * 0.49);
                    h0 = FORTRAN_DIM(h0, 0.0);
                    if (et < 1.0)
                        h0 = et * h0 + (1.0 - et) * 4.343 * Log(Pow((1.0 + 1.4142 / r1) *
                           (1.0 + 1.4142 / r2), 2.0) * (r1 + r2) / (r1 + r2 + 2.8284));
                    if (h0 > 15.0 && _h0s >= 0.0)
                        h0 = _h0s;
                }
                _h0s = h0;
                th = _tha + d * _gme;
                ascatv = ahd(th * d) + 4.343 * Log(47.7 * _wn * Pow(th, 4.0)) - 0.1 *
                       (_ens - 301.0) * Exp(-th * d / 40e3) + h0;
            }
            return ascatv;
        }

        double qerfi(double q)
        {
            const double c0 = 2.515516698;
            const double c1 = 0.802853;
            const double c2 = 0.010328;
            const double d1 = 1.432788;
            const double d2 = 0.189269;
            const double d3 = 0.001308;

            var x = 0.5 - q;
            var t = mymax(0.5 - Abs(x), 0.000001);
            t = Sqrt(-2.0 * Log(t));
            var v = t - ((c2 * t + c1) * t + c0) / (((d3 * t + d2) * t + d1) * t + 1.0);
            if (x < 0.0) v = -v;
            return v;
        }

        void qlrps(double fmhz, double zsys, double en0,
                  int ipol, double eps, double sgm)
        {
            const double gma = 157e-9;
            _wn = fmhz / 47.7;
            _ens = en0;
            if (zsys != 0.0)
                _ens *= Exp(-zsys / 9460.0);
            _gme = gma * (1.0 - 0.04665 * Exp(_ens / 179.3));
            var prop_zgnd = new Complex(_zgndreal, _zgndimag);
            var zq = new Complex(eps, 376.62 * sgm / _wn);
            prop_zgnd = Complex.Sqrt(zq - 1.0);
            if (ipol != 0.0)
                prop_zgnd = prop_zgnd / zq;

            _zgndreal = prop_zgnd.real(); _zgndimag = prop_zgnd.imag();
        }

        double abq_alos(Complex r)
        { return r.real() * r.real() + r.imag() * r.imag(); }

        double _wls;
        double alos(double d)
        {
            Complex prop_zgnd = new Complex(_zgndreal, _zgndimag);
            double alosv;
            if (d == 0.0)
            {
                _wls = 0.021 / (0.021 + _wn * _dh / mymax(10e3, _dlsa));
                alosv = 0.0;
            }
            else
            {
                var q = (1.0 - 0.8 * Exp(-d / 50e3)) * _dh;
                var s = 0.78 * q * Exp(-Pow(q / 16.0, 0.25));
                q = _he[0] + _he[1];
                var sps = q / Sqrt(d * d + q * q);
                var r = (sps - prop_zgnd) / (sps + prop_zgnd) * Exp(-mymin(10.0, _wn * s * sps));
                q = abq_alos(r);
                if (q < 0.25 || q < sps)
                    r = r * Sqrt(sps / q);
                alosv = _emd * d + _aed;
                q = _wn * _he[0] * _he[1] * 2.0 / d;
                if (q > 1.57)
                    q = 3.14 - 2.4649 / q;
                alosv = (-4.343 * Log(abq_alos(new Complex(Cos(q), -Sin(q)) + r)) - alosv) *
                         _wls + alosv;
            }
            return alosv;
        }


        void qlra(int[] kst, int klimx, int mdvarx)
        {
            for (var j = 0; j < 2; ++j)
            {
                double q;
                if (kst[j] <= 0)
                    _he[j] = _hg[j];
                else
                {
                    q = 4.0;
                    if (kst[j] != 1)
                        q = 9.0;
                    if (_hg[j] < 5.0)
                        q *= Sin(0.3141593 * _hg[j]);
                    _he[j] = _hg[j] + (1.0 + q) * Exp(-mymin(20.0, 2.0 * _hg[j] / mymax(1e-3, _dh)));
                }
                q = Sqrt(2.0 * _he[j] / _gme);
                _dl[j] = q * Exp(-0.07 * Sqrt(_dh / mymax(_he[j], 5.0)));
                _the[j] = (0.65 * _dh * (q / _dl[j] - 1.0) - 2.0 * _he[j]) / q;
            }
            _mdp = 1;
            _lvar = mymax(_lvar, 3);
            if (mdvarx >= 0)
            {
                _mdvar = mdvarx;
                _lvar = mymax(_lvar, 4);
            }
            if (klimx > 0)
            {
                _klim = klimx;
                _lvar = 5;
            }
        }

        bool _wlos, _wscat;
        double _dmin, _xae;
        void lrprop(double d)  // PaulM_lrprop
        {
            var prop_zgnd = new Complex(_zgndreal, _zgndimag);
            double q;
            int j;

            if (_mdp != 0)
            {
                for (j = 0; j < 2; j++)
                    _dls[j] = Sqrt(2.0 * _he[j] / _gme);
                _dlsa = _dls[0] + _dls[1];
                _dla = _dl[0] + _dl[1];
                _tha = mymax(_the[0] + _the[1], -_dla * _gme);
                _wlos = false;
                _wscat = false;
                if (_wn < 0.838 || _wn > 210.0)
                {
                    _kwx = mymax(_kwx, 1);
                }
                for (j = 0; j < 2; j++)
                    if (_hg[j] < 1.0 || _hg[j] > 1000.0)
                    {
                        _kwx = mymax(_kwx, 1);
                    }
                for (j = 0; j < 2; j++)
                    if (Abs(_the[j]) > 200e-3 || _dl[j] < 0.1 * _dls[j] ||
                       _dl[j] > 3.0 * _dls[j])
                    {
                        _kwx = mymax(_kwx, 3);
                    }
                if (_ens < 250.0 || _ens > 400.0 ||
                    _gme < 75e-9 || _gme > 250e-9 ||
                    prop_zgnd.real() <= Abs(prop_zgnd.imag()) ||
                    _wn < 0.419 || _wn > 420.0)
                {
                    _kwx = 4;
                }
                for (j = 0; j < 2; j++)
                    if (_hg[j] < 0.5 || _hg[j] > 3000.0)
                    {
                        _kwx = 4;
                    }
                _dmin = Abs(_he[0] - _he[1]) / 200e-3;
                q = adiff(0.0);
                _xae = Pow(_wn * Pow(_gme, 2), -_third);
                var d3 = mymax(_dlsa, 1.3787 * _xae + _dla);
                var d4 = d3 + 2.7574 * _xae;
                var a3 = adiff(d3);
                var a4 = adiff(d4);
                _emd = (a4 - a3) / (d4 - d3);
                _aed = a3 - _emd * d3;
            }
            if (_mdp >= 0)
            {
                _mdp = 0;
                _dist = d;
            }
            if (_dist > 0.0)
            {
                if (_dist > 1000e3)
                {
                    _kwx = mymax(_kwx, 1);
                }
                if (_dist < _dmin)
                {
                    _kwx = mymax(_kwx, 3);
                }
                if (_dist < 1e3 || _dist > 2000e3)
                {
                    _kwx = 4;
                }
            }
            if (_dist < _dlsa)
            {
                if (!_wlos)
                {
                    q = alos(0.0);
                    var d2 = _dlsa;
                    var a2 = _aed + d2 * _emd;
                    var d0 = 1.908 * _wn * _he[0] * _he[1];
                    double d1;
                    if (_aed >= 0.0)
                    {
                        d0 = mymin(d0, 0.5 * _dla);
                        d1 = d0 + 0.25 * (_dla - d0);
                    }
                    else
                        d1 = mymax(-_aed / _emd, 0.25 * _dla);
                    var a1 = alos(d1);
                    var wq = false;
                    if (d0 < d1)
                    {
                        var a0 = alos(d0);
                        q = Log(d2 / d0);
                        _ak2 = mymax(0.0, ((d2 - d0) * (a1 - a0) - (d1 - d0) * (a2 - a0)) /
                                       ((d2 - d0) * Log(d1 / d0) - (d1 - d0) * q));
                        wq = _aed >= 0.0 || _ak2 > 0.0;
                        if (wq)
                        {
                            _ak1 = (a2 - a0 - _ak2 * q) / (d2 - d0);
                            if (_ak1 < 0.0)
                            {
                                _ak1 = 0.0;
                                _ak2 = FORTRAN_DIM(a2, a0) / q;
                                if (_ak2 == 0.0) _ak1 = _emd;
                            }
                        }
                    }
                    if (!wq)
                    {
                        _ak1 = FORTRAN_DIM(a2, a1) / (d2 - d1);
                        _ak2 = 0.0;
                        if (_ak1 == 0.0) _ak1 = _emd;
                    }
                    _ael = a2 - _ak1 * d2 - _ak2 * Log(d2);
                    _wlos = true;
                }
                if (_dist > 0.0)
                    _aref = _ael + _ak1 * _dist +
                               _ak2 * Log(_dist);
            }
            if (_dist <= 0.0 || _dist >= _dlsa)
            {
                if (!_wscat)
                {
                    q = ascat(0.0);
                    var d5 = _dla + 200e3;
                    var d6 = d5 + 200e3;
                    var a6 = ascat(d6);
                    var a5 = ascat(d5);
                    if (a5 < 1000.0)
                    {
                        _ems = (a6 - a5) / 200e3;
                        _dx = mymax(_dlsa, mymax(_dla + 0.3 * _xae *
                               Log(47.7 * _wn), (a5 - _aed - _ems * d5) /
                               (_emd - _ems)));
                        _aes = (_emd - _ems) * _dx + _aed;
                    }
                    else
                    {
                        _ems = _emd;
                        _aes = _aed;
                        _dx = 10.0e6;
                    }
                    _wscat = true;
                }
                if (_dist > _dx)
                    _aref = _aes + _ems * _dist;
                else
                    _aref = _aed + _emd * _dist;
            }
            _aref = mymax(_aref, 0.0);
        }

        double curve(double c1, double c2, double x1,
              double x2, double x3, double de)
        {
            return (c1 + c2 / (1.0 + Pow((de - x2) / x3, 2.0))) * Pow(de / x1, 2.0) /
                   (1.0 + Pow(de / x1, 2.0));
        }

        int _kdv;
        double _dexa, _de, _vmd, _vs0, _sgl, _sgtm, _sgtp, _sgtd, _tgtd,
            _gm, _gp, _cv1, _cv2, _yv1, _yv2, _yv3, _csm1, _csm2, _ysm1, _ysm2,
            _ysm3, _csp1, _csp2, _ysp1, _ysp2, _ysp3, _csd1, _zd, _cfm1, _cfm2,
            _cfm3, _cfp1, _cfp2, _cfp3;
        bool _ws, _w1;
        private static readonly double[] _avar_bv1 = { -9.67, -0.62, 1.26, -9.21, -0.62, -0.39, 3.15 };
        private static readonly double[] _avar_bv2 = { 12.7, 9.19, 15.5, 9.05, 9.19, 2.86, 857.9 };
        private static readonly double[] _avar_xv1 = { 144.9e3, 228.9e3, 262.6e3, 84.1e3, 228.9e3, 141.7e3, 2222.0e3 };
        private static readonly double[] _avar_xv2 = { 190.3e3, 205.2e3, 185.2e3, 101.1e3, 205.2e3, 315.9e3, 164.8e3 };
        private static readonly double[] _avar_xv3 = { 133.8e3, 143.6e3, 99.8e3, 98.6e3, 143.6e3, 167.4e3, 116.3e3 };
        private static readonly double[] _avar_bsm1 = { 2.13, 2.66, 6.11, 1.98, 2.68, 6.86, 8.51 };
        private static readonly double[] _avar_bsm2 = { 159.5, 7.67, 6.65, 13.11, 7.16, 10.38, 169.8 };
        private static readonly double[] _avar_xsm1 = { 762.2e3, 100.4e3, 138.2e3, 139.1e3, 93.7e3, 187.8e3, 609.8e3 };
        private static readonly double[] _avar_xsm2 = { 123.6e3, 172.5e3, 242.2e3, 132.7e3, 186.8e3, 169.6e3, 119.9e3 };
        private static readonly double[] _avar_xsm3 = { 94.5e3, 136.4e3, 178.6e3, 193.5e3, 133.5e3, 108.9e3, 106.6e3 };
        private static readonly double[] _avar_bsp1 = { 2.11, 6.87, 10.08, 3.68, 4.75, 8.58, 8.43 };
        private static readonly double[] _avar_bsp2 = { 102.3, 15.53, 9.60, 159.3, 8.12, 13.97, 8.19 };
        private static readonly double[] _avar_xsp1 = { 636.9e3, 138.7e3, 165.3e3, 464.4e3, 93.2e3, 216.0e3, 136.2e3 };
        private static readonly double[] _avar_xsp2 = { 134.8e3, 143.7e3, 225.7e3, 93.1e3, 135.9e3, 152.0e3, 188.5e3 };
        private static readonly double[] _avar_xsp3 = { 95.6e3, 98.6e3, 129.7e3, 94.2e3, 113.4e3, 122.7e3, 122.9e3 };
        private static readonly double[] _avar_bsd1 = { 1.224, 0.801, 1.380, 1.000, 1.224, 1.518, 1.518 };
        private static readonly double[] _avar_bzd1 = { 1.282, 2.161, 1.282, 20.0, 1.282, 1.282, 1.282 };
        private static readonly double[] _avar_bfm1 = { 1.0, 1.0, 1.0, 1.0, 0.92, 1.0, 1.0 };
        private static readonly double[] _avar_bfm2 = { 0.0, 0.0, 0.0, 0.0, 0.25, 0.0, 0.0 };
        private static readonly double[] _avar_bfm3 = { 0.0, 0.0, 0.0, 0.0, 1.77, 0.0, 0.0 };
        private static readonly double[] _avar_bfp1 = { 1.0, 0.93, 1.0, 0.93, 0.93, 1.0, 1.0 };
        private static readonly double[] _avar_bfp2 = { 0.0, 0.31, 0.0, 0.19, 0.31, 0.0, 0.0 };
        private static readonly double[] _avar_bfp3 = { 0.0, 2.00, 0.0, 1.79, 2.00, 0.0, 0.0 };
        double avar(double zzt, double zzl, double zzc)
        {
            const double rt = 7.8, rl = 24.0;
            double sgt, yr;
            int temp_klim = _klim - 1;

            if (_lvar > 0)
            {
                double q;
                switch (_lvar)
                {
                    default:
                        if (_klim <= 0 || _klim > 7)
                        {
                            _klim = 5;
                            temp_klim = 4;
                            {
                                _kwx = mymax(_kwx, 2);
                            }
                        }
                        _cv1 = _avar_bv1[temp_klim];
                        _cv2 = _avar_bv2[temp_klim];
                        _yv1 = _avar_xv1[temp_klim];
                        _yv2 = _avar_xv2[temp_klim];
                        _yv3 = _avar_xv3[temp_klim];
                        _csm1 = _avar_bsm1[temp_klim];
                        _csm2 = _avar_bsm2[temp_klim];
                        _ysm1 = _avar_xsm1[temp_klim];
                        _ysm2 = _avar_xsm2[temp_klim];
                        _ysm3 = _avar_xsm3[temp_klim];
                        _csp1 = _avar_bsp1[temp_klim];
                        _csp2 = _avar_bsp2[temp_klim];
                        _ysp1 = _avar_xsp1[temp_klim];
                        _ysp2 = _avar_xsp2[temp_klim];
                        _ysp3 = _avar_xsp3[temp_klim];
                        _csd1 = _avar_bsd1[temp_klim];
                        _zd = _avar_bzd1[temp_klim];
                        _cfm1 = _avar_bfm1[temp_klim];
                        _cfm2 = _avar_bfm2[temp_klim];
                        _cfm3 = _avar_bfm3[temp_klim];
                        _cfp1 = _avar_bfp1[temp_klim];
                        _cfp2 = _avar_bfp2[temp_klim];
                        _cfp3 = _avar_bfp3[temp_klim];
                        goto case 4;
                    case 4:
                        _kdv = _mdvar;
                        _ws = _kdv >= 20;
                        if (_ws)
                            _kdv -= 20;
                        _w1 = _kdv >= 10;
                        if (_w1)
                            _kdv -= 10;
                        if (_kdv < 0 || _kdv > 3)
                        {
                            _kdv = 0;
                            _kwx = mymax(_kwx, 2);
                        }

                        goto case 3;
                    case 3:
                        q = Log(0.133 * _wn);
                        _gm = _cfm1 + _cfm2 / (Pow(_cfm3 * q, 2.0) + 1.0);
                        _gp = _cfp1 + _cfp2 / (Pow(_cfp3 * q, 2.0) + 1.0);
                        goto case 2;
                    case 2:
                        _dexa = Sqrt(18e6 * _he[0]) + Sqrt(18e6 * _he[1]) +
                             Pow((575.7e12 / _wn), _third);
                        goto case 1;
                    case 1:
                        if (_dist < _dexa)
                            _de = 130e3 * _dist / _dexa;
                        else
                            _de = 130e3 + _dist - _dexa;
                        break;
                }
                _vmd = curve(_cv1, _cv2, _yv1, _yv2, _yv3, _de);
                _sgtm = curve(_csm1, _csm2, _ysm1, _ysm2, _ysm3, _de) * _gm;
                _sgtp = curve(_csp1, _csp2, _ysp1, _ysp2, _ysp3, _de) * _gp;
                _sgtd = _sgtp * _csd1;
                _tgtd = (_sgtp - _sgtd) * _zd;
                if (_w1)
                    _sgl = 0.0;
                else
                {
                    q = (1.0 - 0.8 * Exp(-_dist / 50e3)) * _dh * _wn;
                    _sgl = 10.0 * q / (q + 13.0);
                }
                if (_ws)
                    _vs0 = 0.0;
                else
                    _vs0 = Pow(5.0 + 3.0 * Exp(-_de / 100e3), 2.0);
                _lvar = 0;
            }
            var zt = zzt;
            var zl = zzl;
            var zc = zzc;
            switch (_kdv)
            {
                case 0:
                    zt = zc;
                    zl = zc;
                    break;
                case 1:
                    zl = zc;
                    break;
                case 2:
                    zl = zt;
                    break;
            }
            if (Abs(zt) > 3.1 || Abs(zl) > 3.1 || Abs(zc) > 3.1)
            {
                _kwx = mymax(_kwx, 1);
            }
            if (zt < 0.0)
                sgt = _sgtm;
            else if (zt <= _zd)
                sgt = _sgtp;
            else
                sgt = _sgtd + _tgtd / zt;
            var vs = _vs0 + Pow(sgt * zt, 2.0) / (rt + zc * zc) + Pow(_sgl * zl, 2.0) / (rl + zc * zc);
            if (_kdv == 0)
            {
                yr = 0.0;
                _sgc = Sqrt(sgt * sgt + _sgl * _sgl + vs);
            }
            else if (_kdv == 1)
            {
                yr = sgt * zt;
                _sgc = Sqrt(_sgl * _sgl + vs);
            }
            else if (_kdv == 2)
            {
                yr = Sqrt(sgt * sgt + _sgl * _sgl) * zt;
                _sgc = Sqrt(vs);
            }
            else
            {
                yr = sgt * zt + _sgl * zl;
                _sgc = Sqrt(vs);
            }
            var avarv = _aref - _vmd - yr - _sgc * zc;
            if (avarv < 0.0)
                avarv = avarv * (29.0 - avarv) / (29.0 - 10.0 * avarv);
            return avarv;

        }

        void hzns(double[] pfl)
        {
            var np = (int)pfl[0];
            var xi = pfl[1];
            var za = pfl[2] + _hg[0];
            var zb = pfl[np + 2] + _hg[1];
            var qc = 0.5 * _gme;
            var q = qc * _dist;
            _the[1] = (zb - za) / _dist;
            _the[0] = _the[1] - q;
            _the[1] = -_the[1] - q;
            _dl[0] = _dist;
            _dl[1] = _dist;
            if (np >= 2)
            {
                var sa = 0.0;
                var sb = _dist;
                var wq = true;
                for (var i = 1; i < np; i++)
                {
                    sa += xi;
                    sb -= xi;
                    q = pfl[i + 2] - (qc * sa + _the[0]) * sa - za;
                    if (q > 0.0)
                    {
                        _the[0] += q / sa;
                        _dl[0] = sa;
                        wq = false;
                    }
                    if (!wq)
                    {
                        q = pfl[i + 2] - (qc * sb + _the[1]) * sb - zb;
                        if (q > 0.0)
                        {
                            _the[1] += q / sb;
                            _dl[1] = sb;
                        }
                    }
                }
            }
        }

        void z1sq1(double[] z, double x1, double x2,
                out double z0, out double zn)
        {
            var xn = z[0];
            double xa = (int)FORTRAN_DIM(x1 / z[1], 0.0);
            var xb = xn - (int)FORTRAN_DIM(xn, x2 / z[1]);
            if (xb <= xa)
            {
                xa = FORTRAN_DIM(xa, 1.0);
                xb = xn - FORTRAN_DIM(xn, xb + 1.0);
            }
            var ja = (int)xa;
            var jb = (int)xb;
            var n = jb - ja;
            xa = xb - xa;
            var x = -0.5 * xa;
            xb += x;
            var a = 0.5 * (z[ja + 2] + z[jb + 2]);
            var b = 0.5 * (z[ja + 2] - z[jb + 2]) * x;
            for (var i = 2; i <= n; ++i)
            {
                ++ja;
                x += 1.0;
                a += z[ja + 2];
                b += z[ja + 2] * x;
            }
            a /= xa;
            b = b * 12.0 / ((xa * xa + 2.0) * xa);
            z0 = a - b * xb;
            zn = a + b * (xn - xb);
        }

        double qtile(int nn, Span<double> a, int ir)
        {
            double q = 0;
            int j1 = 0, i0 = 0;
            bool done = false;
            bool goto10 = true;

            var m = 0;
            var n = nn;
            var k = mymin(mymax(0, ir), n);
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

        double qerf(double z)
        {
            const double b1 = 0.319381530, b2 = -0.356563782, b3 = 1.781477937;
            const double b4 = -1.821255987, b5 = 1.330274429;
            const double rp = 4.317008, rrt2pi = 0.398942280;
            double qerfv;
            var x = z;
            var t = Abs(x);
            if (t >= 10.0)
                qerfv = 0.0;
            else
            {
                t = rp / (t + rp);
                qerfv = Exp(-0.5 * x * x) * rrt2pi * ((((b5 * t + b4) * t + b3) * t + b2) * t + b1) * t;
            }
            if (x < 0.0) qerfv = 1.0 - qerfv;
            return qerfv;
        }

        double d1thx(double[] pfl, double x1, double x2)
        {
            var np = (int)pfl[0];
            var xa = x1 / pfl[1];
            var xb = x2 / pfl[1];
            var d1thxv = 0.0;
            if (xb - xa < 2.0)  // exit out
                return d1thxv;
            var ka = (int)(0.1 * (xb - xa + 8.0));
            ka = mymin(mymax(4, ka), 25);
            var n = 10 * ka - 5;
            var kb = n - ka + 1;
            double sn = n - 1;
            var s = new double[n + 2];
            s[0] = sn;
            s[1] = 1.0;
            xb = (xb - xa) / sn;
            var k = (int)(xa + 1.0);
            xa -= (double)k;
            for (var j = 0; j < n; j++)
            {
                while (xa > 0.0 && k < np)
                {
                    xa -= 1.0;
                    ++k;
                }
                s[j + 2] = pfl[k + 2] + (pfl[k + 2] - pfl[k + 1]) * xa;
                xa = xa + xb;
            }
            z1sq1(s, 0.0, sn, out xa, out xb);
            xb = (xb - xa) / sn;
            for (var j = 0; j < n; j++)
            {
                s[j + 2] -= xa;
                xa = xa + xb;
            }
            var s2 = s.AsSpan(2);
            d1thxv = qtile(n - 1, s2, ka - 1) - qtile(n - 1, s2, kb - 1);
            d1thxv /= 1.0 - 0.8 * Exp(-(x2 - x1) / 50.0e3);
            return d1thxv;
        }

        void qlrpfl(double[] pfl, int klimx, int mdvarx)
        {
            var xl = new double[2];

            _dist = pfl[0] * pfl[1];
            var np = (int)pfl[0];
            hzns(pfl);
            for (var j = 0; j < 2; j++)
                xl[j] = mymin(15.0 * _hg[j], 0.1 * _dl[j]);
            xl[1] = _dist - xl[1];
            _dh = d1thx(pfl, xl[0], xl[1]);
            if (_dl[0] + _dl[1] > 1.5 * _dist)
            {
                z1sq1(pfl, xl[0], xl[1], out var za, out var zb);
                _he[0] = _hg[0] + FORTRAN_DIM(pfl[2], za);
                _he[1] = _hg[1] + FORTRAN_DIM(pfl[np + 2], zb);
                for (var j = 0; j < 2; j++)
                    _dl[j] = Sqrt(2.0 * _he[j] / _gme) *
                                Exp(-0.07 * Sqrt(_dh / mymax(_he[j], 5.0)));
                var q = _dl[0] + _dl[1];

                if (q <= _dist)
                {
                    q = Pow(_dist / q, 2.0);
                    for (var j = 0; j < 2; j++)
                    {
                        _he[j] *= q;
                        _dl[j] = Sqrt(2.0 * _he[j] / _gme) *
                              Exp(-0.07 * Sqrt(_dh / mymax(_he[j], 5.0)));
                    }
                }
                for (var j = 0; j < 2; j++)
                {
                    q = Sqrt(2.0 * _he[j] / _gme);
                    _the[j] = (0.65 * _dh * (q / _dl[j] - 1.0) - 2.0 *
                                 _he[j]) / q;
                }
            }
            else
            {
                z1sq1(pfl, xl[0], 0.9 * _dl[0], out var za, out _);
                z1sq1(pfl, _dist - 0.9 * _dl[1], xl[1], out _, out var zb);
                _he[0] = _hg[0] + FORTRAN_DIM(pfl[2], za);
                _he[1] = _hg[1] + FORTRAN_DIM(pfl[np + 2], zb);
            }
            _mdp = -1;
            _lvar = mymax(_lvar, 3);
            if (mdvarx >= 0)
            {
                _mdvar = mdvarx;
                _lvar = mymax(_lvar, 4);
            }
            if (klimx > 0)
            {
                _klim = klimx;
                _lvar = 5;
            }
            lrprop(0.0);
        }

        double deg2rad(double d)
        {
            return d * 3.1415926535897 / 180.0;
        }

        //********************************************************
        //* Point-To-Point Mode Calculations                     *
        //********************************************************

        void point_to_point(double[] elev, double tht_m, double rht_m,
                  double eps_dielect, double sgm_conductivity, double eno_ns_surfref,
                  double frq_mhz, int radio_climate, int pol, double conf, double rel,
                  out double dbloss, out int strmode, out int errnum)
        // pol: 0-Horizontal, 1-Vertical
        // radio_climate: 1-Equatorial, 2-Continental Subtropical, 3-Maritime Tropical,
        //                4-Desert, 5-Continental Temperate, 6-Maritime Temperate, Over Land,
        //                7-Maritime Temperate, Over Sea
        // conf, rel: .01 to .99
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
            double zsys = 0;

            strmode = -1;  // mode is undefined
            _hg[0] = tht_m; _hg[1] = rht_m;
            _klim = radio_climate;
            _kwx = 0;
            _lvar = 5;
            _mdp = -1;
            var zc = qerfi(conf);
            var zr = qerfi(rel);
            var np = (int)elev[0];
            var eno = eno_ns_surfref;
            var enso = 0.0;
            var q = enso;
            if (q <= 0.0)
            {
                var ja = (int)(3.0 + 0.1 * elev[0]);
                var jb = np - ja + 6;
                for (var i = ja - 1; i < jb; ++i)
                    zsys += elev[i];
                zsys /= (jb - ja + 1);
                q = eno;
            }
            _mdvar = 12;
            qlrps(frq_mhz, zsys, q, pol, eps_dielect, sgm_conductivity);
            qlrpfl(elev, _klim, _mdvar);
            var fs = 32.45 + 20.0 * Log10(frq_mhz) + 20.0 * Log10(_dist / 1000.0);
            q = _dist - _dla;
            if ((int)q < 0.0)
                strmode = 0;  // Line-Of-Sight Mode
            else
            {
                if ((int)q == 0.0)
                    strmode = 4;  // Single Horizon
                else if ((int)q > 0.0)
                    strmode = 8;  // Double Horizon
                if (_dist <= _dlsa || _dist <= _dx)
                    strmode += 1; // Diffraction Dominant
                else if (_dist > _dx)
                    strmode += 2; // Troposcatter Dominant
            }
            dbloss = avar(zr, 0.0, zc) + fs;
            errnum = _kwx;
        }


        public void point_to_pointMDH(double[] elev, double tht_m, double rht_m,
                  double eps_dielect, double sgm_conductivity, double eno_ns_surfref,
                  double frq_mhz, RadioClimate radio_climate, Polarization pol, VariabilityMode mdvar, double timepct, double locpct, double confpct,
                  out double dbloss, out PropMode propmode, out double deltaH, out int errnum)
        // pol: 0-Horizontal, 1-Vertical
        // radio_climate: 1-Equatorial, 2-Continental Subtropical, 3-Maritime Tropical,
        //                4-Desert, 5-Continental Temperate, 6-Maritime Temperate, Over Land,
        //                7-Maritime Temperate, Over Sea
        // timepct, locpct, confpct: .01 to .99
        // elev[]: [num points - 1], [delta dist(meters)], [height(meters) point 1], ..., [height(meters) point n]
        // propmode:  Value   Mode
        //             -1     mode is undefined
        //              0     Line of Sight
        //              5     Single Horizon, Diffraction
        //              6     Single Horizon, Troposcatter
        //              9     Double Horizon, Diffraction
        //             10     Double Horizon, Troposcatter
        // errnum: 0- No Error.
        //         1- Warning: Some parameters are nearly out of range.
        //                     Results should be used with caution.
        //         2- Note: Default parameters have been substituted for impossible ones.
        //         3- Warning: A combination of parameters is out of range.
        //                     Results are probably invalid.
        //         Other-  Warning: Some parameters are out of range.
        //                          Results are probably invalid.
        {
            propmode = PropMode.Undefined;  // mode is undefined
            _hg[0] = tht_m;
            _hg[1] = rht_m;
            _klim = (int)radio_climate;
            _kwx = 0;
            _lvar = 5;
            _mdp = -1;

            var zsys = GetAverage(elev);
            _mdvar = (int)mdvar + 10;
            qlrps(frq_mhz, zsys, eno_ns_surfref, (int)pol, eps_dielect, sgm_conductivity);
            qlrpfl(elev, _klim, _mdvar);
            var fs = 32.45 + 20.0 * Log10(frq_mhz) + 20.0 * Log10(_dist / 1000.0);
            deltaH = _dh;
            propmode = GetPropMode(_dist - _dla);
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

        private PropMode GetPropMode(double q)
        {
            PropMode propmode;
            if ((int)q < 0.0)
                propmode = PropMode.LineOfSight;
            else
            {
                if ((int)q == 0.0)
                    propmode = PropMode.SingleHorizon;
                else // if ((int)q > 0.0)
                    propmode = PropMode.DoubleHorizon;
                if (_dist <= _dlsa || _dist <= _dx)
                    propmode |= PropMode.DiffractionDominant;
                else if (_dist > _dx)
                    propmode |= PropMode.TroposcatterDominant;
            }
            return propmode;
        }

        void point_to_pointDH(double[] elev, double tht_m, double rht_m,
                  double eps_dielect, double sgm_conductivity, double eno_ns_surfref,
                  double frq_mhz, int radio_climate, int pol, double conf, double rel,
                  out double dbloss, out double deltaH, out int errnum)
        // pol: 0-Horizontal, 1-Vertical
        // radio_climate: 1-Equatorial, 2-Continental Subtropical, 3-Maritime Tropical,
        //                4-Desert, 5-Continental Temperate, 6-Maritime Temperate, Over Land,
        //                7-Maritime Temperate, Over Sea
        // conf, rel: .01 to .99
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
            double zsys = 0;

            var strmode = -1; // mode is undefined
            _hg[0] = tht_m; _hg[1] = rht_m;
            _klim = radio_climate;
            _kwx = 0;
            _lvar = 5;
            _mdp = -1;
            var zc = qerfi(conf);
            var zr = qerfi(rel);
            var np = (int)elev[0];
            var dkm = (elev[1] * elev[0]) / 1000.0;
            var xkm = elev[1] / 1000.0;
            var eno = eno_ns_surfref;
            var enso = 0.0;
            var q = enso;
            if (q <= 0.0)
            {
                var ja = (int)(3.0 + 0.1 * elev[0]);
                var jb = np - ja + 6;
                for (var i = ja - 1; i < jb; ++i)
                    zsys += elev[i];
                zsys /= (jb - ja + 1);
                q = eno;
            }
            _mdvar = 12;
            qlrps(frq_mhz, zsys, q, pol, eps_dielect, sgm_conductivity);
            qlrpfl(elev, _klim, _mdvar);
            var fs = 32.45 + 20.0 * Log10(frq_mhz) + 20.0 * Log10(_dist / 1000.0);
            deltaH = _dh;
            q = _dist - _dla;
            if ((int)q < 0.0)
                strmode = 0;  // Line-Of-Sight Mode
            else
            {
                if ((int)q == 0.0)
                    strmode = 4;  // Single Horizon
                else if ((int)q > 0.0)
                    strmode = 8;  // Double Horizon
                if (_dist <= _dlsa || _dist <= _dx)
                    strmode += 1; // Diffraction Dominant
                else if (_dist > _dx)
                    strmode += 2; // Troposcatter Dominant
            }
            dbloss = avar(zr, 0.0, zc) + fs;      //avar(time,location,confidence)
            errnum = _kwx;
        }


        //********************************************************
        //* Area Mode Calculations                               *
        //********************************************************

        public void area(VariabilityMode ModVar, double deltaH, double tht_m, double rht_m,
                  double dist_km, SiteCriteria TSiteCriteria, SiteCriteria RSiteCriteria,
                  double eps_dielect, double sgm_conductivity, double eno_ns_surfref,
                  double frq_mhz, RadioClimate radio_climate, Polarization pol, double pctTime, double pctLoc,
                  double pctConf, out double dbloss, out PropMode strmode, out int errnum)
        {
            // pol: 0-Horizontal, 1-Vertical
            // TSiteCriteria, RSiteCriteria:
            //		   0 - random, 1 - careful, 2 - very careful
            // radio_climate: 1-Equatorial, 2-Continental Subtropical, 3-Maritime Tropical,
            //                4-Desert, 5-Continental Temperate, 6-Maritime Temperate, Over Land,
            //                7-Maritime Temperate, Over Sea
            // ModVar: 0 - Single: pctConf is "Time/Situation/Location", pctTime, pctLoc not used
            //         1 - Individual: pctTime is "Situation/Location", pctConf is "Confidence", pctLoc not used
            //         2 - Mobile: pctTime is "Time/Locations (Reliability)", pctConf is "Confidence", pctLoc not used
            //         3 - Broadcast: pctTime is "Time", pctLoc is "Location", pctConf is "Confidence"
            // pctTime, pctLoc, pctConf: .01 to .99
            // errnum: 0- No Error.
            //         1- Warning: Some parameters are nearly out of range.
            //                     Results should be used with caution.
            //         2- Note: Default parameters have been substituted for impossible ones.
            //         3- Warning: A combination of parameters is out of range.
            //                     Results are probably invalid.
            //         Other-  Warning: Some parameters are out of range.
            //                          Results are probably invalid.
            // NOTE: strmode is not used at this time.
            var kst = new int[2];

            strmode = PropMode.Undefined;  // mode is undefined
            kst[0] = (int)TSiteCriteria;
            kst[1] = (int)RSiteCriteria;
            _dh = deltaH;
            _hg[0] = tht_m;
            _hg[1] = rht_m;
            _klim = (int)radio_climate;
            _ens = eno_ns_surfref;
            _kwx = 0;
            var ivar = (int)ModVar;
            var ipol = (int)pol;
            qlrps(frq_mhz, 0.0, eno_ns_surfref, ipol, eps_dielect, sgm_conductivity);
            qlra(kst, _klim, ivar);
            if (_lvar < 1) _lvar = 1;
            lrprop(dist_km * 1000.0);
            var fs = 32.45 + 20.0 * Log10(frq_mhz) + 20.0 * Log10(_dist / 1000.0);
            var xlb = fs + avar(qerfi(pctTime), qerfi(pctLoc), qerfi(pctConf));
            dbloss = xlb;
            if (_kwx == 0)
                errnum = 0;
            else
                errnum = _kwx;
        }


        double ITMAreadBLoss(VariabilityMode ModVar, double deltaH, double tht_m, double rht_m,
                  double dist_km, SiteCriteria TSiteCriteria, SiteCriteria RSiteCriteria,
                  double eps_dielect, double sgm_conductivity, double eno_ns_surfref,
                  double frq_mhz, RadioClimate radio_climate, Polarization pol, double pctTime, double pctLoc,
                  double pctConf)
        {
            area(ModVar, deltaH, tht_m, rht_m, dist_km, TSiteCriteria, RSiteCriteria,
                  eps_dielect, sgm_conductivity, eno_ns_surfref,
                  frq_mhz, radio_climate, pol, pctTime, pctLoc,
                  pctConf, out var dbloss, out _, out _);
            return dbloss;
        }

        public double ITMDLLVersion()
        {
            return 7.0;
        }
    }
}
