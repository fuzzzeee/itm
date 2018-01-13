using LongleyRice;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using System;
using System.Collections.Generic;
using System.Runtime.InteropServices;

namespace UnitTests
{
    [TestClass]
    public class UnitTest1
    {
        [DllImport("itm.dll")]
        private static extern void point_to_pointMDH(double[] elev, double tht_m, double rht_m,
            double eps_dielect, double sgm_conductivity, double eno_ns_surfref,
            double frq_mhz, int radio_climate, int pol, double timepct, double locpct, double confpct,
            out double dbloss, out int propmode, out double deltaH, out int errnum);

        [DllImport("itm.dll")]
        private static extern void area(int ModVar, double deltaH, double tht_m, double rht_m,
            double dist_km, int TSiteCriteria, int RSiteCriteria,
            double eps_dielect, double sgm_conductivity, double eno_ns_surfref,
            double frq_mhz, int radio_climate, int pol, double pctTime, double pctLoc,
            double pctConf, out double dbloss, IntPtr strmode, out int errnum);

        [TestMethod]
        public void TestMethod1()
        {
            foreach (var p in GetParameters())
            {
                point_to_pointMDH(p.elev, p.tht_m, p.rht_m, p.eps_dielect, p.sgm_conductivity, p.eps_dielect, p.frq_mhz, (int)p.radio_climate, (int)p.pol, p.timepct, p.locpct, p.confpct,
                    out var dbloss0, out var propMode0, out var deltaH0, out var errnum0);

                var itm = new Original();
                itm.point_to_pointMDH(p.elev, p.tht_m, p.rht_m, p.eps_dielect, p.sgm_conductivity, p.eps_dielect, p.frq_mhz, (int)p.radio_climate, (int)p.pol, p.timepct, p.locpct, p.confpct,
                    out var dbloss1, out var propMode1, out var deltaH1, out var errnum1);

                Assert.AreEqual(Math.Round(dbloss0, 10), Math.Round(dbloss1, 10)); // fractional precision variance after 10 decimal places
                Assert.AreEqual(propMode0, propMode1);
                Assert.AreEqual(Math.Round(deltaH0, 10), Math.Round(deltaH1, 10)); // fractional precision variance after 10 decimal places
                Assert.AreEqual(errnum0, errnum1);

                var itm2 = new Refactored();
                itm2.point_to_pointMDH(p.elev, p.tht_m, p.rht_m, p.eps_dielect, p.sgm_conductivity, p.eps_dielect, p.frq_mhz, p.radio_climate, p.pol, p.timepct, p.locpct, p.confpct,
                    out var dbloss2, out var propMode2, out var deltaH2, out var errnum2);

                Assert.AreEqual(dbloss1, dbloss2);
                Assert.AreEqual(propMode1, (int)propMode2);
                Assert.AreEqual(deltaH1, deltaH2);
                Assert.AreEqual(errnum1, errnum2);

                foreach (SiteCriteria SiteCriteria in Enum.GetValues(typeof(SiteCriteria)))
                {
                    foreach (VariabilityMode ModVar in Enum.GetValues(typeof(VariabilityMode)))
                    {
                        area((int)ModVar, 1, p.tht_m, p.rht_m, p.elev[1] * p.elev[0] / 1000, (int)SiteCriteria,
                            (int)SiteCriteria, p.eps_dielect, p.sgm_conductivity, p.eno_ns_surfref, p.frq_mhz,
                            (int)p.radio_climate, (int)p.pol, p.timepct, p.locpct, p.confpct, out dbloss0, IntPtr.Zero,
                            out errnum0);

                        itm = new Original();
                        itm.area((int)ModVar, 1, p.tht_m, p.rht_m, p.elev[1] * p.elev[0] / 1000, (int)SiteCriteria,
                            (int)SiteCriteria, p.eps_dielect, p.sgm_conductivity, p.eno_ns_surfref, p.frq_mhz,
                            (int)p.radio_climate, (int)p.pol, p.timepct, p.locpct, p.confpct, out dbloss1, out _,
                            out errnum1);

                        Assert.AreEqual(Math.Round(dbloss0, 9), Math.Round(dbloss1, 9));
                        Assert.AreEqual(errnum0, errnum1);

                        itm2 = new Refactored();
                        itm2.area(ModVar, 1, p.tht_m, p.rht_m, p.elev[1] * p.elev[0] / 1000, SiteCriteria,
                            SiteCriteria, p.eps_dielect, p.sgm_conductivity, p.eno_ns_surfref, p.frq_mhz,
                            p.radio_climate, p.pol, p.timepct, p.locpct, p.confpct, out dbloss2, out errnum2);

                        Assert.AreEqual(dbloss1, dbloss2);
                        Assert.AreEqual(errnum1, errnum2);
                    }
                }
            }
        }

        static IEnumerable<Parameters> GetParameters()
        {
            var elevations = new double[] { 4, 500, 108, 106, 105, 107, 109 };

            foreach (var height in GetHeights())
            {
                foreach (var frq in new[] { 50, 100, 500, 915, 1000, 2000, 5000 })
                {
                    foreach (var pct in GetPercentages())
                    {
                        foreach (RadioClimate rc in Enum.GetValues(typeof(RadioClimate)))
                        {
                            foreach (Polarization pol in Enum.GetValues(typeof(Polarization)))
                            {
                                yield return new Parameters
                                {
                                    confpct = pct,
                                    elev = elevations,
                                    eno_ns_surfref = 310,
                                    eps_dielect = 15,
                                    frq_mhz = frq,
                                    locpct = pct,
                                    pol = pol,
                                    radio_climate = rc,
                                    rht_m = height,
                                    sgm_conductivity = 0.001,
                                    tht_m = height,
                                    timepct = pct
                                };
                            }
                        }
                    }
                }
            }
        }

        static IEnumerable<int> GetHeights()
        {
            var rand = new Random();
            for (var i = 0; i < 10; i++)
            {
                yield return rand.Next(1, 100);
            }
        }

        static IEnumerable<double> GetPercentages()
        {
            var rand = new Random();
            for (var i = 0; i < 10; i++)
            {
                yield return rand.Next(1, 100) / 100.0;
            }
        }

        class Parameters
        {
            public double[] elev;
            public double tht_m;
            public double rht_m;
            public double eps_dielect;
            public double sgm_conductivity;
            public double eno_ns_surfref;
            public double frq_mhz;
            public RadioClimate radio_climate;
            public Polarization pol;
            public double timepct;
            public double locpct;
            public double confpct;
        }
    }
}
