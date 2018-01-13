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
                        foreach (var deltaH in new[] { 0, 30, 90, 200, 500 })
                        {
                            area((int)ModVar, deltaH, p.tht_m, p.rht_m, p.elev[1] * p.elev[0] / 1000, (int)SiteCriteria,
                                (int)SiteCriteria, p.eps_dielect, p.sgm_conductivity, p.eno_ns_surfref, p.frq_mhz,
                                (int)p.radio_climate, (int)p.pol, p.timepct, p.locpct, p.confpct, out dbloss0,
                                IntPtr.Zero,
                                out errnum0);

                            itm = new Original();
                            itm.area((int)ModVar, deltaH, p.tht_m, p.rht_m, p.elev[1] * p.elev[0] / 1000,
                                (int)SiteCriteria,
                                (int)SiteCriteria, p.eps_dielect, p.sgm_conductivity, p.eno_ns_surfref, p.frq_mhz,
                                (int)p.radio_climate, (int)p.pol, p.timepct, p.locpct, p.confpct, out dbloss1, out _,
                                out errnum1);

                            Assert.AreEqual(Math.Round(dbloss0, 8), Math.Round(dbloss1, 8));
                            Assert.AreEqual(errnum0, errnum1);

                            itm2 = new Refactored();
                            itm2.area(ModVar, deltaH, p.tht_m, p.rht_m, p.elev[1] * p.elev[0] / 1000, SiteCriteria,
                                SiteCriteria, p.eps_dielect, p.sgm_conductivity, p.eno_ns_surfref, p.frq_mhz,
                                p.radio_climate, p.pol, p.timepct, p.locpct, p.confpct, out dbloss2, out errnum2);

                            Assert.AreEqual(dbloss1, dbloss2);
                            Assert.AreEqual(errnum1, errnum2);
                        }
                    }
                }
            }
        }

        static IEnumerable<Parameters> GetParameters()
        {
            var elevations = new double[] { 4, 500, 108, 106, 105, 107, 109 };
            var eps_dielect = new[] { 15, 4, 25, 81, 81 };
            var sgm_conductivity = new[] { 0.005, 0.001, 0.02, 0.01, 5 };

            foreach (var ht_m in new[] { 2, 5, 10, 50 })
            {
                foreach (var frq_mhz in new[] { 50, 100, 500, 915, 1000, 2000, 5000 })
                {
                    foreach (var pct in new[] { 0.01, 0.1, 0.5, 0.9, 0.99 })
                    {
                        foreach (RadioClimate radio_climate in Enum.GetValues(typeof(RadioClimate)))
                        {
                            var eno_ns_surfref = 301;
                            switch (radio_climate)
                            {
                                case RadioClimate.Equatorial:
                                    eno_ns_surfref = 360;
                                    break;
                                case RadioClimate.ContinentalSubtropical:
                                    eno_ns_surfref = 320;
                                    break;
                                case RadioClimate.MaritimeTropical:
                                    eno_ns_surfref = 370;
                                    break;
                                case RadioClimate.Desert:
                                    eno_ns_surfref = 280;
                                    break;
                                case RadioClimate.MaritimeTemperateOverLand:
                                    eno_ns_surfref = 320;
                                    break;
                                case RadioClimate.MaritimeTemperateOverSea:
                                    eno_ns_surfref = 350;
                                    break;
                            }
                            foreach (Polarization pol in Enum.GetValues(typeof(Polarization)))
                            {
                                for (var ground = 0; ground < 5; ground++)
                                {
                                    yield return new Parameters
                                    {
                                        confpct = pct,
                                        elev = elevations,
                                        eno_ns_surfref = eno_ns_surfref,
                                        eps_dielect = eps_dielect[ground],
                                        frq_mhz = frq_mhz,
                                        locpct = pct,
                                        pol = pol,
                                        radio_climate = radio_climate,
                                        rht_m = ht_m,
                                        sgm_conductivity = sgm_conductivity[ground],
                                        tht_m = ht_m,
                                        timepct = pct
                                    };
                                }
                            }
                        }
                    }
                }
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
