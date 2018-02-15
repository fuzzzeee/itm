using LongleyRice;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
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

        public TestContext TestContext { get; set; }

        [TestMethod]
        public void TestMethod1()
        {
            var results = new List<PointToPointModel>();
            var times = new Dictionary<int, Stopwatch>
            {
                {0, new Stopwatch()},
                {1, new Stopwatch()},
                {2, new Stopwatch()},
            };

            var itm = new IrregularTerrainModel();
            foreach (var p in GetPointToPointModels())
            {
                var e = new double[p.Elevations.Length + 2];
                e[0] = p.Elevations.Length - 1;
                e[1] = p.Distance / e[0];
                Buffer.BlockCopy(p.Elevations, 0, e, sizeof(double) * 2, sizeof(double) * p.Elevations.Length);
                times[0].Start();
                point_to_pointMDH(e, p.Transmitter.Height, p.Receiver.Height, p.GroundDielectric, p.GroundConductivity, p.SurfaceRefractivity, p.Frequency, (int)p.Climate, (int)p.Polarization, p.Variability.Time, p.Variability.Location, p.Variability.Confidence,
                    out var dbloss0, out var propMode0, out var deltaH0, out var errnum0);
                times[0].Stop();

                itm.UseOriginal = true;
                times[1].Start();
                itm.PointToPoint(p);
                times[1].Stop();

                results.Add(p);

                Assert.AreEqual(Math.Round(dbloss0, 10), Math.Round(p.DbLoss, 10)); // fractional precision variance after 10 decimal places
                Assert.AreEqual(propMode0, (int)p.PropMode);
                Assert.AreEqual(Math.Round(deltaH0, 10), Math.Round(p.DeltaH, 10)); // fractional precision variance after 10 decimal places
                Assert.AreEqual(errnum0, p.ErrorIndicator);

                var dbloss1 = p.DbLoss;
                var propMode1 = p.PropMode;
                var deltaH1 = p.DeltaH;
                var errnum1 = p.ErrorIndicator;

                itm.UseOriginal = false;
                times[2].Start();
                itm.PointToPoint(p);
                times[2].Stop();

                Assert.AreEqual(dbloss1, p.DbLoss);
                Assert.AreEqual((int)propMode1, (int)p.PropMode);
                Assert.AreEqual(deltaH1, p.DeltaH);
                Assert.AreEqual(errnum1, p.ErrorIndicator);
            }

            foreach (var p in GetAreaModels())
            {
                times[0].Start();
                area((int)p.Variability.Mode, p.DeltaH, p.Transmitter.Height, p.Receiver.Height, p.Distance, (int)p.Transmitter.SiteCriteria,
                    (int)p.Receiver.SiteCriteria, p.GroundDielectric, p.GroundConductivity, p.SurfaceRefractivity, p.Frequency,
                    (int)p.Climate, (int)p.Polarization, p.Variability.Time, p.Variability.Location, p.Variability.Confidence, out var dbloss0,
                    IntPtr.Zero,
                    out var errnum0);
                times[0].Stop();

                itm.UseOriginal = true;
                times[1].Start();
                itm.Area(p);
                times[1].Stop();

                Assert.AreEqual(Math.Round(dbloss0, 8), Math.Round(p.DbLoss, 8));
                Assert.AreEqual(errnum0, p.ErrorIndicator);

                var dbloss1 = p.DbLoss;
                var errnum1 = p.ErrorIndicator;

                itm.UseOriginal = false;
                times[2].Start();
                itm.Area(p);
                times[2].Stop();

                Assert.AreEqual(dbloss1, p.DbLoss);
                Assert.AreEqual(errnum1, p.ErrorIndicator);
            }

            TestContext.WriteLine($"C++ = {times[0].Elapsed}, C# Original = {times[1].Elapsed}, C# Refactored = {times[2].Elapsed}");

            var path = Path.Combine(TestContext.TestDir, "results.csv");
            TestContext.WriteLine($"Writing results to {path}");
            using (var sw = new StreamWriter(Path.Combine(TestContext.TestRunResultsDirectory, path)))
            {
                var w = new CsvHelper.CsvWriter(sw);
                w.WriteRecords(results);
            }
        }

        static IEnumerable<PointToPointModel> GetPointToPointModels()
        {
            foreach (var elevations in GetElevations())
            {
                foreach (var height in new[] { 2, 5, 10, 50 })
                {
                    foreach (var frequency in new[] { 50, 107.7, 500, 915, 1000, 2400, 5000 })
                    {
                        foreach (var percent in new[] { 0.01, 0.1, 0.5, 0.9, 0.99 })
                        {
                            foreach (RadioClimate climate in Enum.GetValues(typeof(RadioClimate)))
                            {
                                foreach (Polarization polarization in Enum.GetValues(typeof(Polarization)))
                                {
                                    foreach (GroundQuality ground in Enum.GetValues(typeof(GroundQuality)))
                                    {
                                        var model = new PointToPointModel(elevations, 2000)
                                        {
                                            Climate = climate,
                                            GroundQuality = ground,
                                            Frequency = frequency,
                                            Polarization = polarization
                                        };
                                        model.Variability.Confidence = Math.Max(0.01, percent * 0.9);
                                        model.Variability.Location = percent;
                                        model.Variability.Time = Math.Max(0.01, percent * 0.8);
                                        model.Transmitter.Height = height;
                                        model.Receiver.Height = height * 0.8;
                                        yield return model;
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }

        static IEnumerable<AreaModel> GetAreaModels()
        {
            foreach (var height in new[] { 2, 5, 10, 50 })
            {
                foreach (var frequency in new[] { 50, 107.7, 500, 915, 1000, 2400, 5000 })
                {
                    foreach (var percent in new[] { 0.5, 0.9, 0.1 })
                    {
                        foreach (RadioClimate climate in Enum.GetValues(typeof(RadioClimate)))
                        {
                            foreach (Polarization polarization in Enum.GetValues(typeof(Polarization)))
                            {
                                foreach (TerrainType terrainType in Enum.GetValues(typeof(TerrainType)))
                                {
                                    foreach (GroundQuality ground in Enum.GetValues(typeof(GroundQuality)))
                                    {
                                        foreach (SiteCriteria siteCriteria in Enum.GetValues(typeof(SiteCriteria)))
                                        {
                                            foreach (VariabilityMode variabilityMode in Enum.GetValues(typeof(VariabilityMode)))
                                            {
                                                var model = new AreaModel(2000)
                                                {
                                                    Climate = climate,
                                                    GroundQuality = ground,
                                                    Frequency = frequency,
                                                    Polarization = polarization,
                                                    TerrainType = terrainType,
                                                };
                                                model.Variability.Mode = variabilityMode;
                                                model.Variability.Confidence = Math.Max(0.01, percent * 0.9);
                                                model.Variability.Location = percent;
                                                model.Variability.Time = Math.Max(0.01, percent * 0.8);
                                                model.Transmitter.Height = height;
                                                model.Transmitter.SiteCriteria = siteCriteria;
                                                model.Receiver.Height = height * 0.8;
                                                model.Receiver.SiteCriteria = siteCriteria;
                                                yield return model;
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }

        static IEnumerable<double[]> GetElevations()
        {
            yield return new double[] { 100, 90, 90, 90, 100 }; // simple line of sight
            yield return new double[] { 100, 90, 111, 90, 100 }; // single horizon
            yield return new double[] { 100, 90, 111, 90, 111, 90, 100 }; // double horizon
            yield return new double[] { 100, 90, 113, 90, 113, 90, 100 }; // double horizon
            yield return new double[] { 100, 90, 120, 120, 120, 90, 100 }; // blocked
        }
    }
}
