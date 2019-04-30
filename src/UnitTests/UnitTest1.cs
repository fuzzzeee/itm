using LongleyRice;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using System;
using System.Collections.Generic;
using System.Runtime.InteropServices;

namespace UnitTests
{
    [TestClass]
    public class UnitTests
    {
        [DllImport("itm.dll")]
        private static extern void point_to_pointMDH(double[] elev, double tht_m, double rht_m,
            double eps_dielect, double sgm_conductivity, double eno_ns_surfref,
            double frq_mhz, int radio_climate, int pol, int mdvar, double timepct, double locpct, double confpct,
            out double dbloss, out int propmode, out double deltaH, out int errnum);

        [DllImport("itm.dll")]
        private static extern void area(int ModVar, double deltaH, double tht_m, double rht_m,
            double dist_km, int TSiteCriteria, int RSiteCriteria,
            double eps_dielect, double sgm_conductivity, double eno_ns_surfref,
            double frq_mhz, int radio_climate, int pol, double pctTime, double pctLoc,
            double pctConf, out double dbloss, IntPtr strmode, out int errnum);

        const double delta = 0.00000001;

        [TestMethod]
        public void ModelTests()
        {
            var model = new AreaModel(2000);
            Assert.ThrowsException<ArgumentOutOfRangeException>(() => model.SurfaceRefractivity = int.MinValue);
            Assert.ThrowsException<ArgumentOutOfRangeException>(() => model.SurfaceRefractivity = int.MaxValue);
            Assert.ThrowsException<ArgumentException>(() => model.GroundQuality = (GroundQuality)int.MaxValue);
            Assert.ThrowsException<ArgumentException>(() => model.Climate = (RadioClimate)int.MaxValue);
            Assert.ThrowsException<ArgumentOutOfRangeException>(() => model.Frequency = int.MinValue);
            Assert.ThrowsException<ArgumentOutOfRangeException>(() => model.Frequency = int.MaxValue);
            Assert.ThrowsException<ArgumentOutOfRangeException>(() => model.ErrorIndicator = int.MinValue);
            Assert.ThrowsException<ArgumentOutOfRangeException>(() => model.ErrorIndicator = int.MaxValue);
            Assert.ThrowsException<ArgumentOutOfRangeException>(() => model.Variability.Confidence = int.MinValue);
            Assert.ThrowsException<ArgumentOutOfRangeException>(() => model.Variability.Confidence = int.MaxValue);
            Assert.ThrowsException<ArgumentOutOfRangeException>(() => model.Transmitter.Height = int.MinValue);
        }

        [TestMethod]
        public void AreaModelTests()
        {
            var model = new AreaModel(2000);
            Assert.ThrowsException<ArgumentException>(() => model.Distance = int.MinValue);
        }

        [TestMethod]
        public void PointToPointModelTests()
        {
            var model = new PointToPointModel(new double[2], 1);
            Assert.ThrowsException<ArgumentNullException>(() => model.Elevations = null);
            Assert.ThrowsException<ArgumentException>(() => model.Elevations = new double[0]);
            Assert.ThrowsException<ArgumentException>(() => model.Elevations = new double[1]);
            Assert.ThrowsException<ArgumentException>(() => model.Elevations = new double[1]);
            model.Elevations = new double[2];
            Assert.ThrowsException<ArgumentOutOfRangeException>(() => model.Distance = int.MinValue);
            model.Distance = 1;
        }

        [TestMethod]
        public void PointToPointTests()
        {
            var itm = new IrregularTerrainModel();
            foreach (var p in GetPointToPointModels())
            {
                var e = new double[p.Elevations.Length + 2];
                e[0] = p.Elevations.Length - 1;
                e[1] = p.Distance / e[0];
                p.Elevations.CopyTo(e, 2);
                point_to_pointMDH(e, p.Transmitter.Height, p.Receiver.Height, p.GroundDielectric, p.GroundConductivity, p.SurfaceRefractivity, p.Frequency, (int)p.Climate, (int)p.Polarization, (int)p.Variability.Mode, p.Variability.Time, p.Variability.Location, p.Variability.Confidence,
                    out var dbloss0, out var propMode0, out var deltaH0, out var errnum0);

#if DEBUG
                itm.UseOriginal = true;
                itm.PointToPoint(p);

                Assert.AreEqual(dbloss0, p.DbLoss, delta);
                Assert.AreEqual(propMode0, (int)p.PropMode);
                Assert.AreEqual(deltaH0, p.DeltaH, delta);
                Assert.AreEqual(errnum0, p.ErrorIndicator);

                itm.UseOriginal = false;
#endif
                itm.PointToPoint(p);

                Assert.AreEqual(dbloss0, p.DbLoss, delta);
                Assert.AreEqual((int)propMode0, (int)p.PropMode);
                Assert.AreEqual(deltaH0, p.DeltaH, delta);
                Assert.AreEqual(errnum0, p.ErrorIndicator);
            }
        }

        [TestMethod]
        public void AreaTests()
        {
            var itm = new IrregularTerrainModel();
            foreach (var p in GetAreaModels())
            {
                area((int)p.Variability.Mode, p.DeltaH, p.Transmitter.Height, p.Receiver.Height, p.Distance, (int)p.Transmitter.SiteCriteria,
                    (int)p.Receiver.SiteCriteria, p.GroundDielectric, p.GroundConductivity, p.SurfaceRefractivity, p.Frequency,
                    (int)p.Climate, (int)p.Polarization, p.Variability.Time, p.Variability.Location, p.Variability.Confidence, out var dbloss0,
                    IntPtr.Zero,
                    out var errnum0);

#if DEBUG
                itm.UseOriginal = true;
                itm.Area(p);

                Assert.AreEqual(dbloss0, p.DbLoss, delta);
                Assert.AreEqual(errnum0, p.ErrorIndicator);

                itm.UseOriginal = false;
#endif
                itm.Area(p);

                Assert.AreEqual(dbloss0, p.DbLoss, delta);
                Assert.AreEqual(errnum0, p.ErrorIndicator);
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
                        foreach (VariabilityMode mode in Enum.GetValues(typeof(VariabilityMode)))
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
                                            model.Variability.Mode = mode;
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
