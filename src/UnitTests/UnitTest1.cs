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
            foreach (var model in GetElevations())
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
                                            model.Climate = climate;
                                            model.GroundQuality = ground;
                                            model.Frequency = frequency;
                                            model.Polarization = polarization;
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

        static IEnumerable<PointToPointModel> GetElevations()
        {
            yield return new PointToPointModel(new double[] { 39, 40, 40, 40, 40, 41, 41, 41, 41, 42, 42, 42, 43, 43, 43, 43, 43, 42, 43, 42, 42, 41, 41, 40, 40, 39, 38, 37, 35, 34, 33, 32, 31, 29, 28, 27, 26, 25, 25, 25, 25, 24, 24, 23, 22, 22, 21, 21, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 21, 21, 22, 22, 22, 23, 23, 23, 23, 23, 23, 23, 24, 24, 24, 24, 24, 25, 26, 26, 27, 28, 29, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 29, 28, 28, 27, 26, 26, 25, 25, 24, 24, 24, 23, 23, 22, 22, 21, 21, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 21, 22, 22, 23, 24, 24, 25, 25, 25, 26, 26, 27, 27, 27, 27, 27, 28, 29, 29, 29, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 31, 31, 32, 32, 33, 33, 34, 34, 34, 35, 35, 35, 35, 35, 35, 35, 35, 35, 36, 36, 36, 37, 37, 37, 38, 38, 39, 39, 39, 39, 40, 40, 40, 40, 41, 41, 41, 42, 42, 42, 43, 43, 43, 44, 44, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 44, 43, 43, 42, 41, 41, 40, 39, 38, 38, 37, 37, 36, 36, 35, 35, 35, 35, 35, 35, 35, 35, 35, 35, 35, 35, 35, 35, 35, 34, 34 }, 2883.78747976532);
            yield return new PointToPointModel(new double[] { 39, 39, 38, 38, 37, 37, 36, 36, 35, 35, 35, 34, 34, 34, 33, 33, 33, 33, 34, 35, 35, 35, 35, 35, 35, 36, 36, 37, 39, 40, 41, 42, 43, 44, 45, 45, 45, 45, 45, 45, 44, 42, 41, 39, 35, 33, 32, 30, 30, 30, 30, 30, 31, 32, 33, 34, 35, 35, 35, 33, 32, 31, 30, 29, 28, 26, 25, 25, 25, 25, 25, 26, 27, 28, 27, 27, 27, 27, 26, 24, 24, 24, 23, 23, 23, 22, 21, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 21, 21, 22, 23, 24, 24, 25, 26, 27, 27, 28, 29, 29, 30, 30, 30, 31, 32, 32, 31, 31, 31, 30, 29, 28, 27, 26, 25, 23, 22, 20, 20, 20, 21, 21, 21, 23, 23, 23, 22, 22, 21, 20, 20, 19, 17, 14, 13, 11, 11, 11, 11, 10, 10, 10, 10, 10, 11, 11, 12, 12, 13, 14, 14, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 16, 16, 17, 19, 19, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 21, 22, 22, 23, 23, 23, 23, 25, 25, 24, 24, 24, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 26, 28, 29, 29, 29, 30, 32, 32, 33, 34, 33, 34, 35, 36, 36, 36, 34, 34, 34, 34, 35, 35, 35, 34, 35, 35, 35, 35, 36, 35, 36, 36, 37, 37, 38, 38, 39, 41, 44, 46, 48, 49, 50, 50, 50, 50, 50, 50, 50, 51, 51, 52, 52, 53, 53, 55, 55, 55, 55, 55, 55, 58, 58 }, 3200.83770382502);
            yield return new PointToPointModel(new double[] { 39, 40, 40, 40, 41, 41, 42, 42, 42, 42, 42, 41, 40, 40, 40, 40, 40, 39, 38, 38, 37, 36, 35, 35, 35, 35, 35, 35, 36, 37, 38, 39, 39, 40, 41, 42, 42, 43, 44, 44, 44, 45, 45, 45, 45, 45, 45, 46, 47, 47, 48, 47, 47, 46, 46, 45, 45, 44, 43, 43, 43, 44, 44, 46, 46, 45, 45, 44, 44, 43, 43, 42, 41, 39, 38, 37, 36, 35, 35, 35, 35, 35, 35, 35, 35, 36, 38, 39, 41, 43, 44, 45, 45, 44, 43, 43, 43, 43, 43, 43, 43, 43, 42, 42, 42, 42, 41, 39, 38, 36, 35, 35, 35, 35, 35, 35, 35, 35, 35, 35, 35, 35, 35, 35, 35, 36, 36, 37, 38, 38, 39, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 41, 42, 42, 44, 44, 45, 46, 47, 48, 48, 49, 48, 48, 48, 47, 47, 47, 47, 49, 49, 50, 51, 52, 53, 53, 54, 53, 53, 53, 52, 51, 52, 54, 54, 55, 55, 57, 59, 60, 62, 63, 64, 65, 66, 68, 69, 70, 70, 70, 70, 69, 67, 65, 64, 60, 58, 57, 55, 54, 53, 52, 50, 49, 49, 48, 47, 46, 47, 46, 47, 47, 47, 47, 48, 48, 48, 49, 49, 51, 53, 55, 56, 58, 58, 59, 61, 62, 63, 64, 65, 65, 65, 67, 68, 70, 70, 70, 70, 70, 70, 70, 70, 70, 70, 70, 69, 70, 70, 70, 69, 67, 66, 64, 62, 61, 58, 55, 55, 54, 53, 52, 52, 52, 53, 53, 54, 56, 59, 61, 63, 65, 65, 65, 65, 65, 65, 65, 67, 68, 68, 67, 65, 64, 64, 65, 65, 65, 64, 60, 60, 62, 65, 67, 68, 68, 68, 65, 63, 63, 62, 61, 62, 63, 65, 66, 67, 69, 70, 71, 72, 71, 71, 70, 68, 65, 64, 62, 59, 59, 58, 58, 57, 56, 55, 54, 51, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 52, 54, 56, 57, 59, 61, 62, 64, 65, 65, 65, 65, 65, 65, 65, 64, 63, 62, 62, 64, 64, 66, 67 }, 3837.97846287605);
            yield return new PointToPointModel(new double[] { 39, 39, 40, 41, 42, 42, 43, 44, 45, 45, 45, 45, 44, 44, 43, 42, 41, 41, 39, 38, 37, 37, 35, 34, 33, 33, 31, 30, 30, 30, 30, 30, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 41, 42, 44, 44, 44, 45, 45, 45, 44, 43, 43, 42, 40, 39, 39, 38, 37, 36, 36, 34, 33, 32, 30, 29, 28, 27, 26, 26, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 26, 26, 27, 27, 28, 28, 29, 29, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 31, 33, 34, 34, 36, 37, 37, 38, 39, 39, 40, 40, 40, 40, 40, 40, 40, 40, 40, 39, 38, 37, 36, 35, 35, 35, 35, 35, 35, 35, 35, 35, 35, 36, 37, 38, 39, 40, 40, 41, 41, 42, 42, 43, 43, 44, 45, 45, 45, 45, 45, 46, 47, 48, 48, 48, 48, 48, 49, 49, 48, 50, 50, 51, 52, 51, 52, 52, 53, 53, 54, 55, 55, 55, 55, 54, 53, 52, 51, 50, 48, 48, 47, 45, 44, 44, 43, 42, 41, 40, 39, 38, 36, 36, 35, 34, 33, 32, 31, 31, 30, 30, 30, 30, 30, 30, 30, 31, 32, 32, 32, 33, 34, 34, 34, 34, 32, 33, 32, 32, 31, 31, 31, 31, 32, 33, 34, 35, 36, 37, 37, 38, 38, 39, 40, 40, 40, 41, 40, 40, 40, 40, 40, 40, 41, 42, 43, 44, 45, 46, 48, 48, 50, 51, 52, 53, 55, 55, 55, 55, 55, 55, 54, 54, 52, 49, 48, 47, 46, 45, 43, 41, 41, 40, 40, 40, 40, 40, 40, 42, 42, 44, 45, 46, 47, 48, 48, 50, 50, 51, 52, 53, 53, 54, 54, 55, 55, 55, 56, 56, 56, 57, 56, 55, 55, 54, 51, 51, 50, 50, 50, 50, 50, 50, 49, 50, 48, 47, 46, 45, 43, 43, 41, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 41, 42, 44, 46, 46, 48, 49, 50, 50, 50, 50, 50, 50, 51, 53, 53, 53, 52, 51, 50, 49, 48, 48, 46, 46, 45, 44, 43, 42, 41, 41, 40, 40, 40, 40, 41, 41, 42, 44, 44, 45, 46, 46, 48, 49, 49, 50, 51, 51, 52, 53, 53, 54, 55, 55, 56, 57, 57, 58, 57 }, 4131.34308516371);
            yield return new PointToPointModel(new double[] { 39, 39, 40, 40, 41, 42, 42, 43, 44, 44, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 44, 42, 41, 39, 37, 36, 33, 32, 29, 29, 29, 29, 29, 29, 28, 30, 30, 32, 32, 33, 33, 35, 36, 36, 37, 37, 38, 37, 39, 38, 39, 38, 36, 37, 35, 35, 34, 34, 34, 33, 33, 32, 31, 30, 30, 29, 28, 27, 27, 27, 26, 26, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 26, 27, 28, 29, 30, 30, 31, 31, 31, 32, 33, 34, 34, 35, 35, 35, 35, 35, 35, 35, 35, 35, 35, 35, 35, 35, 35, 35, 35, 35, 35, 35, 34, 34, 34, 33, 32, 32, 32, 31, 30, 30, 29, 28, 27, 26, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 26, 26, 27, 27, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 27, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 29, 29, 29, 29, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 29, 29, 28, 28, 28, 28, 28, 28, 27, 27, 27, 27, 27, 27, 27, 27, 27, 26, 26, 26, 26, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 26, 27, 27, 28, 29, 29, 30, 30, 30, 31, 31, 32, 32, 33, 34, 34, 35, 35, 36, 37, 38, 38, 39, 40, 40, 42, 42, 43, 43, 45, 45, 46, 47, 47, 48, 48, 48, 48, 48, 48, 47, 46, 45, 45, 44, 43, 43, 43, 43, 42, 41, 41, 41, 41, 40, 40, 40, 40, 39, 40, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 40, 40, 41, 41, 41, 41, 41, 41, 41, 41, 40, 40, 40, 40, 40, 39, 39, 38, 39, 39, 38, 39, 39, 39, 39, 39, 38, 38, 38, 37, 38, 38, 38, 38, 38, 38, 37, 36, 35, 34, 34, 34, 33, 34, 33, 33, 33, 33, 33, 32, 32, 32, 31, 31, 31, 31, 30, 30, 30, 30, 30, 30, 30, 31, 33, 34, 35, 36, 37, 38, 40, 41, 41, 42, 43, 44, 45, 45, 45, 45, 45, 45, 45, 45, 46, 46, 46, 46, 45, 44, 42, 40, 40, 40, 40, 40, 40, 40, 41, 43, 45, 45, 45, 45, 46, 46, 46, 47, 47, 47, 47, 48, 48 }, 4065.93348630763);
            yield return new PointToPointModel(new double[] { 39, 38, 37, 36, 36, 35, 34, 34, 33, 31, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 31, 32, 33, 34, 34, 35, 35, 35, 35, 35, 35, 34, 33, 33, 32, 31, 31, 31, 30, 30, 31, 31, 31, 31, 32, 32, 32, 32, 31, 31, 31, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 29, 28, 28, 28, 27, 27, 26, 26, 25, 25, 25, 25, 25, 25, 25, 24, 24, 24, 23, 23, 22, 21, 21, 21, 19, 15, 15, 15, 15, 14, 14, 13, 12, 12, 11, 11, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 16, 22, 30, 33, 41, 49, 51, 59, 60, 60, 60, 60, 57, 58, 58, 57, 58, 59, 62, 62, 67, 70, 71, 71, 75, 79, 78, 79, 78, 78, 76, 74, 74, 71, 68, 66, 66, 63, 61, 61, 59, 58, 57, 57, 57, 56, 56, 55, 53, 51, 51, 48, 45, 45, 43, 42, 40, 40, 40, 40, 40, 40, 40, 40, 41, 45, 46, 46, 46, 46, 46, 44, 43, 42, 41, 39, 38, 36, 35, 35, 35, 35, 36, 38, 40, 40, 41, 43, 42, 41, 43, 43, 43, 45, 45, 46, 47, 50, 52, 53, 54, 55, 55, 55, 55, 55, 55, 55, 54, 53, 52, 51, 50, 50, 45, 39, 38, 37, 36, 35, 35, 35, 35, 35, 35, 36, 38, 38, 40, 41, 41, 42, 43, 43, 44, 47, 51, 51, 52, 54, 54, 55, 58, 60, 60, 60, 59, 58, 57, 55, 54, 53, 52, 51, 49, 47, 45, 45, 45, 45, 45, 49, 50, 51, 53, 55, 56, 56, 58, 59, 60, 60, 61, 60, 60, 59, 56, 57, 56, 54, 54, 50, 47, 46, 47, 48, 51, 51, 54, 56, 56, 59, 60, 60, 60, 60, 57, 57, 55, 51, 50, 49, 48, 46, 46, 44, 42, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 41, 43, 44, 44, 45, 47, 47, 48, 50, 52, 50, 51, 52, 50, 50, 51, 50, 48, 48, 46, 44, 43, 43, 44, 44, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 46, 46, 47, 48, 48, 49, 54, 57, 57, 59, 61, 61, 62, 62, 63, 63, 65, 68, 68, 70, 70, 70, 70, 70, 67, 67, 66, 68, 71, 71, 76, 78, 78, 80, 80, 78, 77, 74, 73, 70, 68, 68, 66, 65, 63, 62, 62, 60, 60, 60, 60, 60, 60, 61, 62, 64, 65, 65, 66, 67, 68, 69, 71, 73, 75 }, 4482.58689429027);
            yield return new PointToPointModel(new double[] { 39, 39, 38, 38, 37, 37, 36, 36, 35, 35, 35, 34, 34, 34, 33, 33, 33, 33, 34, 35, 35, 35, 35, 35, 35, 36, 36, 37, 39, 40, 41, 42, 43, 44, 45, 45, 45, 45, 45, 45, 44, 42, 41, 39, 35, 33, 32, 30, 30, 30, 30, 30, 31, 32, 33, 34, 35, 35, 35, 33, 32, 31, 30, 29, 28, 26, 25, 25, 25, 25, 25, 26, 27, 28, 27, 27, 27, 27, 26, 24, 24, 24, 23, 23, 23, 22, 21, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 21, 21, 22, 23, 24, 24, 25, 26, 27, 27, 28, 29, 29, 30, 30, 30, 31, 32, 32, 31, 31, 31, 30, 29, 28, 27, 26, 25, 23, 22, 20, 20, 20, 21, 21, 21, 23, 23, 23, 22, 22, 21, 20, 20, 19, 17, 14, 13, 11, 11, 11, 11, 10, 10, 10, 10, 10, 11, 11, 12, 12, 13, 14, 14, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 16, 16, 17, 19, 19, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 21, 22, 22, 23, 23, 23, 23, 25, 25, 24, 24, 24, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 26, 28, 29, 29, 29, 30, 32, 32, 33, 34, 33, 34, 35, 36, 36, 36, 34, 34, 34, 34, 35, 35, 35, 34, 35, 35, 35, 35, 36, 35, 36, 36, 37, 37, 38, 38, 39, 41, 44, 46, 48, 49, 50, 50, 50, 50, 50, 50, 50, 51, 51, 52, 52, 53, 53, 55, 55, 55, 55, 55, 55, 58, 58 }, 3200.83770382502);
        }
    }
}
