using System;

namespace LongleyRice
{
    public class IrregularTerrainModel
    {
        public bool UseOriginal { get; set; }

        public void PointToPoint(double[] elev, double dist, double tht_m, double rht_m,
            double eps_dielect, double sgm_conductivity, double eno_ns_surfref,
            double frq_mhz, RadioClimate radio_climate, Polarization pol, double timepct, double locpct, double confpct,
            out double dbloss, out PropMode propmode, out double deltaH, out int errnum)
        {
            var e = new double[elev.Length + 2];
            e[0] = elev.Length - 1;
            e[1] = dist / e[0];
            Buffer.BlockCopy(elev, 0, e, sizeof(double) * 2, sizeof(double) * elev.Length);
            if (UseOriginal)
            {
                var itm = new Original();
                itm.point_to_pointMDH(e, tht_m, rht_m, eps_dielect, sgm_conductivity, eno_ns_surfref, frq_mhz, (int)radio_climate, (int)pol, timepct, locpct, confpct, out dbloss, out var pm, out deltaH, out errnum);
                propmode = (PropMode)pm;
            }
            else
            {
                var itm = new Refactored();
                itm.point_to_pointMDH(e, tht_m, rht_m, eps_dielect, sgm_conductivity, eno_ns_surfref, frq_mhz, radio_climate, pol, timepct, locpct, confpct, out dbloss, out propmode, out deltaH, out errnum);
            }
        }

        public void Area(VariabilityMode ModVar, double deltaH, double tht_m, double rht_m,
            double dist_km, SiteCriteria TSiteCriteria, SiteCriteria RSiteCriteria,
            double eps_dielect, double sgm_conductivity, double eno_ns_surfref,
            double frq_mhz, RadioClimate radio_climate, Polarization pol, double pctTime, double pctLoc,
            double pctConf, out double dbloss, out int errnum)
        {
            if (UseOriginal)
            {
                var itm = new Original();
                itm.area((int)ModVar, deltaH, tht_m, rht_m, dist_km, (int)TSiteCriteria, (int)RSiteCriteria, eps_dielect, sgm_conductivity, eno_ns_surfref, frq_mhz, (int)radio_climate, (int)pol, pctTime, pctLoc, pctConf, out dbloss, out _, out errnum);
            }
            else
            {
                var itm = new Refactored();
                itm.area(ModVar, deltaH, tht_m, rht_m, dist_km, TSiteCriteria, RSiteCriteria, eps_dielect, sgm_conductivity, eno_ns_surfref, frq_mhz, radio_climate, pol, pctTime, pctLoc, pctConf, out dbloss, out errnum);
            }
        }
    }
}
