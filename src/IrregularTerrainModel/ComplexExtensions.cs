using System.Numerics;

namespace LongleyRice;

static class ComplexExtensions
{
    public static double real(this Complex complex)
    {
        return complex.Real;
    }

    public static double imag(this Complex complex)
    {
        return complex.Imaginary;
    }
}