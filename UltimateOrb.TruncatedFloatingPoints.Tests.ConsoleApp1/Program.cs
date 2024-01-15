using System.Diagnostics.CodeAnalysis;
using System.Numerics;
using System.Reflection;

namespace UltimateOrb.TruncatedFloatingPoints.Tests.ConsoleApp1 {

    internal class Program {

        static void Main(string[] args) {
            Console.WriteLine($@"{BFloat16.E}");
            Console.WriteLine($@"{BFloat16.Pi}");
            Console.WriteLine($@"{BFloat16.Tau}");
            Console.WriteLine($@"{BFloat16.CreateTruncating(65520U)}");
            Console.WriteLine($@"{Int128.CreateTruncating(BFloat16.CreateTruncating(65520U))}");
            Console.WriteLine($@"{Int128.CreateTruncating(BFloat16.MaxValue)}");

            

            try {
                Console.WriteLine($@"{Int128.CreateChecked(BFloat16.MaxValue)}");
                Console.Error.WriteLine($@"!!! OverflowException not thrown.");
                throw new Exception();
            } catch (ArithmeticException ex) {
                Console.WriteLine(ex.Message);
            }

            Console.WriteLine($@"{BitConverter.UInt32BitsToSingle(0B01111111_01111111_10000000_00000000U):R}");

            Console.WriteLine($@"{(double)(float)Half.MaxValue:R}");

            Console.WriteLine($@"{(double)Half.MaxValue:R}");
            if (true) {
                for (long i = 0; i <= UInt32.MaxValue; ++i) {
                    var f = BitConverter.UInt32BitsToSingle((UInt32)i);
                    var x = (BFloat16)f;
                    var y = (BFloat16)(double)f;
                    if (!x.Equals(y)) {
                        Console.WriteLine($@"!!! {i:x}");
                    }
                }
            }
           
            {
                var f = BitConverter.UInt32BitsToSingle(0Xff800000U);
                Console.WriteLine($@"{f}");
                Console.WriteLine($@"{(BFloat16)f}");
                Console.WriteLine($@"{(BFloat16)(double)f}");

            }

            Console.WriteLine($@"{(BFloat16)(-3.2F)}");
            Console.WriteLine($@"{(BFloat16)(-3.2D)}");

            Console.WriteLine($@"{(BFloat16)0.002F}");
            Console.WriteLine($@"{(BFloat16)0.002D}");

            Console.WriteLine($@"{(BFloat16)float.NaN}");
            Console.WriteLine($@"{(BFloat16)double.NaN}");

            Console.WriteLine($@"{BitConverter.SingleToUInt32Bits((float)BitConverter.UInt16BitsToHalf(0B10000000000)):X}");

            Console.WriteLine($@"{BitConverter.DoubleToUInt64Bits(BitConverter.UInt32BitsToSingle(0X00800000)):X}");

            Console.WriteLine($@"{BitConverter.UInt32BitsToSingle(0X00800000)}");
            Console.WriteLine($@"{(BFloat16)BitConverter.UInt32BitsToSingle(0X00800000)}");

            Console.WriteLine($@"{float.MaxValue}");
            Console.WriteLine($@"{(float)double.MaxValue}");

            Console.WriteLine($@"{BFloat16.MaxValue}");
            Console.WriteLine($@"{(BFloat16)float.MaxValue}");

            Console.WriteLine($@"{float.Epsilon}");
            Console.WriteLine($@"{(float)double.Epsilon}");

            Console.WriteLine($@"{BFloat16.Epsilon}");
            Console.WriteLine($@"{(BFloat16)float.Epsilon}");

        }
    }
}
