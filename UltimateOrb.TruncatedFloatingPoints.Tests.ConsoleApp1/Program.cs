using System.Diagnostics.CodeAnalysis;
using System.Numerics;
using System.Reflection;

namespace UltimateOrb.TruncatedFloatingPoints.Tests.ConsoleApp1 {

    internal class Program {

        static void Main(string[] args) {


            {
                // Define some constants
                const int N = 10; // Number of elements to generate
                const float MIN = -10f; // Minimum value
                const float MAX = 10f; // Maximum value

                // Create a random number generator
                Random rng = new Random();

                // Generate some random float values in the range [MIN, MAX]
                float[] floats = new float[N];
                for (int i = 0; i < N; i++) {
                    floats[i] = (float)rng.NextDouble() * (MAX - MIN) + MIN;
                }

                // Convert the float values to BFloat16 values
                BFloat16[] bfloats = new BFloat16[N];
                for (int i = 0; i < N; i++) {
                    bfloats[i] = BFloat16.CreateTruncating(floats[i]);
                }

                // Print the float and BFloat16 values
                Console.WriteLine("Float values:");
                foreach (float f in floats) {
                    Console.WriteLine(f);
                }
                Console.WriteLine();

                Console.WriteLine("BFloat16 values:");
                foreach (BFloat16 b in bfloats) {
                    Console.WriteLine(b);
                }
                Console.WriteLine();

                // Perform some arithmetic operations on the BFloat16 values
                BFloat16 sum = (BFloat16)0F;
                BFloat16 product = (BFloat16)1F;
                BFloat16 min = BFloat16.MaxValue;
                BFloat16 max = BFloat16.MinValue;

                for (int i = 0; i < N; i++) {
                    sum += bfloats[i];
                    product *= bfloats[i];
                    min = BFloat16.Min(min, bfloats[i]);
                    max = BFloat16.Max(max, bfloats[i]);
                }

                // Print the results
                Console.WriteLine("Sum: " + sum);
                Console.WriteLine("Product: " + product);
                Console.WriteLine("Min: " + min);
                Console.WriteLine("Max: " + max);
                Console.WriteLine();
            }



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
