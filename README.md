# UltimateOrb.TruncatedFloatingPoints

Provides the BFloat16 type and friends.

[![Join the chat at https://gitter.im/UltimateOrb-Working-Group/PublicMain](https://badges.gitter.im/UltimateOrb-Working-Group/PublicMain.svg)](https://gitter.im/UltimateOrb-Working-Group/PublicMain?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

This repo contains the code to build the UltimateOrb.TruncatedFloatingPoints package, as well as the sources to related tools and unit tests.

[![Version](https://img.shields.io/nuget/vpre/UltimateOrb.TruncatedFloatingPoints.svg)](https://www.nuget.org/packages/UltimateOrb.TruncatedFloatingPoints)
[![NuGet download count](https://img.shields.io/nuget/dt/UltimateOrb.TruncatedFloatingPoints.svg)](https://www.nuget.org/packages/UltimateOrb.TruncatedFloatingPoints)

## Example
This is an example of the output of the program, using the `BFloat16` type and some basic arithmetic operations.
```csharp
using System;
using UltimateOrb;

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
```

The following is an example of one possible output:

```
Float values:
-9.158998
-4.69648
-2.9157972
4.6221685
-2.8663998
-9.488923
1.659193
-1.0525465
7.7342377
2.6802025

BFloat16 values:
-9.1875
-4.6875
-2.921875
4.625
-2.859375
-9.5
1.65625
-1.0546875
7.71875
2.6875

Sum: -13.4375
Product: 569344
Min: -9.5
Max: 7.71875

```