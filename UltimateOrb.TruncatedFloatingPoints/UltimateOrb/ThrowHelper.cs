
using System.Diagnostics.CodeAnalysis;

namespace UltimateOrb {
    static partial class ThrowHelper {

        [DoesNotReturn]
        internal static void ThrowNotSupportedException() => throw new NotSupportedException();
    }
}
