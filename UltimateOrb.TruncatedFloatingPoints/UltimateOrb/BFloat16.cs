using System.Buffers.Binary;
using System.Diagnostics;
using System.Diagnostics.CodeAnalysis;
using System.Globalization;
using System.Numerics;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

[assembly: CLSCompliant(true)]

namespace UltimateOrb {

    /// <summary>
    /// Represents a 16-bit floating-point number that has the same range as a 32-bit <see cref="Single"></see> number, but less precision.
    /// </summary>
    /// <remarks>
    /// This type does not conform to the IEEE 754 standard for floating-point arithmetic, even though it implements the <see cref="IBinaryFloatingPointIeee754{TSelf}">IBinaryFloatingPointIeee754&lt;BFloat16&gt;</see> interface.
    /// </remarks>
    [CLSCompliant(false)]
    [Serializable]
    [StructLayout(LayoutKind.Sequential)]
    public readonly struct BFloat16
        : IComparable
        , IConvertible
        , ISpanFormattable
        , IComparable<BFloat16>
        , IEquatable<BFloat16>
        , IBinaryFloatingPointIeee754<BFloat16>
        , IMinMaxValue<BFloat16>
        , IUtf8SpanFormattable
        // , IBinaryFloatParseAndFormatInfo<BFloat16>
        {

        readonly UInt16 bits;

        internal BFloat16(UInt16 bits) {
            this.bits = bits;
        }

        readonly Single InternalValue {

            get => BitConverter.Int32BitsToSingle((Int32)bits << 16);
        }

        public static BFloat16 MinValue {

            get => new(0B_11111111_01111111); // -3.3895314E+38
        }

        public static BFloat16 MaxValue {

            get => new(0B_01111111_01111111); // 3.3895314E+38
        }

        public static BFloat16 Epsilon {

            get => new(0B_00000000_00000001);
        }

        public static BFloat16 NegativeInfinity {

            get => new(0B_11111111_10000000);
        }

        public static BFloat16 PositiveInfinity {

            get => new(0B_01111111_10000000);
        }

        public static BFloat16 NaN {

            get => new(0B_11111111_11000000);
        }

        /// <summary>Represents the number negative zero (-0).</summary>
        public static BFloat16 NegativeZero {

            get => new(0B_10000000_00000000);
        }

        /// <summary>Represents the natural logarithmic base, specified by the constant, e.</summary>
        /// <remarks>This is known as Euler's number and is approximately 2.7182818284590452354.</remarks>
        public static BFloat16 E {

            get => new(0X402e);
        }

        /// <summary>Represents the ratio of the circumference of a circle to its diameter, specified by the constant, PI.</summary>
        /// <remarks>Pi is approximately 3.1415926535897932385.</remarks>
        public static BFloat16 Pi {

            get => new(0X4049);
        }

        /// <summary>Represents the number of radians in one turn, specified by the constant, Tau.</summary>
        /// <remarks>Tau is approximately 6.2831853071795864769.</remarks>
        public static BFloat16 Tau {

            get => new(0X40C9);
        }

        //
        // Constants for manipulating the private bit-representation
        //
        internal const uint SignMask = 0x8000;
        internal const int SignShift = 15;
        internal const byte ShiftedSignMask = (byte)(SignMask >> SignShift);

        internal const uint BiasedExponentMask = 0x7F80;
        internal const int BiasedExponentShift = 7;
        internal const byte ShiftedBiasedExponentMask = (byte)(BiasedExponentMask >> BiasedExponentShift);

        internal const uint TrailingSignificandMask = 0x007F;

        internal const byte MinSign = 0;
        internal const byte MaxSign = 1;

        internal const byte MinBiasedExponent = 0x00;
        internal const byte MaxBiasedExponent = 0xFF;

        internal const byte ExponentBias = 127;

        internal const sbyte MinExponent = -126;
        internal const sbyte MaxExponent = +127;

        internal const uint MinTrailingSignificand = 0x0000;
        internal const uint MaxTrailingSignificand = 0x007F;

        internal const int TrailingSignificandLength = 7;
        internal const int SignificandLength = TrailingSignificandLength + 1;

        internal const int PositiveInfinityBits = 0x7F80;
        internal const int NegativeInfinityBits = 0xFF80;
        internal const int SmallestNormalBits = 0x0080;


        internal byte BiasedExponent {
            get {
                return ExtractBiasedExponentFromBits(bits);
            }
        }

        internal sbyte Exponent {
            get {
                return (sbyte)(BiasedExponent - ExponentBias);
            }
        }

        internal uint Significand {
            get {
                return TrailingSignificand | ((BiasedExponent != 0) ? (1U << BiasedExponentShift) : 0U);
            }
        }

        internal uint TrailingSignificand {
            get {
                return ExtractTrailingSignificandFromBits(bits);
            }
        }

        internal static byte ExtractBiasedExponentFromBits(uint bits) {
            return (byte)((bits >> BiasedExponentShift) & ShiftedBiasedExponentMask);
        }

        internal static uint ExtractTrailingSignificandFromBits(uint bits) {
            return bits & TrailingSignificandMask;
        }

        /// <summary>Determines whether the specified f is finite (zero, subnormal, or normal).</summary>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static bool IsFinite(BFloat16 f) {
            return (f.bits & 0x7FFF) < 0x7F80;
        }

        /// <summary>Determines whether the specified f is infinite.</summary>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static bool IsInfinity(BFloat16 f) {
            return (f.bits & 0x7FFF) == 0x7F80;
        }

        /// <summary>Determines whether the specified f is NaN.</summary>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static bool IsNaN(BFloat16 f) {
            return (f.bits & 0x7FFF) > 0x7F80;
        }

        /// <summary>Determines whether the specified f is negative.</summary>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static bool IsNegative(BFloat16 f) {
            return unchecked((Int16)f.bits) < 0;
        }

        /// <summary>Determines whether the specified f is negative infinity.</summary>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static bool IsNegativeInfinity(BFloat16 f) {
            return f.bits == NegativeInfinityBits;
        }

        /// <summary>Determines whether the specified f is normal.</summary>
        // This is probably not worth inlining, it has branches and should be rarely called
        public static bool IsNormal(BFloat16 f) {
            int bits = f.bits;
            bits &= 0x7FFF;
            return (bits < 0x7F80) && (bits != 0) && ((bits & 0x7F80) != 0);
        }

        /// <summary>Determines whether the specified f is positive infinity.</summary>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static bool IsPositiveInfinity(BFloat16 f) {
            return f.bits == PositiveInfinityBits;
        }

        /// <summary>Determines whether the specified f is subnormal.</summary>
        // This is probably not worth inlining, it has branches and should be rarely called
        public static bool IsSubnormal(BFloat16 f) {
            int bits = f.bits;
            bits &= 0x7FFF;
            return (bits < 0x7F80) && (bits != 0) && ((bits & 0x7F80) == 0);
        }

        // Compares this object to another object, returning an integer that
        // indicates the relationship.
        // Returns a f less than zero if this  object
        // null is considered to be less than any instance.
        // If object is not of type BFloat16, this method throws an ArgumentException.
        //
        public int CompareTo(object? value) {
            if (value == null) {
                return 1;
            }

            if (value is BFloat16 f) {
                var x = InternalValue;
                var y = f.InternalValue;
                return x.CompareTo(y);
            }

            throw new ArgumentException(SR.Arg_MustBeBFloat16);
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public int CompareTo(BFloat16 value) {
            var x = InternalValue;
            var y = value.InternalValue;
            return x.CompareTo(y);
        }


        /// <inheritdoc cref="IEqualityOperators{TSelf, TOther, TResult}.op_Equality(TSelf, TOther)" />
        public static bool operator ==(BFloat16 left, BFloat16 right) => left.InternalValue == right.InternalValue;

        /// <inheritdoc cref="IEqualityOperators{TSelf, TOther, TResult}.op_Inequality(TSelf, TOther)" />
        public static bool operator !=(BFloat16 left, BFloat16 right) => left.InternalValue != right.InternalValue;

        /// <inheritdoc cref="IComparisonOperators{TSelf, TOther, TResult}.op_LessThan(TSelf, TOther)" />
        public static bool operator <(BFloat16 left, BFloat16 right) => left.InternalValue < right.InternalValue;

        /// <inheritdoc cref="IComparisonOperators{TSelf, TOther, TResult}.op_GreaterThan(TSelf, TOther)" />
        public static bool operator >(BFloat16 left, BFloat16 right) => left.InternalValue > right.InternalValue;

        /// <inheritdoc cref="IComparisonOperators{TSelf, TOther, TResult}.op_LessThanOrEqual(TSelf, TOther)" />
        public static bool operator <=(BFloat16 left, BFloat16 right) => left.InternalValue <= right.InternalValue;

        /// <inheritdoc cref="IComparisonOperators{TSelf, TOther, TResult}.op_GreaterThanOrEqual(TSelf, TOther)" />
        public static bool operator >=(BFloat16 left, BFloat16 right) => left.InternalValue >= right.InternalValue;


        public override bool Equals([NotNullWhen(true)] object? obj) {
            return obj is BFloat16 f && Equals(f);
        }

        public bool Equals(BFloat16 obj) {
            var x = InternalValue;
            var y = obj.InternalValue;
            return x.Equals(y);
        }


        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public override int GetHashCode() {
            var bits = (int)this.bits;

            // Optimized check for IsNan() || IsZero()
            if (((bits - 1) & 0x7FFF) >= 0x7F80) {
                // Ensure that all NaNs and both zeros have the same hash code
                bits &= 0x7F80;
            }

            return bits;
        }

        public override string ToString() {
            return InternalValue.ToString();
        }

        public string ToString(IFormatProvider? provider) {
            return InternalValue.ToString(provider);
        }

        // TODO: Fix formatting
        public string ToString([StringSyntax(StringSyntaxAttribute.NumericFormat)] string? format) {

            return InternalValue.ToString(format);
        }

        // TODO: Fix formatting
        public string ToString([StringSyntax(StringSyntaxAttribute.NumericFormat)] string? format, IFormatProvider? provider) {
            return InternalValue.ToString(format, provider);
        }

        // TODO: Fix formatting
        public bool TryFormat(Span<char> destination, out int charsWritten, [StringSyntax(StringSyntaxAttribute.NumericFormat)] ReadOnlySpan<char> format = default, IFormatProvider? provider = null) {
            return InternalValue.TryFormat(destination, out charsWritten, format, provider);
        }

        // TODO: Fix formatting
        /// <inheritdoc cref="IUtf8SpanFormattable.TryFormat" />
        public bool TryFormat(Span<byte> utf8Destination, out int bytesWritten, [StringSyntax(StringSyntaxAttribute.NumericFormat)] ReadOnlySpan<char> format = default, IFormatProvider? provider = null) {
            return InternalValue.TryFormat(utf8Destination, out bytesWritten, format, provider);
        }

        // Parses a float from a String in the given style.  If
        // a NumberFormatInfo isn't specified, the current culture's
        // NumberFormatInfo is assumed.
        //
        // This method will not throw an OverflowException, but will return
        // PositiveInfinity or NegativeInfinity for a number that is too
        // large or too small.
        //
        public static BFloat16 Parse(string s) => Parse(s, NumberStyles.Float | NumberStyles.AllowThousands, provider: null);

        public static BFloat16 Parse(string s, NumberStyles style) => Parse(s, style, provider: null);

        public static BFloat16 Parse(string s, IFormatProvider? provider) => Parse(s, NumberStyles.Float | NumberStyles.AllowThousands, provider);

        public static BFloat16 Parse(string s, NumberStyles style, IFormatProvider? provider) {
            ArgumentNullException.ThrowIfNull(nameof(s));
            return Parse(s.AsSpan(), style, provider);
        }

        public static BFloat16 Parse(ReadOnlySpan<char> s, NumberStyles style = NumberStyles.Float | NumberStyles.AllowThousands, IFormatProvider? provider = null) {
            return (BFloat16)float.Parse(s, style, provider);
        }

        public static bool TryParse([NotNullWhen(true)] string? s, out BFloat16 result) => TryParse(s, NumberStyles.Float | NumberStyles.AllowThousands, provider: null, out result);

        public static bool TryParse(ReadOnlySpan<char> s, out BFloat16 result) => TryParse(s, NumberStyles.Float | NumberStyles.AllowThousands, provider: null, out result);

        /// <summary>Tries to convert a UTF-8 character span containing the string representation of a number to its single-precision floating-point number equivalent.</summary>
        /// <param name="utf8Text">A read-only UTF-8 character span that contains the number to convert.</param>
        /// <param name="result">When this method returns, contains a single-precision floating-point number equivalent of the numeric f or symbol contained in <paramref name="utf8Text" /> if the conversion succeeded or zero if the conversion failed. The conversion fails if the <paramref name="utf8Text" /> is <see cref="ReadOnlySpan{T}.Empty" /> or is not in a valid format. This parameter is passed uninitialized; any f originally supplied in result will be overwritten.</param>
        /// <returns><c>true</c> if <paramref name="utf8Text" /> was converted successfully; otherwise, false.</returns>
        public static bool TryParse(ReadOnlySpan<byte> utf8Text, out BFloat16 result) => TryParse(utf8Text, NumberStyles.Float | NumberStyles.AllowThousands, provider: null, out result);

        public static bool TryParse([NotNullWhen(true)] string? s, NumberStyles style, IFormatProvider? provider, out BFloat16 result) {
            if (float.TryParse(s, style, provider, out var f)) {
                result = (BFloat16)f;
                return true;
            }
            result = default;
            return false;
        }

        public static bool TryParse(ReadOnlySpan<char> s, NumberStyles style, IFormatProvider? provider, out BFloat16 result) {
            if (float.TryParse(s, style, provider, out var f)) {
                result = (BFloat16)f;
                return true;
            }
            result = default;
            return false;
        }

        //
        // IConvertible implementation
        //

        public TypeCode GetTypeCode() {
            return TypeCode.Object;
        }

        bool IConvertible.ToBoolean(IFormatProvider? provider) {
            return Convert.ToBoolean(InternalValue);
        }

        char IConvertible.ToChar(IFormatProvider? provider) {
            throw new InvalidCastException(SR.Format(SR.InvalidCast_FromTo, "BFloat16", "Char"));
        }

        sbyte IConvertible.ToSByte(IFormatProvider? provider) {
            return Convert.ToSByte(InternalValue);
        }

        byte IConvertible.ToByte(IFormatProvider? provider) {
            return Convert.ToByte(InternalValue);
        }

        short IConvertible.ToInt16(IFormatProvider? provider) {
            return Convert.ToInt16(InternalValue);
        }

        ushort IConvertible.ToUInt16(IFormatProvider? provider) {
            return Convert.ToUInt16(InternalValue);
        }

        int IConvertible.ToInt32(IFormatProvider? provider) {
            return Convert.ToInt32(InternalValue);
        }

        uint IConvertible.ToUInt32(IFormatProvider? provider) {
            return Convert.ToUInt32(InternalValue);
        }

        long IConvertible.ToInt64(IFormatProvider? provider) {
            return Convert.ToInt64(InternalValue);
        }

        ulong IConvertible.ToUInt64(IFormatProvider? provider) {
            return Convert.ToUInt64(InternalValue);
        }

        float IConvertible.ToSingle(IFormatProvider? provider) {
            return InternalValue;
        }

        double IConvertible.ToDouble(IFormatProvider? provider) {
            return Convert.ToDouble(InternalValue);
        }

        decimal IConvertible.ToDecimal(IFormatProvider? provider) {
            return Convert.ToDecimal(InternalValue);
        }

        DateTime IConvertible.ToDateTime(IFormatProvider? provider) {
            throw new InvalidCastException(SR.Format(SR.InvalidCast_FromTo, "BFloat16", "DateTime"));
        }

        object IConvertible.ToType(Type type, IFormatProvider? provider) {
            if (type == typeof(BFloat16)) {
                return this;
            }
            return Convert_DefaultToType((IConvertible)this, type, provider);
        }

        internal static object Convert_DefaultToType(IConvertible value, Type targetType, IFormatProvider? provider) {
            ArgumentNullException.ThrowIfNull(targetType);

            Debug.Assert(value != null, "[Convert.DefaultToType]value!=null");

            if (ReferenceEquals(value.GetType(), targetType)) {
                return value;
            }

            if (ReferenceEquals(targetType, typeof(bool)))
                return value.ToBoolean(provider);
            if (ReferenceEquals(targetType, typeof(char)))
                return value.ToChar(provider);
            if (ReferenceEquals(targetType, typeof(sbyte)))
                return value.ToSByte(provider);
            if (ReferenceEquals(targetType, typeof(byte)))
                return value.ToByte(provider);
            if (ReferenceEquals(targetType, typeof(short)))
                return value.ToInt16(provider);
            if (ReferenceEquals(targetType, typeof(ushort)))
                return value.ToUInt16(provider);
            if (ReferenceEquals(targetType, typeof(int)))
                return value.ToInt32(provider);
            if (ReferenceEquals(targetType, typeof(uint)))
                return value.ToUInt32(provider);
            if (ReferenceEquals(targetType, typeof(long)))
                return value.ToInt64(provider);
            if (ReferenceEquals(targetType, typeof(ulong)))
                return value.ToUInt64(provider);
            if (ReferenceEquals(targetType, typeof(float)))
                return value.ToSingle(provider);
            if (ReferenceEquals(targetType, typeof(double)))
                return value.ToDouble(provider);
            if (ReferenceEquals(targetType, typeof(decimal)))
                return value.ToDecimal(provider);
            if (ReferenceEquals(targetType, typeof(DateTime)))
                return value.ToDateTime(provider);
            if (ReferenceEquals(targetType, typeof(string)))
                return value.ToString(provider);
            if (ReferenceEquals(targetType, typeof(object)))
                return (object)value;
            // Need to special case Enum because typecode will be underlying type, e.g. Int32
            if (ReferenceEquals(targetType, typeof(Enum)))
                return (Enum)value;
            if (ReferenceEquals(targetType, typeof(DBNull)))
                throw new InvalidCastException(SR.InvalidCast_DBNull);
            if (targetType.FullName == "System.Empty"/*ReferenceEquals(targetType, typeof(Empty))*/)
                throw new InvalidCastException(SR.InvalidCast_Empty);

            throw new InvalidCastException(SR.Format(SR.InvalidCast_FromTo, value.GetType().FullName, targetType.FullName));
        }

        //
        // IAdditionOperators
        //

        /// <inheritdoc cref="IAdditionOperators{TSelf, TOther, TResult}.op_Addition(TSelf, TOther)" />
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        static BFloat16 IAdditionOperators<BFloat16, BFloat16, BFloat16>.operator +(BFloat16 left, BFloat16 right) => (BFloat16)(left + right);

        //
        // IAdditiveIdentity
        //

        /// <inheritdoc cref="IAdditiveIdentity{TSelf, TResult}.AdditiveIdentity" />
        static BFloat16 IAdditiveIdentity<BFloat16, BFloat16>.AdditiveIdentity => default;

        //
        // IBinaryNumber
        //

        /// <inheritdoc cref="IBinaryNumber{TSelf}.AllBitsSet" />
        static BFloat16 IBinaryNumber<BFloat16>.AllBitsSet => new(0xFFFF);

        /// <inheritdoc cref="IBinaryNumber{TSelf}.IsPow2(TSelf)" />
        public static bool IsPow2(BFloat16 value) {
            uint bits = value.bits;

            if ((int)bits <= 0) {
                // Zero and negative values cannot be powers of 2
                return false;
            }

            byte biasedExponent = ExtractBiasedExponentFromBits(bits);
            uint trailingSignificand = ExtractTrailingSignificandFromBits(bits);

            if (biasedExponent == MinBiasedExponent) {
                // Subnormal values have 1 bit set when they're powers of 2
                return uint.PopCount(trailingSignificand) == 1;
            } else if (biasedExponent == MaxBiasedExponent) {
                // NaN and Infinite values cannot be powers of 2
                return false;
            }

            // Normal values have 0 bits set when they're powers of 2
            return trailingSignificand == MinTrailingSignificand;
        }

        /// <inheritdoc cref="IBinaryNumber{TSelf}.Log2(TSelf)" />
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static BFloat16 Log2(BFloat16 value) => (BFloat16)MathF.Log2(value);

        //
        // IBitwiseOperators
        //

        /// <inheritdoc cref="IBitwiseOperators{TSelf, TOther, TResult}.op_BitwiseAnd(TSelf, TOther)" />
        static BFloat16 IBitwiseOperators<BFloat16, BFloat16, BFloat16>.operator &(BFloat16 left, BFloat16 right) {
            return new(unchecked((UInt16)(left.bits & right.bits)));
        }

        /// <inheritdoc cref="IBitwiseOperators{TSelf, TOther, TResult}.op_BitwiseOr(TSelf, TOther)" />
        static BFloat16 IBitwiseOperators<BFloat16, BFloat16, BFloat16>.operator |(BFloat16 left, BFloat16 right) {
            return new(unchecked((UInt16)(left.bits | right.bits)));
        }

        /// <inheritdoc cref="IBitwiseOperators{TSelf, TOther, TResult}.op_ExclusiveOr(TSelf, TOther)" />
        static BFloat16 IBitwiseOperators<BFloat16, BFloat16, BFloat16>.operator ^(BFloat16 left, BFloat16 right) {
            return new(unchecked((UInt16)(left.bits ^ right.bits)));
        }

        /// <inheritdoc cref="IBitwiseOperators{TSelf, TOther, TResult}.op_OnesComplement(TSelf)" />
        static BFloat16 IBitwiseOperators<BFloat16, BFloat16, BFloat16>.operator ~(BFloat16 value) {
            return new(unchecked((UInt16)(~value.bits)));
        }

        //
        // IDecrementOperators
        //

        /// <inheritdoc cref="IDecrementOperators{TSelf}.op_Decrement(TSelf)" />
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        static BFloat16 IDecrementOperators<BFloat16>.operator --(BFloat16 value) {
            var f = value.InternalValue;
            --f;
            return (BFloat16)f;
        }

        //
        // IDivisionOperators
        //

        /// <inheritdoc cref="IDivisionOperators{TSelf, TOther, TResult}.op_Division(TSelf, TOther)" />
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        static BFloat16 IDivisionOperators<BFloat16, BFloat16, BFloat16>.operator /(BFloat16 left, BFloat16 right) => (BFloat16)(left / right);

        //
        // IExponentialFunctions
        //

        /// <inheritdoc cref="IExponentialFunctions{TSelf}.Exp" />
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static BFloat16 Exp(BFloat16 x) => (BFloat16)MathF.Exp(x);

        /// <inheritdoc cref="IExponentialFunctions{TSelf}.ExpM1(TSelf)" />
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static BFloat16 ExpM1(BFloat16 x) => (BFloat16)(MathF.Exp(x) - 1);

        /// <inheritdoc cref="IExponentialFunctions{TSelf}.Exp2(TSelf)" />
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static BFloat16 Exp2(BFloat16 x) => (BFloat16)MathF.Pow(2, x);

        /// <inheritdoc cref="IExponentialFunctions{TSelf}.Exp2M1(TSelf)" />
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static BFloat16 Exp2M1(BFloat16 x) => (BFloat16)(MathF.Pow(2, x) - 1);

        /// <inheritdoc cref="IExponentialFunctions{TSelf}.Exp10(TSelf)" />
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static BFloat16 Exp10(BFloat16 x) => (BFloat16)MathF.Pow(10, x);

        /// <inheritdoc cref="IExponentialFunctions{TSelf}.Exp10M1(TSelf)" />
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static BFloat16 Exp10M1(BFloat16 x) => (BFloat16)(MathF.Pow(10, x) - 1);

        //
        // IFloatingPoint
        //

        // TODO: Double rounding
        /// <inheritdoc cref="IFloatingPoint{TSelf}.Ceiling(TSelf)" />
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static BFloat16 Ceiling(BFloat16 x) => (BFloat16)MathF.Ceiling(x);

        // TODO: Double rounding
        /// <inheritdoc cref="IFloatingPoint{TSelf}.Floor(TSelf)" />
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static BFloat16 Floor(BFloat16 x) => (BFloat16)MathF.Floor(x);

        // TODO: Double rounding
        /// <inheritdoc cref="IFloatingPoint{TSelf}.Round(TSelf)" />
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static BFloat16 Round(BFloat16 x) => (BFloat16)MathF.Round(x);

        // TODO: Double rounding
        /// <inheritdoc cref="IFloatingPoint{TSelf}.Round(TSelf, int)" />
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static BFloat16 Round(BFloat16 x, int digits) => (BFloat16)MathF.Round(x, digits);

        // TODO: Double rounding
        /// <inheritdoc cref="IFloatingPoint{TSelf}.Round(TSelf, MidpointRounding)" />
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static BFloat16 Round(BFloat16 x, MidpointRounding mode) => (BFloat16)MathF.Round(x, mode);

        // TODO: Double rounding
        /// <inheritdoc cref="IFloatingPoint{TSelf}.Round(TSelf, MidpointRounding)" />
        /// <inheritdoc cref="IFloatingPoint{TSelf}.Round(TSelf, int, MidpointRounding)" />
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static BFloat16 Round(BFloat16 x, int digits, MidpointRounding mode) => (BFloat16)MathF.Round(x, digits, mode);

        // TODO: Double rounding
        /// <inheritdoc cref="IFloatingPoint{TSelf}.Truncate(TSelf)" />
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static BFloat16 Truncate(BFloat16 x) => (BFloat16)MathF.Truncate(x);

        /// <inheritdoc cref="IFloatingPoint{TSelf}.GetExponentByteCount()" />
        int IFloatingPoint<BFloat16>.GetExponentByteCount() => sizeof(sbyte);

        /// <inheritdoc cref="IFloatingPoint{TSelf}.GetExponentShortestBitLength()" />
        int IFloatingPoint<BFloat16>.GetExponentShortestBitLength() {
            sbyte exponent = Exponent;

            if (exponent >= 0) {
                return (sizeof(sbyte) * 8) - sbyte.LeadingZeroCount(exponent);
            } else {
                return (sizeof(sbyte) * 8) + 1 - sbyte.LeadingZeroCount((sbyte)(~exponent));
            }
        }

        /// <inheritdoc cref="IFloatingPoint{TSelf}.GetSignificandByteCount()" />
        int IFloatingPoint<BFloat16>.GetSignificandByteCount() => sizeof(sbyte);

        /// <inheritdoc cref="IFloatingPoint{TSelf}.GetSignificandBitLength()" />
        int IFloatingPoint<BFloat16>.GetSignificandBitLength() => 8;

        /// <inheritdoc cref="IFloatingPoint{TSelf}.TryWriteExponentBigEndian(Span{byte}, out int)" />
        bool IFloatingPoint<BFloat16>.TryWriteExponentBigEndian(Span<byte> destination, out int bytesWritten) {
            if (destination.Length >= sizeof(sbyte)) {
                sbyte exponent = Exponent;
                Unsafe.WriteUnaligned(ref MemoryMarshal.GetReference(destination), exponent);

                bytesWritten = sizeof(sbyte);
                return true;
            } else {
                bytesWritten = 0;
                return false;
            }
        }

        /// <inheritdoc cref="IFloatingPoint{TSelf}.TryWriteExponentLittleEndian(Span{byte}, out int)" />
        bool IFloatingPoint<BFloat16>.TryWriteExponentLittleEndian(Span<byte> destination, out int bytesWritten) {
            if (destination.Length >= sizeof(sbyte)) {
                sbyte exponent = Exponent;
                Unsafe.WriteUnaligned(ref MemoryMarshal.GetReference(destination), exponent);

                bytesWritten = sizeof(sbyte);
                return true;
            } else {
                bytesWritten = 0;
                return false;
            }
        }

        /// <inheritdoc cref="IFloatingPoint{TSelf}.TryWriteSignificandBigEndian(Span{byte}, out int)" />
        bool IFloatingPoint<BFloat16>.TryWriteSignificandBigEndian(Span<byte> destination, out int bytesWritten) {
            if (destination.Length >= sizeof(byte)) {
                byte significand = checked((byte)Significand);

                if (BitConverter.IsLittleEndian) {
                    significand = BinaryPrimitives.ReverseEndianness(significand);
                }

                Unsafe.WriteUnaligned(ref MemoryMarshal.GetReference(destination), significand);

                bytesWritten = sizeof(byte);
                return true;
            } else {
                bytesWritten = 0;
                return false;
            }
        }

        /// <inheritdoc cref="IFloatingPoint{TSelf}.TryWriteSignificandLittleEndian(Span{byte}, out int)" />
        bool IFloatingPoint<BFloat16>.TryWriteSignificandLittleEndian(Span<byte> destination, out int bytesWritten) {
            if (destination.Length >= sizeof(byte)) {
                byte significand = checked((byte)Significand);

                if (!BitConverter.IsLittleEndian) {
                    significand = BinaryPrimitives.ReverseEndianness(significand);
                }

                Unsafe.WriteUnaligned(ref MemoryMarshal.GetReference(destination), significand);

                bytesWritten = sizeof(byte);
                return true;
            } else {
                bytesWritten = 0;
                return false;
            }
        }

        //
        // IFloatingPointConstants
        //

        /// <inheritdoc cref="IFloatingPointConstants{TSelf}.E" />
        static BFloat16 IFloatingPointConstants<BFloat16>.E => E;

        /// <inheritdoc cref="IFloatingPointConstants{TSelf}.Pi" />
        static BFloat16 IFloatingPointConstants<BFloat16>.Pi => Pi;

        /// <inheritdoc cref="IFloatingPointConstants{TSelf}.Tau" />
        static BFloat16 IFloatingPointConstants<BFloat16>.Tau => Tau;

        //
        // IFloatingPointIeee754
        //

        /// <inheritdoc cref="IFloatingPointIeee754{TSelf}.Epsilon" />
        static BFloat16 IFloatingPointIeee754<BFloat16>.Epsilon => Epsilon;

        /// <inheritdoc cref="IFloatingPointIeee754{TSelf}.NaN" />
        static BFloat16 IFloatingPointIeee754<BFloat16>.NaN => NaN;

        /// <inheritdoc cref="IFloatingPointIeee754{TSelf}.NegativeInfinity" />
        static BFloat16 IFloatingPointIeee754<BFloat16>.NegativeInfinity => NegativeInfinity;

        /// <inheritdoc cref="IFloatingPointIeee754{TSelf}.NegativeZero" />
        static BFloat16 IFloatingPointIeee754<BFloat16>.NegativeZero => NegativeZero;

        /// <inheritdoc cref="IFloatingPointIeee754{TSelf}.PositiveInfinity" />
        static BFloat16 IFloatingPointIeee754<BFloat16>.PositiveInfinity => PositiveInfinity;

        /// <inheritdoc cref="IFloatingPointIeee754{TSelf}.Atan2(TSelf, TSelf)" />
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static BFloat16 Atan2(BFloat16 y, BFloat16 x) => (BFloat16)MathF.Atan2(y, x);

        /// <inheritdoc cref="IFloatingPointIeee754{TSelf}.Atan2Pi(TSelf, TSelf)" />
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static BFloat16 Atan2Pi(BFloat16 y, BFloat16 x) => (BFloat16)(Atan2(y, x) / float.Pi);

        /// <inheritdoc cref="IFloatingPointIeee754{TSelf}.BitDecrement(TSelf)" />
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static BFloat16 BitDecrement(BFloat16 x) {
            int bits = unchecked((Int16)x.bits);

            if ((bits & 0x7F80) >= 0x7F80) {
                // NaN returns NaN
                // -Infinity returns -Infinity
                // +Infinity returns BFloat16.MaxValue
                return (bits == 0x7F80) ? BFloat16.MaxValue : x;
            }

            if (bits == 0x0000) {
                // +0.0 returns -BFloat16.Epsilon
                return new(0B_10000000_00000001);
            }

            // Negative values need to be incremented
            // Positive values need to be decremented

            unchecked {
                bits += ((bits < 0) ? +1 : -1);
            }
            return new(unchecked((UInt16)bits));
        }

        /// <inheritdoc cref="IFloatingPointIeee754{TSelf}.BitIncrement(TSelf)" />
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static BFloat16 BitIncrement(BFloat16 x) {
            int bits = unchecked((Int16)x.bits);

            if ((bits & 0x7F80) >= 0x7F80) {
                // NaN returns NaN
                // -Infinity returns BFloat16.MinValue
                // +Infinity returns +Infinity
                return (bits == 0xFF80) ? BFloat16.MinValue : x;
            }

            if (bits == 0x8000) {
                // -0.0 returns BFloat16.Epsilon
                return Epsilon;
            }

            // Negative values need to be decremented
            // Positive values need to be incremented

            unchecked {
                bits += ((bits < 0) ? -1 : +1);
            }
            return new(unchecked((UInt16)bits));
        }

        /// <inheritdoc cref="IFloatingPointIeee754{TSelf}.FusedMultiplyAdd(TSelf, TSelf, TSelf)" />
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static BFloat16 FusedMultiplyAdd(BFloat16 left, BFloat16 right, BFloat16 addend) => (BFloat16)MathF.FusedMultiplyAdd(left, right, addend);

        /// <inheritdoc cref="IFloatingPointIeee754{TSelf}.Ieee754Remainder(TSelf, TSelf)" />
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static BFloat16 Ieee754Remainder(BFloat16 left, BFloat16 right) => (BFloat16)MathF.IEEERemainder(left, right);

        /// <inheritdoc cref="IFloatingPointIeee754{TSelf}.ILogB(TSelf)" />
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static int ILogB(BFloat16 x) => MathF.ILogB(x);

        /// <inheritdoc cref="IFloatingPointIeee754{TSelf}.Lerp(TSelf, TSelf, TSelf)" />
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static BFloat16 Lerp(BFloat16 value1, BFloat16 value2, BFloat16 amount) => (BFloat16)((value1 * (1.0F - amount)) + (value2 * amount));

        /// <inheritdoc cref="IFloatingPointIeee754{TSelf}.ReciprocalEstimate(TSelf)" />
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static BFloat16 ReciprocalEstimate(BFloat16 x) => (BFloat16)MathF.ReciprocalEstimate(x);

        /// <inheritdoc cref="IFloatingPointIeee754{TSelf}.ReciprocalSqrtEstimate(TSelf)" />
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static BFloat16 ReciprocalSqrtEstimate(BFloat16 x) => (BFloat16)MathF.ReciprocalSqrtEstimate(x);

        // TODO: Performance
        /// <inheritdoc cref="IFloatingPointIeee754{TSelf}.ScaleB(TSelf, int)" />
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static BFloat16 ScaleB(BFloat16 x, int n) => (BFloat16)MathF.ScaleB(x, n);

        // /// <inheritdoc cref="IFloatingPointIeee754{TSelf}.Compound(TSelf, TSelf)" />
        // public static float Compound(float x, float n) => MathF.Compound(x, n);

        //
        // IHyperbolicFunctions
        //

        /// <inheritdoc cref="IHyperbolicFunctions{TSelf}.Acosh(TSelf)" />
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static BFloat16 Acosh(BFloat16 x) => (BFloat16)MathF.Acosh(x);

        /// <inheritdoc cref="IHyperbolicFunctions{TSelf}.Asinh(TSelf)" />
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static BFloat16 Asinh(BFloat16 x) => (BFloat16)MathF.Asinh(x);

        /// <inheritdoc cref="IHyperbolicFunctions{TSelf}.Atanh(TSelf)" />
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static BFloat16 Atanh(BFloat16 x) => (BFloat16)MathF.Atanh(x);

        /// <inheritdoc cref="IHyperbolicFunctions{TSelf}.Cosh(TSelf)" />
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static BFloat16 Cosh(BFloat16 x) => (BFloat16)MathF.Cosh(x);

        /// <inheritdoc cref="IHyperbolicFunctions{TSelf}.Sinh(TSelf)" />
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static BFloat16 Sinh(BFloat16 x) => (BFloat16)MathF.Sinh(x);

        /// <inheritdoc cref="IHyperbolicFunctions{TSelf}.Tanh(TSelf)" />
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static BFloat16 Tanh(BFloat16 x) => (BFloat16)MathF.Tanh(x);

        //
        // IIncrementOperators
        //

        /// <inheritdoc cref="IIncrementOperators{TSelf}.op_Increment(TSelf)" />
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        static BFloat16 IIncrementOperators<BFloat16>.operator ++(BFloat16 value) {
            var f = value.InternalValue;
            ++f;
            return (BFloat16)f;
        }

        //
        // ILogarithmicFunctions
        //

        /// <inheritdoc cref="ILogarithmicFunctions{TSelf}.Log(TSelf)" />
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static BFloat16 Log(BFloat16 x) => (BFloat16)MathF.Log(x);

        /// <inheritdoc cref="ILogarithmicFunctions{TSelf}.Log(TSelf, TSelf)" />
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static BFloat16 Log(BFloat16 x, BFloat16 newBase) => (BFloat16)MathF.Log(x, newBase);

        /// <inheritdoc cref="ILogarithmicFunctions{TSelf}.LogP1(TSelf)" />
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static BFloat16 LogP1(BFloat16 x) => (BFloat16)MathF.Log(x + 1);

        /// <inheritdoc cref="ILogarithmicFunctions{TSelf}.Log10(TSelf)" />
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static BFloat16 Log10(BFloat16 x) => (BFloat16)MathF.Log10(x);

        /// <inheritdoc cref="ILogarithmicFunctions{TSelf}.Log2P1(TSelf)" />
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static BFloat16 Log2P1(BFloat16 x) => (BFloat16)MathF.Log2(x + 1);

        /// <inheritdoc cref="ILogarithmicFunctions{TSelf}.Log10P1(TSelf)" />
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static BFloat16 Log10P1(BFloat16 x) => (BFloat16)MathF.Log10(x + 1);

        //
        // IMinMaxValue
        //

        /// <inheritdoc cref="IMinMaxValue{TSelf}.MinValue" />
        static BFloat16 IMinMaxValue<BFloat16>.MinValue => MinValue;

        /// <inheritdoc cref="IMinMaxValue{TSelf}.MaxValue" />
        static BFloat16 IMinMaxValue<BFloat16>.MaxValue => MaxValue;

        //
        // IModulusOperators
        //

        /// <inheritdoc cref="IModulusOperators{TSelf, TOther, TResult}.op_Modulus(TSelf, TOther)" />
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        static BFloat16 IModulusOperators<BFloat16, BFloat16, BFloat16>.operator %(BFloat16 left, BFloat16 right) => (BFloat16)(left % right);

        //
        // IMultiplicativeIdentity
        //

        /// <inheritdoc cref="IMultiplicativeIdentity{TSelf, TResult}.MultiplicativeIdentity" />
        static BFloat16 IMultiplicativeIdentity<BFloat16, BFloat16>.MultiplicativeIdentity => new(0X3f80); // 1.0

        //
        // IMultiplyOperators
        //

        /// <inheritdoc cref="IMultiplyOperators{TSelf, TOther, TResult}.op_Multiply(TSelf, TOther)" />
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        static BFloat16 IMultiplyOperators<BFloat16, BFloat16, BFloat16>.operator *(BFloat16 left, BFloat16 right) => (BFloat16)(left * right);

        //
        // INumber
        //

        /// <inheritdoc cref="INumber{TSelf}.Clamp(TSelf, TSelf, TSelf)" />
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static BFloat16 Clamp(BFloat16 value, BFloat16 min, BFloat16 max) => (BFloat16)Math.Clamp(value, min, max);

        /// <inheritdoc cref="INumber{TSelf}.CopySign(TSelf, TSelf)" />
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static BFloat16 CopySign(BFloat16 value, BFloat16 sign) => (BFloat16)MathF.CopySign(value, sign);

        /// <inheritdoc cref="INumber{TSelf}.Max(TSelf, TSelf)" />
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static BFloat16 Max(BFloat16 x, BFloat16 y) => (BFloat16)MathF.Max(x, y);

        /// <inheritdoc cref="INumber{TSelf}.MaxNumber(TSelf, TSelf)" />
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static BFloat16 MaxNumber(BFloat16 x, BFloat16 y) {
            // This matches the IEEE 754:2019 `maximumNumber` function
            //
            // It does not propagate NaN inputs back to the caller and
            // otherwise returns the larger of the inputs. It
            // treats +0 as larger than -0 as per the specification.

            if (x != y) {
                if (!IsNaN(y)) {
                    return y < x ? x : y;
                }

                return x;
            }

            return IsNegative(y) ? x : y;
        }

        /// <inheritdoc cref="INumber{TSelf}.Min(TSelf, TSelf)" />
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static BFloat16 Min(BFloat16 x, BFloat16 y) {
            // This matches the IEEE 754:2019 `minimum` function
            //
            // It propagates NaN inputs back to the caller and
            // otherwise returns the lesser of the inputs. It
            // treats +0 as greater than -0 as per the specification.

            if (x != y) {
                if (!IsNaN(x)) {
                    return x < y ? x : y;
                }

                return x;
            }

            return IsNegative(x) ? x : y;
        }

        /// <inheritdoc cref="INumber{TSelf}.MinNumber(TSelf, TSelf)" />
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static BFloat16 MinNumber(BFloat16 x, BFloat16 y) {
            // This matches the IEEE 754:2019 `minimumNumber` function
            //
            // It does not propagate NaN inputs back to the caller and
            // otherwise returns the larger of the inputs. It
            // treats +0 as larger than -0 as per the specification.

            if (x != y) {
                if (!IsNaN(y)) {
                    return x < y ? x : y;
                }

                return x;
            }

            return IsNegative(x) ? x : y;
        }

        /// <inheritdoc cref="INumber{TSelf}.Sign(TSelf)" />
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static int Sign(BFloat16 value) => MathF.Sign(value);

        //
        // INumberBase
        //

        /// <inheritdoc cref="INumberBase{TSelf}.One" />
        static BFloat16 INumberBase<BFloat16>.One => new(0X3f80);

        /// <inheritdoc cref="INumberBase{TSelf}.Radix" />
        static int INumberBase<BFloat16>.Radix => 2;

        /// <inheritdoc cref="INumberBase{TSelf}.Zero" />
        static BFloat16 INumberBase<BFloat16>.Zero => default;

        /// <inheritdoc cref="INumberBase{TSelf}.Abs(TSelf)" />
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static BFloat16 Abs(BFloat16 value) => (BFloat16)MathF.Abs(value);

        /// <inheritdoc cref="INumberBase{TSelf}.CreateChecked{TOther}(TOther)" />
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static BFloat16 CreateChecked<TOther>(TOther value)
            where TOther : INumberBase<TOther> {
            BFloat16 result;

            if (typeof(TOther) == typeof(BFloat16)) {
                result = (BFloat16)(object)value;
            } else if (!TryConvertFrom(value, out result) && !TOther.TryConvertToChecked(value, out result)) {
                ThrowHelper.ThrowNotSupportedException();
            }

            return result;
        }

        /// <inheritdoc cref="INumberBase{TSelf}.CreateSaturating{TOther}(TOther)" />
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static BFloat16 CreateSaturating<TOther>(TOther value)
            where TOther : INumberBase<TOther> {
            BFloat16 result;

            if (typeof(TOther) == typeof(BFloat16)) {
                result = (BFloat16)(object)value;
            } else if (!TryConvertFrom(value, out result) && !TOther.TryConvertToSaturating(value, out result)) {
                ThrowHelper.ThrowNotSupportedException();
            }

            return result;
        }

        /// <inheritdoc cref="INumberBase{TSelf}.CreateTruncating{TOther}(TOther)" />
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static BFloat16 CreateTruncating<TOther>(TOther value)
            where TOther : INumberBase<TOther> {
            BFloat16 result;

            if (typeof(TOther) == typeof(BFloat16)) {
                result = (BFloat16)(object)value;
            } else if (!TryConvertFrom(value, out result) && !TOther.TryConvertToTruncating(value, out result)) {
                ThrowHelper.ThrowNotSupportedException();
            }

            return result;
        }

        /// <inheritdoc cref="INumberBase{TSelf}.IsCanonical(TSelf)" />
        static bool INumberBase<BFloat16>.IsCanonical(BFloat16 value) => true;

        /// <inheritdoc cref="INumberBase{TSelf}.IsComplexNumber(TSelf)" />
        static bool INumberBase<BFloat16>.IsComplexNumber(BFloat16 value) => false;

        /// <inheritdoc cref="INumberBase{TSelf}.IsEvenInteger(TSelf)" />
        public static bool IsEvenInteger(BFloat16 value) => IsInteger(value) && (Abs((BFloat16)(value % 2)) == 0);

        /// <inheritdoc cref="INumberBase{TSelf}.IsImaginaryNumber(TSelf)" />
        static bool INumberBase<BFloat16>.IsImaginaryNumber(BFloat16 value) => false;

        /// <inheritdoc cref="INumberBase{TSelf}.IsInteger(TSelf)" />
        public static bool IsInteger(BFloat16 value) => IsFinite(value) && (value == Truncate(value));

        /// <inheritdoc cref="INumberBase{TSelf}.IsOddInteger(TSelf)" />
        public static bool IsOddInteger(BFloat16 value) => IsInteger(value) && (Abs((BFloat16)(value % 2)) == 1);

        /// <inheritdoc cref="INumberBase{TSelf}.IsPositive(TSelf)" />
        public static bool IsPositive(BFloat16 value) => unchecked((Int16)value.bits) >= 0;

        /// <inheritdoc cref="INumberBase{TSelf}.IsRealNumber(TSelf)" />
        public static bool IsRealNumber(BFloat16 value) {
            // A NaN will never equal itself so this is an
            // easy and efficient way to check for a real number.

#pragma warning disable CS1718
            return value == value;
#pragma warning restore CS1718
        }

        /// <inheritdoc cref="INumberBase{TSelf}.IsZero(TSelf)" />
        static bool INumberBase<BFloat16>.IsZero(BFloat16 value) => (value == 0);

        /// <inheritdoc cref="INumberBase{TSelf}.MaxMagnitude(TSelf, TSelf)" />
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static BFloat16 MaxMagnitude(BFloat16 x, BFloat16 y) => (BFloat16)MathF.MaxMagnitude(x, y);

        /// <inheritdoc cref="INumberBase{TSelf}.MaxMagnitudeNumber(TSelf, TSelf)" />
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static BFloat16 MaxMagnitudeNumber(BFloat16 x, BFloat16 y) {
            // This matches the IEEE 754:2019 `maximumMagnitudeNumber` function
            //
            // It does not propagate NaN inputs back to the caller and
            // otherwise returns the input with a larger magnitude.
            // It treats +0 as larger than -0 as per the specification.

            BFloat16 ax = Abs(x);
            BFloat16 ay = Abs(y);

            if ((ax > ay) || IsNaN(ay)) {
                return x;
            }

            if (ax == ay) {
                return IsNegative(x) ? y : x;
            }

            return y;
        }

        /// <inheritdoc cref="INumberBase{TSelf}.MinMagnitude(TSelf, TSelf)" />
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static BFloat16 MinMagnitude(BFloat16 x, BFloat16 y) => (BFloat16)MathF.MinMagnitude(x, y);

        /// <inheritdoc cref="INumberBase{TSelf}.MinMagnitudeNumber(TSelf, TSelf)" />
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static BFloat16 MinMagnitudeNumber(BFloat16 x, BFloat16 y) {
            // This matches the IEEE 754:2019 `minimumMagnitudeNumber` function
            //
            // It does not propagate NaN inputs back to the caller and
            // otherwise returns the input with a larger magnitude.
            // It treats +0 as larger than -0 as per the specification.

            BFloat16 ax = Abs(x);
            BFloat16 ay = Abs(y);

            if ((ax < ay) || IsNaN(ay)) {
                return x;
            }

            if (ax == ay) {
                return IsNegative(x) ? x : y;
            }

            return y;
        }

        /// <inheritdoc cref="INumberBase{TSelf}.TryConvertFromChecked{TOther}(TOther, out TSelf)" />
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        static bool INumberBase<BFloat16>.TryConvertFromChecked<TOther>(TOther value, out BFloat16 result) {
            return TryConvertFrom(value, out result);
        }

        /// <inheritdoc cref="INumberBase{TSelf}.TryConvertFromSaturating{TOther}(TOther, out TSelf)" />
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        static bool INumberBase<BFloat16>.TryConvertFromSaturating<TOther>(TOther value, out BFloat16 result) {
            return TryConvertFrom(value, out result);
        }

        /// <inheritdoc cref="INumberBase{TSelf}.TryConvertFromTruncating{TOther}(TOther, out TSelf)" />
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        static bool INumberBase<BFloat16>.TryConvertFromTruncating<TOther>(TOther value, out BFloat16 result) {
            return TryConvertFrom(value, out result);
        }

        private static bool TryConvertFrom<TOther>(TOther value, out BFloat16 result)
            where TOther : INumberBase<TOther> {
            // In order to reduce overall code duplication and improve the inlinabilty of these
            // methods for the corelib types we have `ConvertFrom` handle the same sign and
            // `ConvertTo` handle the opposite sign. However, since there is an uneven split
            // between signed and unsigned types, the one that handles unsigned will also
            // handle `Decimal`.
            //
            // That is, `ConvertFrom` for `BFloat16` will handle the other signed types and
            // `ConvertTo` will handle the unsigned types

            if (typeof(TOther) == typeof(float)) {
                float actualValue = (float)(object)value;
                result = (BFloat16)actualValue;
                return true;
            } else if (typeof(TOther) == typeof(double)) {
                double actualValue = (double)(object)value;
                result = (BFloat16)actualValue;
                return true;
            } else if (typeof(TOther) == typeof(Half)) {
                Half actualValue = (Half)(object)value;
                result = (BFloat16)(float)actualValue;
                return true;
            } else if (typeof(TOther) == typeof(short)) {
                short actualValue = (short)(object)value;
                result = (BFloat16)(float)actualValue;
                return true;
            } else if (typeof(TOther) == typeof(int)) {
                int actualValue = (int)(object)value;
                result = (BFloat16)(double)actualValue;
                return true;
            } else if (typeof(TOther) == typeof(long)) {
                long actualValue = (long)(object)value;
                // TODO: Double rounding
                result = (BFloat16)(double)actualValue;
                return true;
            } else if (typeof(TOther) == typeof(Int128)) {
                Int128 actualValue = (Int128)(object)value;
                // TODO: Double rounding
                result = (BFloat16)(double)actualValue;
                return true;
            } else if (typeof(TOther) == typeof(nint)) {
                nint actualValue = (nint)(object)value;
                // TODO: Double rounding
                result = (BFloat16)(double)actualValue;
                return true;
            } else if (typeof(TOther) == typeof(sbyte)) {
                sbyte actualValue = (sbyte)(object)value;
                result = (BFloat16)(float)actualValue;
                return true;
            } else if (typeof(TOther) == typeof(ushort)) {
                ushort actualValue = (ushort)(object)value;
                result = (BFloat16)(float)actualValue;
                return true;
            } else if (typeof(TOther) == typeof(uint)) {
                uint actualValue = (uint)(object)value;
                result = (BFloat16)(double)actualValue;
                return true;
            } else if (typeof(TOther) == typeof(ulong)) {
                ulong actualValue = (ulong)(object)value;
                // TODO: Double rounding
                result = (BFloat16)(double)actualValue;
                return true;
            } else if (typeof(TOther) == typeof(UInt128)) {
                UInt128 actualValue = (UInt128)(object)value;
                // TODO: Double rounding
                result = (BFloat16)(double)actualValue;
                return true;
            } else if (typeof(TOther) == typeof(nuint)) {
                nuint actualValue = (nuint)(object)value;
                // TODO: Double rounding
                result = (BFloat16)(double)actualValue;
                return true;
            } else if (typeof(TOther) == typeof(char)) {
                char actualValue = (char)(object)value;
                result = (BFloat16)(float)actualValue;
                return true;
            } else if (typeof(TOther) == typeof(byte)) {
                byte actualValue = (byte)(object)value;
                result = (BFloat16)(float)actualValue;
                return true;
            } else if (typeof(TOther) == typeof(decimal)) {
                decimal actualValue = (decimal)(object)value;
                // TODO: Double rounding
                result = (BFloat16)(double)actualValue;
                return true;
            } else {
                result = default;
                return false;
            }
        }

        /// <inheritdoc cref="INumberBase{TSelf}.TryConvertToChecked{TOther}(TSelf, out TOther)" />
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        static bool INumberBase<BFloat16>.TryConvertToChecked<TOther>(BFloat16 value, [MaybeNullWhen(false)] out TOther result) {
            // In order to reduce overall code duplication and improve the inlinabilty of these
            // methods for the corelib types we have `ConvertFrom` handle the same sign and
            // `ConvertTo` handle the opposite sign. However, since there is an uneven split
            // between signed and unsigned types, the one that handles unsigned will also
            // handle `Decimal`.
            //
            // That is, `ConvertFrom` for `float` will handle the other signed types and
            // `ConvertTo` will handle the unsigned types.

            if (typeof(TOther) == typeof(byte)) {
                byte actualResult = checked((byte)value);
                result = (TOther)(object)actualResult;
                return true;
            } else if (typeof(TOther) == typeof(char)) {
                char actualResult = checked((char)value);
                result = (TOther)(object)actualResult;
                return true;
            } else if (typeof(TOther) == typeof(decimal)) {
                decimal actualResult = checked((decimal)(float)value);
                result = (TOther)(object)actualResult;
                return true;
            } else if (typeof(TOther) == typeof(ushort)) {
                ushort actualResult = checked((ushort)value);
                result = (TOther)(object)actualResult;
                return true;
            } else if (typeof(TOther) == typeof(uint)) {
                uint actualResult = checked((uint)value);
                result = (TOther)(object)actualResult;
                return true;
            } else if (typeof(TOther) == typeof(ulong)) {
                ulong actualResult = checked((ulong)value);
                result = (TOther)(object)actualResult;
                return true;
            } else if (typeof(TOther) == typeof(UInt128)) {
                UInt128 actualResult = checked((UInt128)(float)value);
                result = (TOther)(object)actualResult;
                return true;
            } else if (typeof(TOther) == typeof(nuint)) {
                nuint actualResult = checked((nuint)value);
                result = (TOther)(object)actualResult;
                return true;
            } else if (typeof(TOther) == typeof(float)) {
                float actualResult = checked((float)value);
                result = (TOther)(object)actualResult;
                return true;
            } else if (typeof(TOther) == typeof(double)) {
                double actualResult = checked((double)value);
                result = (TOther)(object)actualResult;
                return true;
            } else if (typeof(TOther) == typeof(Half)) {
                Half actualResult = checked((Half)(float)value);
                result = (TOther)(object)actualResult;
                return true;
            } else if (typeof(TOther) == typeof(short)) {
                short actualResult = checked((short)value);
                result = (TOther)(object)actualResult;
                return true;
            } else if (typeof(TOther) == typeof(int)) {
                int actualResult = checked((int)value);
                result = (TOther)(object)actualResult;
                return true;
            } else if (typeof(TOther) == typeof(long)) {
                long actualResult = checked((long)value);
                result = (TOther)(object)actualResult;
                return true;
            } else if (typeof(TOther) == typeof(Int128)) {
                Int128 actualResult = checked((Int128)(float)value);
                result = (TOther)(object)actualResult;
                return true;
            } else if (typeof(TOther) == typeof(nint)) {
                nint actualResult = checked((nint)value);
                result = (TOther)(object)actualResult;
                return true;
            } else if (typeof(TOther) == typeof(sbyte)) {
                sbyte actualResult = checked((sbyte)value);
                result = (TOther)(object)actualResult;
                return true;
            } else {
                result = default;
                return false;
            }
        }

        /// <inheritdoc cref="INumberBase{TSelf}.TryConvertToSaturating{TOther}(TSelf, out TOther)" />
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        static bool INumberBase<BFloat16>.TryConvertToSaturating<TOther>(BFloat16 value, [MaybeNullWhen(false)] out TOther result) {
            return TryConvertTo(value, out result);
        }

        /// <inheritdoc cref="INumberBase{TSelf}.TryConvertToTruncating{TOther}(TSelf, out TOther)" />
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        static bool INumberBase<BFloat16>.TryConvertToTruncating<TOther>(BFloat16 value, [MaybeNullWhen(false)] out TOther result) {
            return TryConvertTo(value, out result);
        }

        private static bool TryConvertTo<TOther>(BFloat16 value, [MaybeNullWhen(false)] out TOther result)
            where TOther : INumberBase<TOther> {
            // In order to reduce overall code duplication and improve the inlinabilty of these
            // methods for the corelib types we have `ConvertFrom` handle the same sign and
            // `ConvertTo` handle the opposite sign. However, since there is an uneven split
            // between signed and unsigned types, the one that handles unsigned will also
            // handle `Decimal`.
            //
            // That is, `ConvertFrom` for `BFloat16` will handle the other signed types and
            // `ConvertTo` will handle the unsigned types.

            float f = value;

            if (typeof(TOther) == typeof(byte)) {
                var actualResult = (f >= byte.MaxValue) ? byte.MaxValue :
                                   (f <= byte.MinValue) ? byte.MinValue : (byte)f;
                result = (TOther)(object)actualResult;
                return true;
            } else if (typeof(TOther) == typeof(char)) {
                char actualResult = (f >= char.MaxValue) ? char.MaxValue :
                                    (f <= char.MinValue) ? char.MinValue : (char)f;
                result = (TOther)(object)actualResult;
                return true;
            } else if (typeof(TOther) == typeof(decimal)) {
                // TODO: Double rounding
                decimal actualResult = (f >= +79228162514264337593543950336.0f) ? decimal.MaxValue :
                                       (f <= -79228162514264337593543950336.0f) ? decimal.MinValue :
                                       float.IsNaN(f) ? 0.0m : (decimal)(float)f;
                result = (TOther)(object)actualResult;
                return true;
            } else if (typeof(TOther) == typeof(ushort)) {
                ushort actualResult = (f >= ushort.MaxValue) ? ushort.MaxValue :
                                      (f <= ushort.MinValue) ? ushort.MinValue : (ushort)f;
                result = (TOther)(object)actualResult;
                return true;
            } else if (typeof(TOther) == typeof(uint)) {
                uint actualResult = (f >= uint.MaxValue) ? uint.MaxValue :
                                    (f <= uint.MinValue) ? uint.MinValue : (uint)f;
                result = (TOther)(object)actualResult;
                return true;
            } else if (typeof(TOther) == typeof(ulong)) {
                ulong actualResult = (f >= ulong.MaxValue) ? ulong.MaxValue :
                                     (f <= ulong.MinValue) ? ulong.MinValue :
                                     float.IsNaN(f) ? 0 : (ulong)f;
                result = (TOther)(object)actualResult;
                return true;
            } else if (typeof(TOther) == typeof(UInt128)) {
                UInt128 actualResult = (f == PositiveInfinity) ? UInt128.MaxValue :
                                       (f <= 0.0F) ? UInt128.MinValue : (UInt128)(float)f;
                result = (TOther)(object)actualResult;
                return true;
            } else if (typeof(TOther) == typeof(nuint)) {
#if CORELIB
#if TARGET_64BIT
                nuint actualResult = (value >= ulong.MaxValue) ? unchecked((nuint)ulong.MaxValue) :
                                     (value <= ulong.MinValue) ? unchecked((nuint)ulong.MinValue) : (nuint)value;
                result = (TOther)(object)actualResult;
                return true;
#else
                nuint actualResult = (value >= uint.MaxValue) ? uint.MaxValue :
                                     (value <= uint.MinValue) ? uint.MinValue : (nuint)value;
                result = (TOther)(object)actualResult;
                return true;
#endif
#else
                nuint actualResult = (f >= nuint.MaxValue) ? nuint.MaxValue :
                                     (f <= nuint.MinValue) ? nuint.MinValue : (nuint)f;
                result = (TOther)(object)actualResult;
                return true;
#endif
            } else if (typeof(TOther) == typeof(float)) {
                float actualResult = (float)f;
                result = (TOther)(object)actualResult;
                return true;
            } else if (typeof(TOther) == typeof(double)) {
                double actualResult = (double)f;
                result = (TOther)(object)actualResult;
                return true;
            } else if (typeof(TOther) == typeof(Half)) {
                Half actualResult = (Half)f;
                result = (TOther)(object)actualResult;
                return true;
            } else if (typeof(TOther) == typeof(short)) {
                short actualResult = (f >= short.MaxValue) ? short.MaxValue :
                                     (f <= short.MinValue) ? short.MinValue : (short)f;
                result = (TOther)(object)actualResult;
                return true;
            } else if (typeof(TOther) == typeof(int)) {
                int actualResult = (f >= int.MaxValue) ? int.MaxValue :
                                     (f <= int.MinValue) ? int.MinValue : (int)f;
                result = (TOther)(object)actualResult;
                return true;
            } else if (typeof(TOther) == typeof(long)) {
                long actualResult = (f >= long.MaxValue) ? long.MaxValue :
                                     (f <= long.MinValue) ? long.MinValue : (long)f;
                result = (TOther)(object)actualResult;
                return true;
            } else if (typeof(TOther) == typeof(Int128)) {
                Int128 actualResult = (f == PositiveInfinity) ? Int128.MaxValue :
                                       (f <= 0.0F) ? Int128.MinValue : (Int128)(float)f;
                result = (TOther)(object)actualResult;
                return true;
            } else if (typeof(TOther) == typeof(nint)) {
                nint actualResult = (f >= nint.MaxValue) ? nint.MaxValue :
                                     (f <= nint.MinValue) ? nint.MinValue : (nint)f;
                result = (TOther)(object)actualResult;
                return true;
            } else if (typeof(TOther) == typeof(sbyte)) {
                sbyte actualResult = (f >= sbyte.MaxValue) ? sbyte.MaxValue :
                                     (f <= sbyte.MinValue) ? sbyte.MinValue : (sbyte)f;
                result = (TOther)(object)actualResult;
                return true;
            } else {
                result = default;
                return false;
            }
        }

        //
        // IParsable
        //

        /// <inheritdoc cref="IParsable{TSelf}.TryParse(string?, IFormatProvider?, out TSelf)" />
        public static bool TryParse([NotNullWhen(true)] string? s, IFormatProvider? provider, out BFloat16 result) => TryParse(s, NumberStyles.Float | NumberStyles.AllowThousands, provider, out result);

        //
        // IPowerFunctions
        //

        /// <inheritdoc cref="IPowerFunctions{TSelf}.Pow(TSelf, TSelf)" />
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static BFloat16 Pow(BFloat16 x, BFloat16 y) => (BFloat16)MathF.Pow(x, y);

        //
        // IRootFunctions
        //

        /// <inheritdoc cref="IRootFunctions{TSelf}.Cbrt(TSelf)" />
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static BFloat16 Cbrt(BFloat16 x) => (BFloat16)MathF.Cbrt(x);

        /// <inheritdoc cref="IRootFunctions{TSelf}.Hypot(TSelf, TSelf)" />
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static BFloat16 Hypot(BFloat16 x, BFloat16 y) => (BFloat16)float.Hypot(x, y);

        /// <inheritdoc cref="IRootFunctions{TSelf}.RootN(TSelf, int)" />
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static BFloat16 RootN(BFloat16 x, int n) => (BFloat16)float.RootN(x, n);

        /// <inheritdoc cref="IRootFunctions{TSelf}.Sqrt(TSelf)" />
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static BFloat16 Sqrt(BFloat16 x) => (BFloat16)MathF.Sqrt(x);

        //
        // ISignedNumber
        //

        /// <inheritdoc cref="ISignedNumber{TSelf}.NegativeOne" />
        static BFloat16 ISignedNumber<BFloat16>.NegativeOne => new(0Xbf80);

        //
        // ISpanParsable
        //

        /// <inheritdoc cref="ISpanParsable{TSelf}.Parse(ReadOnlySpan{char}, IFormatProvider?)" />
        public static BFloat16 Parse(ReadOnlySpan<char> s, IFormatProvider? provider) => Parse(s, NumberStyles.Float | NumberStyles.AllowThousands, provider);

        /// <inheritdoc cref="ISpanParsable{TSelf}.TryParse(ReadOnlySpan{char}, IFormatProvider?, out TSelf)" />
        public static bool TryParse(ReadOnlySpan<char> s, IFormatProvider? provider, out BFloat16 result) => TryParse(s, NumberStyles.Float | NumberStyles.AllowThousands, provider, out result);

        //
        // ISubtractionOperators
        //

        /// <inheritdoc cref="ISubtractionOperators{TSelf, TOther, TResult}.op_Subtraction(TSelf, TOther)" />
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        static BFloat16 ISubtractionOperators<BFloat16, BFloat16, BFloat16>.operator -(BFloat16 left, BFloat16 right) => (BFloat16)(left - right);

        //
        // ITrigonometricFunctions
        //

        /// <inheritdoc cref="ITrigonometricFunctions{TSelf}.Acos(TSelf)" />
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static BFloat16 Acos(BFloat16 x) => (BFloat16)MathF.Acos(x);

        /// <inheritdoc cref="ITrigonometricFunctions{TSelf}.AcosPi(TSelf)" />
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static BFloat16 AcosPi(BFloat16 x) => (BFloat16)float.Acos(x);

        /// <inheritdoc cref="ITrigonometricFunctions{TSelf}.Asin(TSelf)" />
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static BFloat16 Asin(BFloat16 x) => (BFloat16)MathF.Asin(x);

        /// <inheritdoc cref="ITrigonometricFunctions{TSelf}.AsinPi(TSelf)" />
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static BFloat16 AsinPi(BFloat16 x) => (BFloat16)float.AsinPi(x);

        /// <inheritdoc cref="ITrigonometricFunctions{TSelf}.Atan(TSelf)" />
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static BFloat16 Atan(BFloat16 x) => (BFloat16)MathF.Atan(x);

        /// <inheritdoc cref="ITrigonometricFunctions{TSelf}.AtanPi(TSelf)" />
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static BFloat16 AtanPi(BFloat16 x) => (BFloat16)float.AtanPi(x);

        /// <inheritdoc cref="ITrigonometricFunctions{TSelf}.Cos(TSelf)" />
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static BFloat16 Cos(BFloat16 x) => (BFloat16)MathF.Cos(x);

        /// <inheritdoc cref="ITrigonometricFunctions{TSelf}.CosPi(TSelf)" />
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static BFloat16 CosPi(BFloat16 x) => (BFloat16)float.CosPi(x);

        /// <inheritdoc cref="ITrigonometricFunctions{TSelf}.DegreesToRadians(TSelf)" />
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static BFloat16 DegreesToRadians(BFloat16 degrees) => (BFloat16)float.DegreesToRadians(degrees);

        /// <inheritdoc cref="ITrigonometricFunctions{TSelf}.RadiansToDegrees(TSelf)" />
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static BFloat16 RadiansToDegrees(BFloat16 radians) => (BFloat16)float.RadiansToDegrees(radians);


        /// <inheritdoc cref="ITrigonometricFunctions{TSelf}.Sin(TSelf)" />
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static BFloat16 Sin(BFloat16 x) => (BFloat16)MathF.Sin(x);

        /// <inheritdoc cref="ITrigonometricFunctions{TSelf}.SinCos(TSelf)" />
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static (BFloat16 Sin, BFloat16 Cos) SinCos(BFloat16 x) {
            var (sin, cos) = MathF.SinCos(x);
            return ((BFloat16)sin, (BFloat16)cos);
        }

        /// <inheritdoc cref="ITrigonometricFunctions{TSelf}.SinCos(TSelf)" />
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static (BFloat16 SinPi, BFloat16 CosPi) SinCosPi(BFloat16 x) {
            var (sin, cos) = float.SinCosPi(x);
            return ((BFloat16)sin, (BFloat16)cos);
        }

        /// <inheritdoc cref="ITrigonometricFunctions{TSelf}.SinPi(TSelf)" />
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static BFloat16 SinPi(BFloat16 x) => (BFloat16)float.SinPi(x);

        /// <inheritdoc cref="ITrigonometricFunctions{TSelf}.Tan(TSelf)" />
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static BFloat16 Tan(BFloat16 x) => (BFloat16)MathF.Tan(x);

        /// <inheritdoc cref="ITrigonometricFunctions{TSelf}.TanPi(TSelf)" />
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static BFloat16 TanPi(BFloat16 x) => (BFloat16)float.TanPi(x);

        //
        // IUnaryNegationOperators
        //

        /// <inheritdoc cref="IUnaryNegationOperators{TSelf, TResult}.op_UnaryNegation(TSelf)" />
        static BFloat16 IUnaryNegationOperators<BFloat16, BFloat16>.operator -(BFloat16 value) => new(unchecked((UInt16)(value.bits ^ SignMask)));

        //
        // IUnaryPlusOperators
        //

        /// <inheritdoc cref="IUnaryPlusOperators{TSelf, TResult}.op_UnaryPlus(TSelf)" />
        static BFloat16 IUnaryPlusOperators<BFloat16, BFloat16>.operator +(BFloat16 value) => value;

        //
        // IUtf8SpanParsable
        //

        /// <inheritdoc cref="INumberBase{TSelf}.Parse(ReadOnlySpan{byte}, NumberStyles, IFormatProvider?)" />
        public static BFloat16 Parse(ReadOnlySpan<byte> utf8Text, NumberStyles style = NumberStyles.Float | NumberStyles.AllowThousands, IFormatProvider? provider = null) {
            return (BFloat16)float.Parse(utf8Text, style, provider);
        }

        /// <inheritdoc cref="INumberBase{TSelf}.TryParse(ReadOnlySpan{byte}, NumberStyles, IFormatProvider?, out TSelf)" />
        public static bool TryParse(ReadOnlySpan<byte> utf8Text, NumberStyles style, IFormatProvider? provider, out BFloat16 result) {
            if (float.TryParse(utf8Text, style, provider, out var f)) {
                result = (BFloat16)f;
                return true;
            }
            result = default;
            return false;
        }

        /// <inheritdoc cref="IUtf8SpanParsable{TSelf}.Parse(ReadOnlySpan{byte}, IFormatProvider?)" />
        public static BFloat16 Parse(ReadOnlySpan<byte> utf8Text, IFormatProvider? provider) => Parse(utf8Text, NumberStyles.Float | NumberStyles.AllowThousands, provider);

        /// <inheritdoc cref="IUtf8SpanParsable{TSelf}.TryParse(ReadOnlySpan{byte}, IFormatProvider?, out TSelf)" />
        public static bool TryParse(ReadOnlySpan<byte> utf8Text, IFormatProvider? provider, out BFloat16 result) => TryParse(utf8Text, NumberStyles.Float | NumberStyles.AllowThousands, provider, out result);

        public static implicit operator Single(BFloat16 value) {
            return value.InternalValue;
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static explicit operator BFloat16(Single value) {
            var b = BitConverter.SingleToUInt32Bits(value);
            var a = b & 0X7fffffff;
            var s = unchecked(b - a);
            var m = (uint)unchecked((UInt16)b);
            if ((b & 0X7f800000) != 0X7f800000) {
                // Normal or subnormal numbers
                if (m > 0X8000 || (m == 0X8000 && (b & 0X10000) == 0X10000)) {
                    unchecked {
                        a += 0X10000;
                    }
                }
                return new(unchecked((UInt16)((s | a) >> 16)));
            }
            return new(unchecked((UInt16)((b >> 16) | Unsafe.BitCast<bool, byte>(m != 0))));
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static implicit operator double(BFloat16 value) {
            return value.InternalValue;
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static explicit operator BFloat16(double value) {
            // Minimum exponent for rounding
            const UInt64 MinExp = 0X38100000_00000000U;
            // Exponent displacement #1
            const UInt64 Exponent942 = 0X3ae00000_00000000U;
            const int DoubleMaxExponent = 0X7ff;
            const int DoubleBiasedExponentShift = 52;
            // Exponent mask
            const UInt64 DoubleBiasedExponentMask = (UInt64)DoubleMaxExponent << DoubleBiasedExponentShift;
            // Exponent displacement #2
            const UInt64 Exponent45 = 0X02d00000_00000000U;
            // Maximum value that is not Infinity in BFloat16
            const double MaxBFloat16ValueBelowInfinity = 3.3961775E+38F;
            // Mask for exponent bits in BFloat16
            const uint ExponentMask = BiasedExponentMask;
            UInt64 bitValue = BitConverter.DoubleToUInt64Bits(value);
            // Extract sign bit
            const UInt64 DoubleSignMask = unchecked((UInt64)Int64.MinValue);
            UInt64 sign = (bitValue & DoubleSignMask) >> 48;
            // Detecting NaN (~0u if a is not NaN)
            UInt64 realMask = (UInt64)(Int64)(Unsafe.BitCast<bool, sbyte>(double.IsNaN(value)) - 1);
            // Clear sign bit
            value = double.Abs(value);
            // Rectify values that are Infinity in BFloat16. (double.Min now emits vminps instruction if one of two arguments is a constant)
            value = double.Min(MaxBFloat16ValueBelowInfinity, value);
            // Rectify lower exponent
            UInt64 exponentOffset0 = BitConverter.DoubleToUInt64Bits(double.Max(value, BitConverter.UInt64BitsToDouble(MinExp)));
            // Extract exponent
            exponentOffset0 &= DoubleBiasedExponentMask;
            // Add exponent by 45
            exponentOffset0 += Exponent45;
            // Round Double into BFloat16's precision (NaN also gets modified here, just setting the MSB of fraction)
            value += BitConverter.UInt64BitsToDouble(exponentOffset0);
            bitValue = BitConverter.DoubleToUInt64Bits(value);
            // Only exponent bits will be modified if NaN
            UInt64 maskedBFloat16ExponentForNaN = ~realMask & ExponentMask;
            // Subtract exponent by 942
            bitValue -= Exponent942;
            // Shift bitValue right by 45 bits to match the boundary of exponent part and fraction part.
            UInt64 newExponent = bitValue >> 45;
            // Clear the fraction parts if the value was NaN.
            bitValue &= realMask;
            // Merge the exponent part with fraction part, and add the exponent part and fraction part's overflow.
            bitValue += newExponent;
            // Clear exponents if value is NaN
            bitValue &= ~maskedBFloat16ExponentForNaN;
            // Merge sign bit with possible NaN exponent
            UInt64 signAndMaskedExponent = maskedBFloat16ExponentForNaN | sign;
            // Merge sign bit and possible NaN exponent
            bitValue |= signAndMaskedExponent;
            // The final result
            return new(unchecked((UInt16)bitValue));
        }

        [Experimental("May be relocated in the future.")]
        public static BFloat16 UInt16BitsToBFloat16(UInt16 bits) {
            return new(bits);
        }

        [Experimental("May be relocated in the future.")]
        public static UInt16 BFloat16ToUInt16Bits(BFloat16 value) {
            return value.bits;
        }

        [Experimental("May be relocated in the future.")]
        public static BFloat16 Int16BitsToBFloat16(Int16 bits) {
            return new(unchecked((UInt16)bits));
        }

        [Experimental("May be relocated in the future.")]
        public static Int16 BFloat16ToInt16Bits(BFloat16 value) {
            return unchecked((Int16)value.bits);
        }

        /// <summary>
        /// Converts a <see cref="float"/> value to a <see cref="BFloat16"/> value. The <see cref="float"/> value is truncated to fit the <see cref="BFloat16"/> format.
        /// </summary>
        /// <remarks>This method may return positive or negative infinity when the input <paramref name="value"/> is not a number (NaN).</remarks>
        [Experimental("")]
        public static BFloat16 FromSingleFastRealNumber(Single value) {
            return Unsafe.As<Single, BFloat16>(ref value);
        }

        /// <summary>
        /// Converts a <see cref="double"/> value to a <see cref="BFloat16"/> value. The <see cref="double"/> value is first rounded to a <see cref="float"/> value, then truncated to fit the <see cref="BFloat16"/> format.
        /// </summary>
        /// <remarks>This method may return positive or negative infinity when the input <paramref name="value"/> is not a number (NaN).</remarks>
        [Experimental("")]
        public static BFloat16 FromDoubleFastRealNumber(double value) {
            return FromSingleFastRealNumber((Single)value);
        }
    }
}
