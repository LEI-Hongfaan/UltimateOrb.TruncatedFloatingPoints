
namespace UltimateOrb {

    static partial class SR {

        private static readonly bool s_usingResourceKeys = AppContext.TryGetSwitch("System.Resources.UseSystemResourceKeys", out bool usingResourceKeys) && usingResourceKeys;

        // This method is used to decide if we need to append the exception message parameters to the message when calling SR.Format.
        // by default it returns the value of System.Resources.UseSystemResourceKeys AppContext switch or false if not specified.
        // Native code generators can replace the value this returns based on user input at the time of native code generation.
        // The trimming tools are also capable of replacing the value of this method when the application is being trimmed.
        internal static bool UsingResourceKeys() => s_usingResourceKeys;

        internal static string Format(string resourceFormat, object? p1) {
            if (UsingResourceKeys()) {
                return string.Join(", ", resourceFormat, p1);
            }

            return string.Format(resourceFormat, p1);
        }

        internal static string Format(string resourceFormat, object? p1, object? p2) {
            if (UsingResourceKeys()) {
                return string.Join(", ", resourceFormat, p1, p2);
            }

            return string.Format(resourceFormat, p1, p2);
        }

        internal static string Format(string resourceFormat, object? p1, object? p2, object? p3) {
            if (UsingResourceKeys()) {
                return string.Join(", ", resourceFormat, p1, p2, p3);
            }

            return string.Format(resourceFormat, p1, p2, p3);
        }

        internal static string Format(string resourceFormat, params object?[]? args) {
            if (args != null) {
                if (UsingResourceKeys()) {
                    return resourceFormat + ", " + string.Join(", ", args);
                }

                return string.Format(resourceFormat, args);
            }

            return resourceFormat;
        }

        internal static string Format(IFormatProvider? provider, string resourceFormat, object? p1) {
            if (UsingResourceKeys()) {
                return string.Join(", ", resourceFormat, p1);
            }

            return string.Format(provider, resourceFormat, p1);
        }

        internal static string Format(IFormatProvider? provider, string resourceFormat, object? p1, object? p2) {
            if (UsingResourceKeys()) {
                return string.Join(", ", resourceFormat, p1, p2);
            }

            return string.Format(provider, resourceFormat, p1, p2);
        }

        internal static string Format(IFormatProvider? provider, string resourceFormat, object? p1, object? p2, object? p3) {
            if (UsingResourceKeys()) {
                return string.Join(", ", resourceFormat, p1, p2, p3);
            }

            return string.Format(provider, resourceFormat, p1, p2, p3);
        }

        internal static string Format(IFormatProvider? provider, string resourceFormat, params object?[]? args) {
            if (args != null) {
                if (UsingResourceKeys()) {
                    return resourceFormat + ", " + string.Join(", ", args);
                }

                return string.Format(provider, resourceFormat, args);
            }

            return resourceFormat;
        }

        public static string Arg_MustBeBFloat16 { get => @"Object must be of type BFloat16."; }

        public static string InvalidCast_DBNull { get => @"Object cannot be cast to DBNull."; }

        public static string InvalidCast_Empty { get => @"Object cannot be cast to Empty."; }
     
        public static string InvalidCast_FromTo { get => @"Conversion from type '{0}' to type '{1}' is not valid."; }
    }
}
