namespace Welch.Brainmess
{
    static internal class Identity
    {
        /// <summary>
        /// The Identity function. Returns the specified value.
        /// Useful for quieting Resharper warnings about null.
        /// </summary>
        public static T ValueOf<T>(this T value)
        {
            return value;
        }
    }
}