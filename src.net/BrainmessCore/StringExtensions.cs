using System;
using System.Diagnostics;

namespace Welch.Brainmess
{
    /// <summary>
    /// A utility class for finding matching braces. 
    /// </summary>
    public static class StringExtensions
    {

        /// <summary>
        /// Returns the index of the square bracket that matches the bracket at the specified index of sequence.
        /// If the character at the specified index is not a bracket then an excpetion is thrown.
        /// </summary>
        public static int FindMatch(this string sequence, int index)
        {
            if (sequence[index] == '[') return FindMatch(sequence, index, TravelForward);
            if (sequence[index] == ']') return FindMatch(sequence, index, TravelBackward);
            throw new ArgumentException("The character at index " + index + " is not a jump character");

        }

        private const int TravelForward = 1;
        private const int TravelBackward = -1;

        private static int FindMatch(string sequence, int index, int increment)
        {
            // increment should be +1 to move forward, and -1 to move backward
            Debug.Assert((increment == -1) || (increment == 1));

            int position = index + increment;
            int nestLevel = 1;

            while (nestLevel > 0)
            {
                char character = sequence[position];

                if (character == '[') nestLevel = nestLevel + increment;
                else if (character == ']') nestLevel = nestLevel - increment;

                position = position + increment;
            }

            return position - increment;
        }


    }
}

