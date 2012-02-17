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
            int travelDirection;
            if (sequence[index] == '[') travelDirection = TravelForward;
            else if (sequence[index] == ']') travelDirection = TravelBackward;
            else throw new ArgumentException("The character at index " + index + " is not a jump character");

            int position = index + travelDirection;
            int nestLevel = 1;

            while (nestLevel > 0)
            {
                char character = sequence[position];

                if (character == '[') nestLevel = nestLevel + travelDirection;
                else if (character == ']') nestLevel = nestLevel - travelDirection;

                position = position + travelDirection;
            }

            return position - travelDirection;
        }

        private const int TravelForward = 1;
        private const int TravelBackward = -1;
    }
}

