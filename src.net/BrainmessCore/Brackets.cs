using System;

namespace Welch.Brainmess
{
    /// <summary>
    /// A utility class for finding matching braces. 
    /// </summary>
    public class Brackets
    {
        // Clean this up.

        /// <summary>
        /// Returns the index of the brace that matches the brace at the specified index of sequence.
        /// </summary>
        public static int FindMatch(string sequence, int index)
        {
            if (sequence[index] == '[') return JumpForward(sequence, index);
            if (sequence[index] == ']') return JumpBackward(sequence, index);
            throw new ArgumentException("The character at index " + index + " is not a jump character");

        }

        private static int JumpForward(string sequence, int index)
        {
            int counter = index + 1;
            int nestLevel = 1;

            while (nestLevel > 0)
            {
                char character = sequence[counter];

                if (character == '[') nestLevel++;
                else if (character == ']') nestLevel--;

                counter++;
            }

            return counter - 1;
        }

        private static int JumpBackward(string sequence, int index)
        {
            int counter = index - 1;
            int nestLevel = 1;

            while (nestLevel > 0)
            {
                char character = sequence[counter];

                if (character == '[') nestLevel--;
                else if (character == ']') nestLevel++;

                counter--;
            }

            return counter + 1;
        }


    }
}

