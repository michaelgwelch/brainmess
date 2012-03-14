using System;

namespace BrainmessShort
{
    public static class StringExtensions
    {
        /// <summary>
        /// Finds the match for the bracket pointed to by
        /// pc in the program. Increment tells the algorithm
        /// which way to search.
        /// </summary>
        /// <param name="program"></param>
        /// <param name="pc"></param>
        /// <param name="increment"></param>
        /// <returns></returns>
        private static int FindMatch(this string program, int pc, int increment)
        {
            int nestLevel = 1;
            pc += increment;
            while (nestLevel > 0)
            {
                char instruction = program[pc];
                if (instruction == '[') nestLevel += increment;
                else if (instruction == ']') nestLevel -= increment;
                pc += increment;
            }
            return pc - increment;
        }

        public static int FindMatch(this string program, int pc)
        {
            if (program[pc] == '[') return program.FindMatch(pc, 1);
            if (program[pc] == ']') return program.FindMatch(pc, -1);
            throw new ArgumentException("The character at specified location is not a square bracket");

        }
    }
}