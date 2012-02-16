using System;
using System.IO;

namespace Welch.Brainmess
{
    /// <summary>
    /// Defines all of the instructions available in Brainmess. Each type of instruction
    /// defines its own behavior given an execution context that consists of a
    /// <see cref="IProgramStream"/>, <see cref="Tape"/>, <see cref="TextReader"/> (for input),
    /// and a <see cref="TextWriter"/> ) (for output).
    /// </summary>
    public class Instruction
    {
        private readonly Action<IProgramStream, Tape, TextReader, TextWriter> _action;
        protected Instruction(Action<IProgramStream, Tape, TextReader, TextWriter> action)
        {
            _action = action;
        }

        /// <summary>
        /// Executes this instruction within the given context.
        /// </summary>
        public void Execute(IProgramStream program, Tape tape, TextReader input, TextWriter output)
        {
            _action(program, tape, input, output);
        }

        /// <summary>
        /// Returns a MoveForward instruction which when executed, moves the tape forward.
        /// </summary>
        public static readonly Instruction MoveForward =
            new Instruction((program, tape, input, output) => tape.MoveForward());

        /// <summary>
        /// Returns a MoveBackward instruction which when executed, moves the tape backward.
        /// </summary>
        public static readonly Instruction MoveBackward =
            new Instruction((program, tape, input, output) => tape.MoveBackward());

        /// <summary>
        /// Returns an Increment instruction which when executed increments the value of the current tape cell
        /// by one.
        /// </summary>
        public static readonly Instruction Increment =
            new Instruction((program, tape, input, output) => tape.Increment());

        /// <summary>
        /// Returns a Decrement instruction which when executed decrements the value of the current tape cell
        /// by one.
        /// </summary>
        public static readonly Instruction Decrement =
            new Instruction((program, tape, input, output) => tape.Decrement());

        /// <summary>
        /// Returns an Input instruction which when exectued reads one character from the input
        /// and writes its ASCII value to the tape.
        /// </summary>
        public static readonly Instruction Input =
            new Instruction((program, tape, input, output) => tape.Current = input.Read());

        /// <summary>
        /// Returns an Output instruction which when executed gets the integer value from
        /// the current tape cell, converts it to its character value and writes that to output.
        /// </summary>
        public static readonly Instruction Output =
            new Instruction((program, tape, input, output) => output.Write((char)tape.Current));

        /// <summary>
        /// Returns a TestAndJumpForward instruction. When this instruction is executed it looks at the 
        /// value of the current tape cell, and if it is non-zero it does nothing. If the value is zero,
        /// then it advances the program to the instruction following the loop.
        /// </summary>
        public static readonly Instruction TestAndJumpFoward =
            new Instruction((program, tape, input, output) => { if (tape.Current == 0) program.JumpForward(); });

        /// <summary>
        /// Returns a TestAndJumpBackward instruction. When this instruction is executed it looks at
        /// the value of the current tape cell and if it is zero it does nothing. If the value is non-zero,
        /// then it moves the tape back to the beginning of the loop.
        /// </summary>
        public static readonly Instruction TestAndJumpBackward =
            new Instruction((program, tape, input, output) => { if (tape.Current != 0) program.JumpBackward(); });

        /// <summary>
        /// Returns a NoOperation instruction which does nothing when it is executed.
        /// </summary>
        public static readonly Instruction NoOperation =
            new Instruction((program, tape, input, output) => { });

        /// <summary>
        /// Given the ascii value of a character, it returns the <see cref="Instruction"/>
        /// that matches the character.
        /// </summary>
        /// <param name="characterValue"></param>
        /// <returns></returns>
        public static Instruction FromInt(int characterValue)
        {
            switch (characterValue)
            {
                case '>':
                    return MoveForward;
                case '<':
                    return MoveBackward;
                case '+':
                    return Increment;
                case '-':
                    return Decrement;
                case '.':
                    return Output;
                case ',':
                    return Input;
                case '[':
                    return TestAndJumpFoward;
                case ']':
                    return TestAndJumpBackward;
                default:
                    return NoOperation;
            }
        }

    }


}

