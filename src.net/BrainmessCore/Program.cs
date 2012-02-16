using System;
using System.Diagnostics;

namespace Welch.Brainmess
{
    /// <summary>
    /// A stream of Brainmess instructions. It maintains a program counter which points
    /// at the next instruction to execute. (Ed., everything is marked internal for a few reasons:
    /// 1) to indicate that really this isn't a general purpose class. It is used by the Interpreter
    /// and Instruction classes. 2) To show how it can still be tested as the clients of this class
    /// are tested 3) JumpForward and JumpBackward require they are called only in certain conditions
    /// which the Interpreter guarantees. 
    /// </summary>
    public class Program : IProgram
    {
        // Mutable State
        int _programCounter; // = 0;

        // Imutable Data
        private readonly string _program;

        /// <summary>
        /// Initializes a new instance of the <see cref="Program"/> class
        /// with the characters from the specified program. The program counter is set to the first
        /// character in the program.
        /// </summary>
        public Program(string program)
        {
            _program = program;
        }

        /// <summary>
        /// Reads the Instruction at the program counter and returns it.
        /// </summary>
        public Instruction Fetch()
        {
            Instruction instruction;
            do
            {
                var character = _program[_programCounter];
                instruction = Instruction.FromInt(character);
                _programCounter++;
            } while (instruction == Instruction.NoOperation && !EndOfProgram);

            return instruction;
        }

        /// <summary>
        /// Gets a value indicating whether this instance is at the end of the program.
        /// If Fetch is called when EndOfProgram is true an exception will be thrown.
        /// </summary>
        /// <value>
        /// <c>true</c> if end of program; otherwise, <c>false</c>.
        /// </value>
        public bool EndOfProgram { get { return _programCounter >= _program.Length; } }


        /// <summary>
        /// This method causes the program counter to move from current location to right after
        /// a matching ']' instruction. It only makes sense to be called if the interpreter is 
        /// executing a TestAndJumpForward instruction. If that is not the case, the results
        /// are not predictable.
        /// </summary>
        public void JumpForward()
        {
            Debug.Assert(_program[_programCounter - 1] == '[');
            _programCounter = _program.FindMatch(_programCounter - 1) + 1;
        }

        /// <summary>
        /// This method causes the program counter to move from current location to 
        /// a matching '[' instruction. It only makes sense to be called if the interpreter is 
        /// executing a TestAndJumpBackward instruction. If that is not the case, the results
        /// are not predictable.
        /// </summary>
        public void JumpBackward()
        {
            Debug.Assert(_program[_programCounter - 1] == ']');
            _programCounter = _program.FindMatch(_programCounter - 1);
        }

        /// <summary>
        /// Gets the "address" of the next instruction to be fetched.
        /// </summary>
        public int ProgramCounter
        {
            get { return _programCounter; }
        }

        /// <summary>
        /// Returns a new Program with the program counter initialized to i.
        /// </summary>
        /// <param name="programString"> </param>
        /// <param name="initialProgramCounter"> </param>
        /// <returns></returns>
        public static Program LoadState(string programString, int initialProgramCounter = 0)
        {
            if (initialProgramCounter < 0) throw new ArgumentOutOfRangeException("initialProgramCounter");
            Program program = new Program(programString)
                                        {
                                            _programCounter = initialProgramCounter
                                        };
            return program;
        }
    }
}

