using System;
using System.IO;

namespace Welch.Brainmess
{
    /// <summary>
    /// Represents a Brainmess interpreter.
    /// </summary>
    public class Interpreter
    {
        /// <summary>
        /// Creates an interpreter that runs the empty program.
        /// </summary>
        public Interpreter() : this(new Program(""))
        {
            
        }
        /// <summary>
        /// Creates an instance of an Interpreter. Each of the parameters except program is optional..
        /// </summary>
        /// <param name="program">The program to execute. If this is null, the empty program is executed.</param>
        /// <param name="tape">The tape to use as a memory story. If this is null, a default tape is used.</param>
        /// <param name="input">The input source to use. If this is null then Console.In is used.</param>
        /// <param name="output">The output source to use. If this is null then Console.Out is used.</param>
        public Interpreter(IProgram program, Tape tape = null, TextReader input = null, TextWriter output = null)
        {
            _program = program;
            _tape = tape ?? Tape.Default;
            _input = input ?? Console.In;
            _output = output ?? Console.Out;
        }

        private readonly IProgram _program;
        private readonly Tape _tape;
        private readonly TextWriter _output;
        private readonly TextReader _input;


        /// <summary>
        /// Runs the program given in the constructor within the context given
        /// in the constructor.
        /// </summary>
        public void Run()
        {
            while (!_program.EndOfProgram)
            {
                Instruction currentInstruction = _program.Fetch();
                currentInstruction.Execute(_program, _tape, _input, _output);
            }

        }

    }
}

