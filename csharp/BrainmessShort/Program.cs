using System;

namespace BrainmessShort
{
    public class Program
    {
        private readonly string program;
        private int pc;
        
        public Program(string program)
        {
            this.program = program;
            pc = 0;
        }
        
        public bool EndOfProgram
        {
            get
            {
                return pc >= program.Length;
            }
        }
        
        public char Fetch()
        {
            var instruction = program[pc];
            pc++;
            return instruction;
        }

        public void JumpForward()
        {
            pc = program.FindMatch(pc - 1) + 1;
        }

        public void JumpBackward()
        {
            pc = program.FindMatch(pc - 1);
        }
        
        public int ProgramCounter
        {
            get
            {
                return pc;
            }
        }
    }
}

