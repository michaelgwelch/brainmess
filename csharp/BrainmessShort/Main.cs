using System;
using System.IO;

namespace BrainmessShort
{
   public class Brainmess
   {
        private readonly Program _program;
        
        private readonly int[] tape = new int[5000];
        private int tc = 2500;
        public Brainmess(string programString) 
        {
            _program = new Program(programString);
        }
        
        public static void Main(string[] args)
        {       
            var reader = File.OpenText(args[0]);
            new Brainmess(reader.ReadToEnd()).Run();
            reader.Close();
        }
        
        public void Run() 
        {
            while(!_program.EndOfProgram)
            {
                char instruction = _program.Fetch();
                switch(instruction)
                {
                case '>': 
                    tc++;
                    break;
                case '<':
                    tc--;
                    break;
                case '+':
                    tape[tc]++;
                    break;
                case '-':
                    tape[tc]--;
                    break;
                case '.':
                    Console.Write((char)tape[tc]);
                    break;
                case ',':
                    tape[tc] = Console.Read();
                    break;
                case '[':
                    if (tape[tc] == 0)
                    {
                        _program.JumpForward();
                    }
                        break;
                case ']':
                    if (tape[tc] != 0)
                    {
                        _program.JumpBackward();
                    }
                    break;
                }
            }
        }
   }
}
