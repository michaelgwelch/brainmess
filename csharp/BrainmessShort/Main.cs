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

        int MoveForward()
        {
            return tc++;
        }

        int MoveBackward()
        {
            return tc--;
        }

        int Increment()
        {
            return tape[tc]++;
        }

        int Decrement()
        {
            return tape[tc]--;
        }

        int Current
        {
            get
            {
                return tape[tc];
            }
            set
            {
                tape[tc] = value;
            }
        }
        
        public void Run() 
        {
            while(!_program.EndOfProgram)
            {
                char instruction = _program.Fetch();
                switch(instruction)
                {
                case '>': 
                    MoveForward();
                    break;
                case '<':
                    MoveBackward();
                    break;
                case '+':
                    Increment();
                    break;
                case '-':
                    Decrement();
                    break;
                case '.':
                    Console.Write((char)Current);
                    break;
                case ',':
                    Current = Console.Read();
                    break;
                case '[':
                    if (Current == 0)
                    {
                        _program.JumpForward();
                    }
                        break;
                case ']':
                    if (Current != 0)
                    {
                        _program.JumpBackward();
                    }
                    break;
                }
            }
        }
   }
}
