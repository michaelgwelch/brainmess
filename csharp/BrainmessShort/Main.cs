using System;
using System.IO;

namespace BrainmessShort
{
   public class Brainmess
   {
        private readonly Program _program;
        private readonly Tape _tape;

        public Brainmess(string programString) 
        {
            _program = new Program(programString);
            _tape = new Tape();
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
                    _tape.MoveForward();
                    break;
                case '<':
                    _tape.MoveBackward();
                    break;
                case '+':
                    _tape.Increment();
                    break;
                case '-':
                    _tape.Decrement();
                    break;
                case '.':
                    Console.Write((char)_tape.Current);
                    break;
                case ',':
                    _tape.Current = Console.Read();
                    break;
                case '[':
                    if (_tape.Current == 0)
                    {
                        _program.JumpForward();
                    }
                        break;
                case ']':
                    if (_tape.Current != 0)
                    {
                        _program.JumpBackward();
                    }
                    break;
                }
            }
        }
   }
}
