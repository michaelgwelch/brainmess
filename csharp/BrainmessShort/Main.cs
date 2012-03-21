using System;
using System.IO;

namespace BrainmessShort
{
   public class Brainmess
   {
        private readonly string program;
        private int pc = 0;
        private readonly int[] tape = new int[5000];
        private int tc = 2500;
        public Brainmess(string program) 
        {
            this.program = program;
        }
        
        public static void Main(string[] args)
        {       
            var reader = File.OpenText(args[0]);
            new Brainmess(reader.ReadToEnd()).Run();
            reader.Close();
        }
        
        public void Run() 
        {
            while(pc < program.Length)
            {
                char instruction = program[pc];
                pc++;
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
                        pc = program.FindMatch(pc - 1) + 1;
                    }
                        break;
                case ']':
                    if (tape[tc] != 0)
                    {
                        pc = program.FindMatch(pc - 1);
                    }
                    break;
                }
            }
        }
   }
}
