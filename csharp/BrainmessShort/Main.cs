using System;
using System.IO;

namespace BrainmessShort
{
   public class Brainmess
   {
        public static void Main(string[] args)
        {       
            var reader = File.OpenText(args[0]);
            Run(reader.ReadToEnd());
            reader.Close();
        }
        
        public static void Run(string program) 
        {
            int pc = 0;
            int[] tape = new int[5000];
            int tc = 2500;
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
