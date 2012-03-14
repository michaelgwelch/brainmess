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
                        pc = JumpForward(program, pc);
                    }
                        break;
                case ']':
                    if (tape[tc] != 0)
                    {
                        pc = JumpBackward(program, pc);
                    }
                    break;
                }
            }
        }

       private static int JumpForward(string program, int pc)
       {
           const int increment = 1;
           return FindMatch(program, pc - 1, increment) + 1;
       }

       private static int JumpBackward(string program, int pc)
       {
           const int increment = -1;
           return FindMatch(program, pc - 1, increment);
       }

       /// <summary>
       /// Finds the match for the bracket pointed to by
       /// pc in the program. Increment tells the algorithm
       /// which way to search.
       /// </summary>
       /// <param name="program"></param>
       /// <param name="pc"></param>
       /// <param name="increment"></param>
       /// <returns></returns>
       private static int FindMatch(string program, int pc, int increment)
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
   }
}
