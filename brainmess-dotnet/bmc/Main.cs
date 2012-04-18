using System;
using System.Reflection;
using System.Linq;
using System.Reflection.Emit;
using System.IO;
using System.Collections.Generic;
using Bmc.Lexigraph;
namespace Bmc
{
    class MainClass
    {
        public static void Main (string[] args)
        {
            if(args.Length <2)
            {
                Console.Error.WriteLine("Usage: bmc.exe <srcfile> <output>");
                return;
            }
            var src = File.ReadAllText(args[0]);
            BrainmessCompiler(args[1],src);

        }

        private static void BrainmessCompiler(string outputPath, string program)
        {
            var generator = new BrainmessIlGenerator(outputPath,program.Count(x=>x=='['),5000);
            var containerStack = new Stack<List<IInstruction>>();
            containerStack.Push(new List<IInstruction>());
            foreach(var instruction in program)
            {
                switch(instruction)
                {
                case '>':
                    containerStack.Peek().Add(new MoveTape(1));
                    break;
                case '<':
                    containerStack.Peek().Add(new MoveTape(-1));
                    break;
                case '+':
                    containerStack.Peek().Add(new IncrementCurrentValue(1));
                    break;
                case '-':
                    containerStack.Peek().Add(new IncrementCurrentValue(-1));
                    break;
                case '.':
                    containerStack.Peek().Add(new WriteOutCurrentValue());
                    break;
                case ',':
                    containerStack.Peek().Add(new ReadAndStoreChar());
                    break;
                case '[':
                    containerStack.Push(new List<IInstruction>());
                    break;
                case ']':
                    var instructions = containerStack.Pop();
                    containerStack.Peek().Add(new WhileLoop(new InstructionContainer(instructions)));
                    break;
                }
            }
            var lexedProgram =new InstructionContainer(containerStack.Pop());

            if(containerStack.Count >0)
            {
                throw new ArgumentException("Invalid program");
            }

            lexedProgram.Emit(generator);
            generator.FinalizeProgram();
        }
    }
}
