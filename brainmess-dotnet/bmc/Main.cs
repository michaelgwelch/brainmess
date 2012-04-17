using System;
using System.Reflection;
using System.Linq;
using System.Reflection.Emit;
using System.IO;
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
            foreach(var instruction in program)
            {
                switch(instruction)
                {
                case '>':
                    generator.MoveTape(1);
                    break;
                case '<':
                    generator.MoveTape(-1);
                    break;
                case '+':
                    generator.AddValue(1);
                    break;
                case '-':
                    generator.AddValue(-1);
                    break;
                case '.':
                    generator.WriteCurrent();
                    break;
                case ',':
                    generator.ReadAndStoreInput();
                    break;
                case '[':
                    generator.BeginLoop();
                    break;
                case ']':
                    generator.EndLoop();
                    break;
                }
            }
            generator.FinalizeProgram();
        }
    }
}
