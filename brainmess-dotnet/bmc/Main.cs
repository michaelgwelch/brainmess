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
                Console.Error.WriteLine("Usage: bcf.exe <srcfile> <output>");
                return;
            }

            //The first argument is the path of the brainmess file to be compiled to il
            AssemblyName an = new AssemblyName();
            an.Name = "brainmess";
            AppDomain ad = AppDomain.CurrentDomain;
            AssemblyBuilder ab = ad.DefineDynamicAssembly(an, AssemblyBuilderAccess.Save);

            var outputFile = new FileInfo(args[1]);
            ModuleBuilder mb = ab.DefineDynamicModule(an.Name, outputFile.Name);

            TypeBuilder tb = mb.DefineType("CompiledBrainmess",
            TypeAttributes.Public|TypeAttributes.Class);

            MethodBuilder fb = tb.DefineMethod("Main",
            MethodAttributes.Public|
            MethodAttributes.Static,
            typeof(int), new Type[] { typeof(string[]) });


            var src = File.ReadAllText(args[0]);
            BrainmessCompiler(fb.GetILGenerator(),src);


            // Time to finish writing the type
            tb.CreateType();
            // Set the entrypoint (thereby declaring it an EXE)
            ab.SetEntryPoint(fb,PEFileKinds.ConsoleApplication);


            // Save it
            ab.Save(args[1]);
        }

        private static void BrainmessCompiler(ILGenerator ilg, string program)
        {
            var generator = new BrainmessIlGenerator(ilg,program.Count(x=>x=='['),5000);
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
