using System;
using System.Reflection;
using System.Linq;
using System.Reflection.Emit;
using System.IO;
namespace Bfc
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


            // Emit the ubiquitous "Hello, World!" method, in IL
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
            //no program counter is needed as clr has that built in
            //initialize int array (5000) in size
            LocalBuilder tapeVar = ilg.DeclareLocal(typeof(int[]));//index 0 local var
            tapeVar.SetLocalSymInfo("tape");
            LocalBuilder tcVar = ilg.DeclareLocal(typeof(int));//index 1 local var
            tcVar.SetLocalSymInfo("tc");

            int nestLevel =0;//The index of the next [ label nestLevel-1 is the index of the most recent [

            var nestCount = program.Count(x=>x=='[');//How many nestings are there?
            NestHelper[] nests = new NestHelper[nestCount];
            for(int i =0; i<nestCount; i++)
            {
                nests[i] =new  NestHelper(){JmpLabel = ilg.DefineLabel(), HasBeenClosed =false};
            }

            //initialize tape array
            ilg.Emit(OpCodes.Ldc_I4, 5000);
            ilg.Emit(OpCodes.Newarr, typeof(int));
            ilg.Emit(OpCodes.Stloc,0);
            //initialize indexer into array (start at 2500)
            ilg.Emit(OpCodes.Ldc_I4,2500);
            ilg.Emit(OpCodes.Stloc,1);
            var generator = new BrainmessIlGenerator(ilg);
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
                    ilg.MarkLabel(nests[nestLevel].JmpLabel);
                    nestLevel++;
                    break;
                case ']':
                    ilg.Emit(OpCodes.Ldloc_S, 0);
                    ilg.Emit(OpCodes.Ldloc_S, 1);
                    ilg.Emit(OpCodes.Ldelem_I4);
                    ilg.Emit(OpCodes.Ldc_I4_S, 0);
                    //if current index on tape !=0 jump to denestLevel;
                    NestHelper nestBeingClosed = nests.Take(nestLevel).Last(x=>!x.HasBeenClosed);
                    nestBeingClosed.HasBeenClosed = true;
                    ilg.Emit(OpCodes.Bne_Un,nestBeingClosed.JmpLabel);
                    break;
                }
            }
            ilg.Emit(OpCodes.Ldc_I4_0);
            ilg.Emit(OpCodes.Ret);//return 0
        }
        private class NestHelper
        {
            public Label JmpLabel;
            public bool HasBeenClosed;
        }
    }
}
