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

            int nextNestMarkerIndex =0;//The index of the next [ label nestLevel-1 is the index of the most recent [

            var nestCount = program.Count(x=>x=='[');//How many nestings are there?
            NestHelper[] nests = new NestHelper[nestCount];
            for(int i =0; i<nestCount; i++)
            {
                nests[i] =new  NestHelper(){LoopStart = ilg.DefineLabel(), LoopEnd=ilg.DefineLabel(), HasBeenClosed =false};
            }

            //initialize tape array
            ilg.Emit(OpCodes.Ldc_I4, 5000);
            ilg.Emit(OpCodes.Newarr, typeof(int));
            ilg.Emit(OpCodes.Stloc,0);
            //initialize indexer into array (start at 2500)
            ilg.Emit(OpCodes.Ldc_I4,2500);
            ilg.Emit(OpCodes.Stloc,1);

            foreach(var instruction in program)
            {
                switch(instruction)
                {
                case '>':
                    ilg.Emit(OpCodes.Ldloc_S, 1);
                    ilg.Emit(OpCodes.Ldc_I4_S, 1);
                    ilg.Emit(OpCodes.Add);
                    ilg.Emit(OpCodes.Stloc,1);
                    break;
                case '<':
                    ilg.Emit(OpCodes.Ldloc_S, 1);
                    ilg.Emit(OpCodes.Ldc_I4_S, 1);
                    ilg.Emit(OpCodes.Sub);
                    ilg.Emit(OpCodes.Stloc,1);
                    break;
                case '+':
                    ilg.Emit(OpCodes.Ldloc_S, 0);
                    ilg.Emit(OpCodes.Ldloc_S, 1);

                    ilg.Emit(OpCodes.Ldloc_S, 0);
                    ilg.Emit(OpCodes.Ldloc_S, 1);
                    ilg.Emit(OpCodes.Ldelem_I4);
                    //Time to add 1 and restore value
                    ilg.Emit(OpCodes.Ldc_I4_S, 1);
                    ilg.Emit(OpCodes.Add);
                    ilg.Emit(OpCodes.Stelem_I4);

                    break;
                case '-':
                    ilg.Emit(OpCodes.Ldloc_S, 0);
                    ilg.Emit(OpCodes.Ldloc_S, 1);

                    ilg.Emit(OpCodes.Ldloc_S, 0);
                    ilg.Emit(OpCodes.Ldloc_S, 1);
                    ilg.Emit(OpCodes.Ldelem_I4);
                    //Time to subtract 1 and restore value
                    ilg.Emit(OpCodes.Ldc_I4_S, 1);
                    ilg.Emit(OpCodes.Sub);
                    ilg.Emit(OpCodes.Stelem_I4);
                    break;
                case '.':
                    //write tape[tc] to console
                    ilg.Emit(OpCodes.Ldloc_S, 0);
                    ilg.Emit(OpCodes.Ldloc_S, 1);
                    ilg.Emit(OpCodes.Ldelem_I4);
                    ilg.Emit(OpCodes.Call, typeof(Console).GetMethod("Write", new Type[] {typeof(char)} ));
                    break;
                case ',':
                    //Not supported yet
                    //tape[tc] = Console.Read();
                    ilg.Emit(OpCodes.Ldloc_S, 0);
                    ilg.Emit(OpCodes.Ldloc_S, 1);
                    ilg.Emit(OpCodes.Call, typeof(Console).GetMethod("Read", new Type[] {} ));
                    ilg.Emit(OpCodes.Stelem_I4);
                    break;
                case '[':
                    ilg.MarkLabel(nests[nextNestMarkerIndex].LoopStart);
                    ilg.Emit(OpCodes.Ldloc_S, 0);
                    ilg.Emit(OpCodes.Ldloc_S, 1);
                    ilg.Emit(OpCodes.Ldelem_I4);
                    ilg.Emit(OpCodes.Ldc_I4_S, 0);
                    ilg.Emit(OpCodes.Beq,nests[nextNestMarkerIndex].LoopEnd);//if the current tape value == 0 jump past loop end

                    nextNestMarkerIndex++;
                    break;
                case ']':
                    //if current index on tape !=0 jump to denestLevel;
                    NestHelper nestBeingClosed = nests.Take(nextNestMarkerIndex).Last(x=>!x.HasBeenClosed);
                    nestBeingClosed.HasBeenClosed = true;
                    ilg.Emit(OpCodes.Br,nestBeingClosed.LoopStart);
                    ilg.MarkLabel(nestBeingClosed.LoopEnd);
                    break;
                }
            }
            ilg.Emit(OpCodes.Ldc_I4_0);
            ilg.Emit(OpCodes.Ret);//return 0
        }
        private class NestHelper
        {
            public Label LoopEnd;
            public Label LoopStart;
            public bool HasBeenClosed;
        }
    }
}
