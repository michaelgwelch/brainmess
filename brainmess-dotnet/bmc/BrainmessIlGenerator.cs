using System;
using System.Reflection;
using System.Linq;
using System.Reflection.Emit;
using System.IO;

namespace Bmc
{
    public class BrainmessIlGenerator
    {
        private ILGenerator _ilg;
        private AssemblyBuilder _ab;
        private TypeBuilder _tb;
        private MethodBuilder _fb;

        private Loop[] _loops;
        private int _nextUnusedLoopIndex =0;
        private string _outputFilePath;
        public BrainmessIlGenerator(string ouputFilePath, int loopCount, int tapeLength)
        {
            _outputFilePath = ouputFilePath;
            _loops = new Loop[loopCount];
            InitializeProgram(tapeLength);
        }
        private void InitializeProgram(int tapeLength)
        {
            AssemblyName an = new AssemblyName();
            an.Name = "brainmess";
            AppDomain ad = AppDomain.CurrentDomain;
            _ab = ad.DefineDynamicAssembly(an, AssemblyBuilderAccess.Save);

            var outputFile = new FileInfo(_outputFilePath);
            ModuleBuilder mb = _ab.DefineDynamicModule(an.Name, outputFile.Name);

            _tb = mb.DefineType("CompiledBrainmess",
            TypeAttributes.Public|TypeAttributes.Class);


            _fb = _tb.DefineMethod("Main",
            MethodAttributes.Public|
            MethodAttributes.Static, typeof(int), new Type[] { typeof(string[]) });
            _ilg = _fb.GetILGenerator();

            //no program counter is needed as clr has that built in
            //initialize int array (tapeLength) in size
            LocalBuilder tapeVar = _ilg.DeclareLocal(typeof(int[]));//index 0 local var
            tapeVar.SetLocalSymInfo("tape");
            LocalBuilder tcVar = _ilg.DeclareLocal(typeof(int));//index 1 local var
            tcVar.SetLocalSymInfo("tc");

            for(int i =0; i<_loops.Length; i++)
            {
                _loops[i] =new  Loop(){LoopStart = _ilg.DefineLabel(), LoopEnd=_ilg.DefineLabel(), HasBeenClosed =false};
            }

            //initialize tape array
            _ilg.Emit(OpCodes.Ldc_I4, tapeLength);
            _ilg.Emit(OpCodes.Newarr, typeof(int));
            _ilg.Emit(OpCodes.Stloc,0);
            //initialize indexer into array (start at tapeLength/2)
            _ilg.Emit(OpCodes.Ldc_I4,tapeLength/2);
            _ilg.Emit(OpCodes.Stloc,1);
        }
        public void FinalizeProgram()
        {
            _ilg.Emit(OpCodes.Ldc_I4_0);
            _ilg.Emit(OpCodes.Ret);//return 0

            // Time to finish writing the type
            _tb.CreateType();
            // Set the entrypoint (thereby declaring it an EXE)
            _ab.SetEntryPoint(_fb,PEFileKinds.ConsoleApplication);


            // Save it
            _ab.Save(_outputFilePath);
        }

        public void MoveTape(int x)
        {
            _ilg.Emit(OpCodes.Ldloc_S, 1);
            _ilg.Emit(OpCodes.Ldc_I4_S, Math.Abs(x));
            if(x <0)
            {
                _ilg.Emit(OpCodes.Sub);
            }
            else
            {
                _ilg.Emit(OpCodes.Add);
            }
            _ilg.Emit(OpCodes.Stloc,1);
        }

        public void AddValue(int x)
        {
            _ilg.Emit(OpCodes.Ldloc_S, 0);
            _ilg.Emit(OpCodes.Ldloc_S, 1);

            _ilg.Emit(OpCodes.Ldloc_S, 0);
            _ilg.Emit(OpCodes.Ldloc_S, 1);
            _ilg.Emit(OpCodes.Ldelem_I4);

            _ilg.Emit(OpCodes.Ldc_I4_S, Math.Abs(x));
            if(x <0)
            {
                _ilg.Emit(OpCodes.Sub);
            }
            else
            {
                _ilg.Emit(OpCodes.Add);
            }
            _ilg.Emit(OpCodes.Stelem_I4);
        }

        public void  WriteCurrent()
        {
            _ilg.Emit(OpCodes.Ldloc_S, 0);
            _ilg.Emit(OpCodes.Ldloc_S, 1);
            _ilg.Emit(OpCodes.Ldelem_I4);
            _ilg.Emit(OpCodes.Call, typeof(Console).GetMethod("Write", new Type[] {typeof(char)} ));
        }

        public void ReadAndStoreInput()
        {
            _ilg.Emit(OpCodes.Ldloc_S, 0);
            _ilg.Emit(OpCodes.Ldloc_S, 1);
            _ilg.Emit(OpCodes.Call, typeof(Console).GetMethod("Read", new Type[] {} ));
            _ilg.Emit(OpCodes.Stelem_I4);
        }

        public void BeginLoop()
        {
            _ilg.MarkLabel(_loops[_nextUnusedLoopIndex].LoopStart);
            _ilg.Emit(OpCodes.Ldloc_S, 0);
            _ilg.Emit(OpCodes.Ldloc_S, 1);
            _ilg.Emit(OpCodes.Ldelem_I4);
            _ilg.Emit(OpCodes.Ldc_I4_S, 0);
            _ilg.Emit(OpCodes.Beq,_loops[_nextUnusedLoopIndex].LoopEnd);//if the current tape value == 0 jump past loop end

            _nextUnusedLoopIndex++;
        }
        public void EndLoop()
        {
            Loop loopBeingClosed = _loops.Take(_nextUnusedLoopIndex).Last(x=>!x.HasBeenClosed);
            loopBeingClosed.HasBeenClosed = true;
            _ilg.Emit(OpCodes.Br,loopBeingClosed.LoopStart);
            _ilg.MarkLabel(loopBeingClosed.LoopEnd);
        }

        private class Loop
        {
            public Label LoopEnd;
            public Label LoopStart;
            public bool HasBeenClosed;
        }
    }

}

