using System;
using System.Reflection;
using System.Linq;
using System.Reflection.Emit;
using System.IO;

namespace Bfc
{
    public class BrainmessIlGenerator
    {
        private ILGenerator _ilg;
        public BrainmessIlGenerator(ILGenerator ilg)
        {
            _ilg =ilg;
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
    }
}

