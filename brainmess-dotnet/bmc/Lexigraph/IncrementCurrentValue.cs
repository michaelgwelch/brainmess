using System;

namespace Bmc.Lexigraph
{
    public class IncrementCurrentValue: IInstruction
    {
        private int _x;
        public IncrementCurrentValue (int x)
        {
            _x=x;
        }

        public void Emit(IGenerator codeEmittor)
        {
            codeEmittor.AddValue(x);
        }
    }
}

