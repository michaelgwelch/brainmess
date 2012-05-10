using System;

namespace Bmc.Lexigraph
{
    public class WriteOutCurrentValue : IInstruction
    {
        public WriteOutCurrentValue ()
        {
        }
        public void Emit(IGenerator codeEmittor)
        {
            codeEmittor.WriteCurrent();
        }
    }
}

