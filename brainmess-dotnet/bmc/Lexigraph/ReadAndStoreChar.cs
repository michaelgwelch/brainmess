using System;

namespace Bmc.Lexigraph
{
    public class ReadAndStoreChar : IInstruction
    {
        public ReadAndStoreChar ()
        {
        }

        public void Emit(IGenerator codeEmittor)
        {
            codeEmittor.ReadAndStoreInput();
        }
    }
}

