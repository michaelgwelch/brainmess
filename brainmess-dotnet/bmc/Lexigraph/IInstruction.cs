using System;
using Bmc;

namespace Bmc.Lexigraph
{
    public interface IInstruction
    {
        void Emit(IGenerator codeEmittor);
    }
}

