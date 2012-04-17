using System;

namespace Bmc.Lexigraph
{
    public class MoveTape : IInstruction
    {
        private int _x;
        public MoveTape (int x)
        {
            _x=x;
        }

        public void Emit(IGenerator codeEmittor)
        {
            codeEmittor.MoveTape(_x);
        }
    }
}

