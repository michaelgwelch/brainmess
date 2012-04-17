using System;
using System.Linq;
using System.Collections.Generic;

namespace Bmc.Lexigraph
{
    public class WhileLoop : IInstruction
    {
        private IEnumerable<IInstruction> _instructions;
        public WhileLoop (IEnumerable<IInstruction> instructions)
        {
            _instructions = instructions;
        }

        public void Emit(IGenerator codeEmittor)
        {
            codeEmittor.BeginLoop();
            foreach(var i in instructions)
            {
                i.Emit(codeEmittor);
            }
            codeEmittor.EndLoop();
        }
    }
}

