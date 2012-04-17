using System;
using System.Collections.Generic;

namespace Bmc.Lexigraph
{
    public class InstructionContainer: IInstruction
    {
        private IEnumerable<IInstruction> _instructions;
        public InstructionContainer (IEnumerable<IInstruction> instructions)
        {
            _instructions = instructions;
        }

        public void Emit(IGenerator codeEmittor)
        {
            codeEmittor.BeginLoop();
            foreach(var i in _instructions)
            {
                i.Emit(codeEmittor);
            }
            codeEmittor.EndLoop();
        }
    }
}

