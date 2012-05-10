using System;
using System.Linq;
using System.Collections.Generic;

namespace Bmc.Lexigraph
{
    public class WhileLoop : IInstruction
    {
        private InstructionContainer _instructions;
        public WhileLoop (InstructionContainer instructions)
        {
            _instructions = instructions;
        }

        public void Emit(IGenerator codeEmittor)
        {
            codeEmittor.BeginLoop();
            _instructions.Emit(codeEmittor);
            codeEmittor.EndLoop();
        }
    }
}

