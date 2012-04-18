using System;
using Bmc.Lexigraph;
using NUnit.Framework;

namespace bmcTests
{
    [TestFixture]
    public class InstructionContainerTests
    {
        [Test]
        public void InstructionContainer_IncVal_DecVal()
        {
            var instruction = new InstructionContainer(new []
                        {
                            new IncrementCurrentValue(1), new IncrementCurrentValue(-1)
                        });
            var m = new MockGenerator();
            instruction.Emit(m);
            Assert.AreEqual(1, m.ValueDecrements);
            Assert.AreEqual(1, m.ValueIncrements);
        }
    }
}

