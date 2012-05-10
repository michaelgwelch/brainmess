using System;
using NUnit.Framework;
using Bmc.Lexigraph;

namespace bmcTests
{
    [TestFixture]
    public class MoveTapeTests
    {
        [Test]
        public void Inc_Tape_By_One ()
        {
            var instruction = new MoveTape(1);
            var m = new MockGenerator();
            instruction.Emit(m);
            Assert.AreEqual(1, m.TapeAdvances);
        }

        [Test]
        public void Dec_Tape_By_One ()
        {
            var instruction = new MoveTape(-1);
            var m = new MockGenerator();
            instruction.Emit(m);
            Assert.AreEqual(1, m.TapeBacks);
        }
    }
}

