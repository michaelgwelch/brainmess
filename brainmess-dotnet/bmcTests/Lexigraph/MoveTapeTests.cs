using System;
using NUnit.Framework;
using Bmc.Lexigraph;

namespace bmcTests
{
	[TestFixture()]
    public class MoveTapeTests
    {
		[Test()]
        public void Advance_Tape_By_One ()
        {
            var tapeMove = new MoveTape(1);
            var m = new MockGenerator();
            tapeMove.Emit(m);
            Assert.AreEqual(1, m.ValueIncrements);
        }
    }
}

