using System;
using NUnit.Framework;
namespace BrainmessShort
{
    [TestFixture]
    public class ProgramStreamTests
    {
        // White box testing. Each method only needs one test because there are no alternative paths.
        // The "hard" testing is done for StringExtensions.FindMatch
        
        
        [Test]
        public void JumpForward()
        {
            // Arrange
            //                                   0123456789
            Program program = Program.LoadState("++[     ]   ", 3);

            // Act
            program.JumpForward();

            // Assert
            Assert.AreEqual(9, program.ProgramCounter);

        }

        [Test]
        public void JumpBackward()
        {
            // Arrange
            //                                   0123456789
            Program program = Program.LoadState("++[     ]   ", 9);

            // Act
            program.JumpBackward();

            // Assert
            Assert.AreEqual(2, program.ProgramCounter);
        }

        [Test]
        public void Fetch__ShouldReturnNextInstruction()
        {
            // Arrange
            //                                   0123456789
            Program program = Program.LoadState("++[>>.>>>abc]   ", 4);

            // Act
            var instruction = program.Fetch();

            // Assert
            Assert.AreEqual('>', instruction);
        }

    }

}



