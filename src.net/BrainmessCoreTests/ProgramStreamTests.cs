using System;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace Welch.Brainmess
{
    [TestClass]
    public class ProgramStreamTests
    {
        // White box testing. Each method only needs one test because there are no alternative paths.

        // The "hard" testing is done in String.FindMatch, and Instruction.FromInt


        [TestMethod]
        public void JumpForward()
        {
            // Arrange
            //                                               0123456789
            ProgramStream program = ProgramStream.LoadState("++[     ]   ", 3);

            // Act
            program.JumpForward();

            // Assert
            Assert.AreEqual(9, program.ProgramCounter);

        }

        [TestMethod]
        public void JumpBackward()
        {
            // Arrange
            //                                               0123456789
            ProgramStream program = ProgramStream.LoadState("++[     ]   ", 9);

            // Act
            program.JumpBackward();

            // Assert
            Assert.AreEqual(2, program.ProgramCounter);
        }

        // Fetch was modified to skip no ops, so now it has more complicated set of tests.
        // ReSharper disable InconsistentNaming

        [TestMethod]
        public void Fetch_NotANoOp_ShouldReturnNextInstruction()
        {
            // Arrange
            //                                               0123456789
            ProgramStream program = ProgramStream.LoadState("++[>>.>>>abc]   ", 4);

            // Act
            var instruction = program.Fetch();

            // Assert
            Assert.AreEqual(Instruction.MoveForward, instruction);
        }

        [TestMethod]
        public void Fetch_CurrentInstructionIsANoOp_ShouldSkipAndReturnNextRealInstruction()
        {
            // Arrange
            // Arrange
            //                                               0123456789
            ProgramStream program = ProgramStream.LoadState("++[>a.>>>abc]   ", 4);

            // Act
            var instruction = program.Fetch();

            // Assert
            Assert.AreEqual(Instruction.Output, instruction);
        }

        [TestMethod]
        public void Fetch_CurrentInstructionIsNoOpAndWeAreAtEndOfProgram_ShouldReturnNoOp()
        {
            ProgramStream program = new ProgramStream("a");

            // Act
            var instruction = program.Fetch();

            // Assert
            Assert.AreEqual(Instruction.NoOperation, instruction);
        }

        [TestMethod]
        public void LoadState_WithNegativeProgramCounter_ThrowsArgumentOutOfRangeException()
        {
            try
            {
                ProgramStream.LoadState("+", -1);
                Assert.Fail("Expected ArgumentOutOfRangeException");
            }
            catch (ArgumentOutOfRangeException)
            {
            }
        }


        // This is more black box testing from here. We can see from the code that only one path is taken.

        [TestMethod]
        public void EndOfProgram_WhenProgramCounterLessThanLengthOfProgram_ShouldReturnFalse()
        {
            // Arrange
            //                                               0123456789
            ProgramStream program = ProgramStream.LoadState("++[>>.>>]", 8);

            // Act
            var endOfProgram = program.EndOfProgram;

            // Assert
            Assert.IsFalse(endOfProgram);
        }

        [TestMethod]
        public void EndOfProgram_WhenProgramCounterNotLessThanLengthOfProgram_ShouldReturnTrue()
        {
            // Arrange
            //                                               0123456789
            ProgramStream program = ProgramStream.LoadState("++[>>.>>]", 9);

            // Act
            var endOfProgram = program.EndOfProgram;

            // Assert
            Assert.IsTrue(endOfProgram);
        }
        // ReSharper restore InconsistentNaming
    }
}
