using System;
using System.Text;
using System.Collections.Generic;
using System.Linq;
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

        [TestMethod]
        public void Fetch()
        {
            // Arrange
            //                                               0123456789
            ProgramStream program = ProgramStream.LoadState("++[>>.>>>abc]   ", 4);

            // Act
            var instruction = program.Fetch();

            // Assert
            Assert.AreEqual(Instruction.MoveForward, instruction);
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
    }
}
