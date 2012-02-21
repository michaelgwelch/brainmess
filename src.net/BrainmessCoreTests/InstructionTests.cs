using System.IO;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Moq;

namespace Welch.Brainmess
{
    [TestClass]
    public class InstructionTests
    {
        // this is doing White Box testing. It takes advantage of the fact that the real logic
        // is done in other classes, Tape, Program, etc. Most of the instructions are one line
        // and need only one test. The bulk of the testing is then done on Tape and Program

        private static readonly Program NullProgram = Identity<Program>(null);
        private static readonly TextReader NullInput = Identity<TextReader>(null);
        private static readonly TextWriter NullOutput = Identity<TextWriter>(null);

        // ReSharper disable InconsistentNaming
        [TestMethod]
        public void MoveForwardExecution_StartingAtPosition1_ShouldEndUpAtTwo()
        {
            // Arrange
            const int startingPosition = 1;
            var expectedTape = Tape.LoadState(new[] { 1, 3, 5 }, 2);
            var actualTape = Tape.LoadState(new[] { 1, 3, 5 }, startingPosition);

            // Act
            Instruction.MoveForward.Execute(NullProgram, actualTape, NullInput, NullOutput);

            // Assert
            Assert.AreEqual(expectedTape, actualTape);
        }

        [TestMethod]
        public void MoveBackwardExecution_StartingAtPosition1_ShouldEndUpAtZero()
        {
            // Arrange
            const int startingPosition = 1;
            var expectedTape = Tape.LoadState(new[] { 1, 3, 5 }, 0);
            var actualTape = Tape.LoadState(new[] { 1, 3, 5 }, startingPosition);

            // Act
            Instruction.MoveBackward.Execute(NullProgram, actualTape, NullInput, NullOutput);

            // Assert
            Assert.AreEqual(expectedTape, actualTape);
        }

        [TestMethod]
        public void IncrementExecution_ShouldIncrementCurrentCell()
        {
            // Arrange
            var expectedTape = Tape.LoadState(new[] { 5, 8, 9 }, 1);
            var actualTape = Tape.LoadState(new[] { 5, 7, 9 }, 1);

            // Act
            Instruction.Increment.Execute(NullProgram, actualTape, NullInput, NullOutput);

            // Assert - Expect number at index 1 to have been incrmented
            Assert.AreEqual(expectedTape, actualTape);
        }

        [TestMethod]
        public void DecrementExecution_AtIndex1_ShouldDecrementTheValueAtIndex1()
        {
            // Arrange
            var expectedTape = Tape.LoadState(new[] { 5, 6, 9 }, 1);
            var actualTape = Tape.LoadState(new[] { 5, 7, 9 }, 1);

            // Act
            Instruction.Decrement.Execute(NullProgram, actualTape, NullInput, NullOutput);

            // Assert - Expect number at index 1 to have been incrmented
            Assert.AreEqual(expectedTape, actualTape);
        }

        [TestMethod]
        public void InputExecution_ShouldReadFromInputAtWriteToTape()
        {
            // Arrange
            var expectedTape = Tape.LoadState(new[] { 10, 65, 12 }, 1);
            var actualTape = Tape.LoadState(new[] { 10, 11, 12 }, 1);
            var input = CreateReaderWithNextCharacterEqualTo((char)65);

            // Act
            Instruction.Input.Execute(NullProgram, actualTape, input, NullOutput);

            // Assert
            Assert.AreEqual(expectedTape, actualTape);

        }

        [TestMethod]
        public void OutputExecution_ShouldReadFromTapeAndWriteToOutput()
        {
            // Arrange
            var tape = Tape.LoadState(new[] { 55, 57, 59 }, 1);
            var stream = new MemoryStream();
            var output = new StreamWriter(stream)
                             {
                                 AutoFlush = true
                             };

            // Act
            Instruction.Output.Execute(NullProgram, tape, NullInput, output);

            // Assert
            var bytes = stream.ToArray();
            output.Close();
            var reader = new StreamReader(new MemoryStream(bytes));
            Assert.AreEqual(57, reader.Read());
        }

        
        [TestMethod]
        public void TestAndJumpFowardExecution_WithTapeNotEqualToZero_ShouldDoNothing()
        {
            // Arrange
            var mock = new Mock<IProgram>(MockBehavior.Strict); // any call to program should cause failure.
            var actualTape = Tape.LoadState(new[] { 1 }, 0);
            var expectedTape = Tape.LoadState(new[] { 1 }, 0);

            // Act
            Instruction.TestAndJumpForward.Execute(mock.Object, actualTape, NullInput, NullOutput);

            // Assert
            Assert.AreEqual(expectedTape, actualTape); // No change to tape
        }

        [TestMethod]
        public void TestAndJumpForwardExecution_WithTapeEqualToZero_ShouldJump()
        {
            // Arrange
            var tape = Tape.Default;
            var mock = new Mock<IProgram>(MockBehavior.Strict);
            mock.Setup(program => program.JumpForward());

            // Act
            Instruction.TestAndJumpForward.Execute(mock.Object, tape, NullInput, NullOutput);

            // Assert
            mock.VerifyAll();

        }

        [TestMethod]
        public void TestAndJumpBackwardExecution_WithTapeEqualToZero_ShouldDoNothing()
        {
            // Arrange
            var mock = new Mock<IProgram>(MockBehavior.Strict);
            var expectedTape = Tape.LoadState(new[] { 0, 0, 0 }, 1);
            var actualTape = Tape.LoadState(new[] { 0, 0, 0 }, 1);

            // Act
            Instruction.TestAndJumpBackward.Execute(mock.Object, actualTape, NullInput, NullOutput);

            // Assert
            Assert.AreEqual(expectedTape, actualTape);
        }

        [TestMethod]
        public void TestAndJumpBackwardExecution_WithTapeNotEqualToZero_ShouldJump()
        {
            // Arrange
            var tape = Tape.LoadState(new[] { 1, 2, 3 }, 1);
            var mock = new Mock<IProgram>(MockBehavior.Strict);
            mock.Setup(program => program.JumpBackward());

            // Act
            Instruction.TestAndJumpBackward.Execute(mock.Object, tape, NullInput, NullOutput);

            // Assert
            mock.VerifyAll();

        }

        [TestMethod]
        public void NoOperation_WhenExecuted_ShouldDoNothing()
        {
            // Arrange

            // Act
            Instruction.NoOperation.Execute(NullProgram, null, NullInput, NullOutput);

            // Assert
            
            // if no exceptions, then we succeeded since everything passed to Execute was null

        }

        [TestMethod]
        public void FromInt_WithMoveForwardChar_ExpectMoveForwardInstruction()
        {
            AssertInstructionReturnedForChar(Instruction.MoveForward, '>');
        }

        [TestMethod]
        public void FromInt_WithMoveBackwardChar_ExpectMoveBackwardInstruction()
        {
            AssertInstructionReturnedForChar(Instruction.MoveBackward, '<');
        }

        [TestMethod]
        public void FromInt_WithOutputChar_ExpectOutputInstruction()
        {
            AssertInstructionReturnedForChar(Instruction.Output, '.');
        }

        [TestMethod]
        public void FromInt_WithInputChar_ExpectInputInstruction()
        {
            AssertInstructionReturnedForChar(Instruction.Input, ',');
        }

        [TestMethod]
        public void FromInt_WithIncrementChar_ExpectIncrementInstruction()
        {
            AssertInstructionReturnedForChar(Instruction.Increment, '+');
        }

        [TestMethod]
        public void FromInt_WithDecrementChar_ExpectDecrementInstruction()
        {
            AssertInstructionReturnedForChar(Instruction.Decrement, '-');
        }

        [TestMethod]
        public void FromInt_WithTestAndJumpForwardChar_ExpectTestAndJumpForwardInstruction()
        {
            AssertInstructionReturnedForChar(Instruction.TestAndJumpForward, '[');
        }

        [TestMethod]
        public void FromInt_WithTestAndJumpBackwardChar_ExpectTestAndJumpBackwardInstruction()
        {
            AssertInstructionReturnedForChar(Instruction.TestAndJumpBackward, ']');
        }

        [TestMethod]
        public void FromInt_WithA_ExpectNoOperationInstruction()
        {
            AssertInstructionReturnedForChar(Instruction.NoOperation, 'A');
        }

        private static void AssertInstructionReturnedForChar(Instruction expectedInstruction, int characterValue)
        {
            // Arrange
            var expected = expectedInstruction;

            // Act
            var actual = Instruction.FromInt(characterValue);

            // Assert
            Assert.AreEqual(expected, actual);
        }

        private static TextReader CreateReaderWithNextCharacterEqualTo(char c)
        {
            MemoryStream stream = new MemoryStream();
            StreamWriter writer = new StreamWriter(stream);
            writer.Write(c);
            writer.Close();

            MemoryStream newStream = new MemoryStream(stream.ToArray());
            return new StreamReader(newStream);

        }

        /// <summary>
        /// The Identity function. Returns the specified value.
        /// Useful for quieting Resharper warnings about null.
        /// </summary>
        public static T Identity<T>(T value)
        {
            return value;
        }

        // ReSharper restore InconsistentNaming
    }
}
