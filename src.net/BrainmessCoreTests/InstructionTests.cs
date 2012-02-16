using System.IO;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Moq;

namespace Welch.Brainmess
{
    [TestClass]
    public class InstructionTests
    {
        // this is doing White Box testing. It takes advantage of the fact that the real logic
        // is done in other classes, Tape, ProgramStream, etc. Most of the instructions are one line
        // and need only one test. The bulk of the testing is then done on Tape and ProgramStream

        private static readonly ProgramStream _nullProgram  = null;
        private static readonly TextReader _nullInput = null;
        private static readonly TextWriter _nullOutput = null;

        // ReSharper disable InconsistentNaming
        [TestMethod]
        public void MoveForwardExecution_StartingAtPosition1_ShouldEndUpAtTwo()
        {
            // Arrange
            const int startingPosition = 1;
            var tape = Tape.LoadState(new[] { 1, 3, 5 }, startingPosition);

            // Act
            Instruction.MoveForward.Execute(_nullProgram, tape, _nullInput, _nullOutput);

            // Assert
            var actualEndingPosition = tape.GetState().Position;
            const int expectedEndingPosition = 2;
            Assert.AreEqual(expectedEndingPosition, actualEndingPosition);
        }

        [TestMethod]
        public void MoveBackwardExecution_StartingAtPosition1_ShouldEndUpAtZero()
        {
            // Arrange
            const int startingPosition = 1;
            var tape = Tape.LoadState(new[] { 1, 3, 5 }, startingPosition);

            // Act
            Instruction.MoveBackward.Execute(_nullProgram, tape, _nullInput, _nullOutput);

            // Assert
            var actualEndingPosition = tape.GetState().Position;
            const int expectedEndingPosition = 0;

            Assert.AreEqual(expectedEndingPosition, actualEndingPosition);
        }

        [TestMethod]
        public void IncrementExecution_ShouldIncrementCurrentCell()
        {
            // Arrange
            var tape = Tape.LoadState(new[] { 5, 7, 9 }, 1);

            // Act
            Instruction.Increment.Execute(_nullProgram, tape, _nullInput, _nullOutput);

            // Assert - Expect number at index 1 to have been incrmented
            CollectionAssert.AreEqual(new[] { 5, 8, 9 }, tape.GetState().Cells);
        }

        [TestMethod]
        public void DecrementExecution_AtIndex1_ShouldDecrementTheValueAtIndex1()
        {
            // Arrange
            var tape = Tape.LoadState(new[] { 5, 7, 9 }, 1);

            // Act
            Instruction.Decrement.Execute(_nullProgram, tape, _nullInput, _nullOutput);

            // Assert - Expect number at index 1 to have been incrmented
            CollectionAssert.AreEqual(new[] { 5, 6, 9 }, tape.GetState().Cells);
        }

        [TestMethod]
        public void InputExecution_ShouldReadFromInputAtWriteToTape()
        {
            // Arrange
            var tape = Tape.LoadState(new[] { 10, 11, 12 }, 1);
            var input = CreateReaderWithNextCharacterEqualTo((char)65);

            // Act
            Instruction.Input.Execute(_nullProgram, tape, input, _nullOutput);

            // Assert
            CollectionAssert.AreEqual(new[] { 10, 65, 12 }, tape.GetState().Cells);

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
            Instruction.Output.Execute(_nullProgram, tape, _nullInput, output);

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
            var cells = new[] { 0, 24, 0 };
            const int pos = 1;

            var tape = Tape.LoadState(cells, pos);

            // Act
            Instruction.TestAndJumpForward.Execute(_nullProgram, tape, _nullInput, _nullOutput);

            // Assert
            var state = tape.GetState();
            Assert.AreEqual(pos, state.Position);
            CollectionAssert.AreEqual(cells, state.Cells);
        }

        [TestMethod]
        public void TestAndJumpForwardExecution_WithTapeEqualToZero_ShouldJump()
        {
            // Arrange
            var tape = Tape.Default;
            var mock = new Mock<IProgramStream>(MockBehavior.Strict);
            mock.Setup(program => program.JumpForward());

            // Act
            Instruction.TestAndJumpForward.Execute(mock.Object, tape, _nullInput, _nullOutput);

            // Assert
            mock.VerifyAll();

        }

        [TestMethod]
        public void TestAndJumpBackwardExecution_WithTapeEqualToZero_ShouldDoNothing()
        {
            // Arrange
            var cells = new[] { 0, 0, 0 };
            const int pos = 1;

            var tape = Tape.LoadState(cells, pos);

            // Act
            Instruction.TestAndJumpBackward.Execute(_nullProgram, tape, _nullInput, _nullOutput);

            // Assert
            var state = tape.GetState();
            Assert.AreEqual(pos, state.Position);
            CollectionAssert.AreEqual(cells, state.Cells);
        }

        [TestMethod]
        public void TestAndJumpBackwardExecution_WithTapeNotEqualToZero_ShouldJump()
        {
            // Arrange
            var tape = Tape.LoadState(new[] { 1, 2, 3 }, 1);
            var mock = new Mock<IProgramStream>(MockBehavior.Strict);
            mock.Setup(program => program.JumpBackward());

            // Act
            Instruction.TestAndJumpBackward.Execute(mock.Object, tape, _nullInput, _nullOutput);

            // Assert
            mock.VerifyAll();

        }

        [TestMethod]
        public void NoOperation_WhenExecuted_ShouldDoNothing()
        {
            // Arrange

            // Act
            Instruction.NoOperation.Execute(_nullProgram, null, _nullInput, _nullOutput);

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


        // ReSharper restore InconsistentNaming
    }
}
