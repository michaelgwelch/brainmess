using System.IO;
using System.Collections.Generic;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Moq;
using Moq.Language.Flow;

namespace Welch.Brainmess
{
    [TestClass]
    public class InterpreterTests
    {
        // ReSharper disable InconsistentNaming
        [TestMethod]
        public void Run_WithEmptyProgram_ShouldCallEndOfProgramAndExit()
        {
            // Arrange

            // Use strict behavior to make sure the interpreter stops after only one call.
            var mock = new Mock<IProgramStream>(MockBehavior.Strict);
            mock.Setup(prog => prog.EndOfProgram).Returns(true);
            var program = mock.Object;
            var interpreter = new Interpreter(program, null, null, null);

            // Act
            interpreter.Run();

            // Assert
            mock.VerifyAll();

        }

        [TestMethod]
        public void Run_WithOneInstruction()
        {
            // Arrange
            var mock = new Mock<IProgramStream>(MockBehavior.Strict);
            mock.Setup(prog => prog.EndOfProgram).ReturnsInOrder(false, true);
            mock.Setup(prog => prog.Fetch()).Returns(Instruction.Increment);
            var tape = Tape.Default();
            var program = mock.Object;
            var interpreter = new Interpreter(program, tape, null, null);

            // Act
            interpreter.Run();

            // Assert
            var state = tape.GetState();
            Assert.AreEqual(1, state.Cells[state.Position]);


        }

        [TestMethod]
        public void Run_HelloWorld()
        {
            // Arrange
            const string program = @"
++++++++[<+++++++++>-]<.>+++++[<++++++>-]<-.
+++++++..+++.>++++++++[<<++++>>-]<<.
>>++++[<------>-]<.>++++[<++++++>-]<.
+++.------.--------.<+.
";
            const string expectedString = "Hello World!";
            var programStream = new ProgramStream(program);
            var tape = Tape.Default();
            var outputStream = new MemoryStream();
            var output = new StreamWriter(outputStream) {AutoFlush = true};
            var interpeter = new Interpreter(programStream, tape, null, output);

            // Act
            interpeter.Run();
            
            // Assert
            outputStream.Position = 0;
            var reader = new StreamReader(outputStream);
            var actualString = reader.ReadToEnd();
            Assert.AreEqual(expectedString, actualString);
        }

        // ReSharper restore InconsistentNaming

    }

    public static class MoqExtensions
    {
        public static void ReturnsInOrder<T, TResult>(this ISetup<T, TResult> setup,
          params TResult[] results) where T : class
        {
            setup.Returns(new Queue<TResult>(results).Dequeue);
        }
    }
}
