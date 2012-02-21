using System;
using System.IO;
using System.Collections.Generic;
using System.Text;
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
            var mock = new Mock<IProgram>(MockBehavior.Strict);
            mock.Setup(prog => prog.EndOfProgram).Returns(true);
            var program = mock.Object;
            var interpreter = new Interpreter(program);

            // Act
            interpreter.Run();

            // Assert
            mock.VerifyAll();

        }

        [TestMethod]
        public void Run_WithOneInstruction()
        {
            // Arrange
            var mock = new Mock<IProgram>(MockBehavior.Strict);
            mock.Setup(prog => prog.EndOfProgram).ReturnsInOrder(false, true);
            mock.Setup(prog => prog.Fetch()).Returns(Instruction.Increment);
            var expectedTape = Tape.LoadState(new[] { 1 }, 0);
            var actualTape = Tape.Default;
            var program = mock.Object;
            var interpreter = new Interpreter(program, actualTape);

            // Act
            interpreter.Run();

            // Assert
            Assert.AreEqual(expectedTape, actualTape);


        }

        // Ignored because it is an integration test and screws up actual coverage numbers.
        [Ignore]
        [TestCategory("Integration")]
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
            var programStream = new Program(program);
            var tape = Tape.Default;
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

        [TestMethod]
        public void Run_WithEmptyProgram_ShouldNotChangeStateOfAnything()
        {
            // Arrange - Create program, tape, input, and output. We'll verify at the end 
            //           that none of them were modified as a result of running the null program.
            var tape = Tape.Default;
            var program = new Program("");
            var input = new DummyInput(); // fails on Read
            var output = new DummyOutput(); // fails on Write
            var interpreter = new Interpreter(program, tape, input, output);

            // Act
            interpreter.Run();

            // Assert
            Assert.AreEqual(0, program.ProgramCounter);
            Assert.AreEqual(tape, Tape.Default);
        }

        [TestMethod]
        public void Run_WithDefaultInterpreter_DoesNotBlowUp()
        {
            new Interpreter().Run();
        }
        // ReSharper restore InconsistentNaming

    }

    public class DummyInput : TextReader
    {
        public override int Read()
        {
            Assert.Fail("Did not expect a Read");
// ReSharper disable HeuristicUnreachableCode
            return 0;
// ReSharper restore HeuristicUnreachableCode
        }
    }

    public class DummyOutput : TextWriter
    {
        public override void Write(char value)
        {
            Assert.Fail("Did not expect a Write.");
        }

        public override Encoding Encoding
        {
            get { throw new NotImplementedException(); }
        }
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
