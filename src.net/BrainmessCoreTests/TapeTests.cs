using System;
using System.Collections.Generic;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Test = Microsoft.VisualStudio.TestTools.UnitTesting.TestMethodAttribute;

namespace Welch.Brainmess
{
    [TestClass]
    public class TapeTests
    {
        // ReSharper disable InconsistentNaming

        // Some bad smells. Several of my tests require that my Act phase uses two different methods
        // The one I'm testing and another one to cause a side effect.

        // Another thing that permeates all of this is that I construct tapes in Assembly phases
        // But I don't really have any tests of the static constructors.


        [Test]
        public void Current_ConstructDefault_ExpectCurrentIsZero()
        {
            // The "simplest" test. We create an instance and expect that Current is 0
            Assert.AreEqual(0, Tape.Default.Current);
        }

        [Test]
        public void Current_ConstructPrePopulatedTape_ExpectCurrentToMatch()
        {
            // Arrange
            const int expectedValue = 55;
            var list = new LinkedList<int>();
            list.AddFirst(expectedValue);

            // Act
            var tape = Tape.LoadState(list, 0);

            // Assert - The first value in the list should equal the value of the current cell on the tape.
            Assert.AreEqual(expectedValue, tape.Current);
        }

        [Test]
        public void SetCurrent_StateShouldChange()
        {
            // Arrange
            var expectedTape = Tape.LoadState(new[] { 1, 23, 1 }, 1);
            var actualTape = Tape.LoadState(new[] { 1, 2, 1 }, 1);

            // Act
            actualTape.Current = 23;

            // Assert
            Assert.AreEqual(expectedTape, actualTape);
        }

        [Test]
        public void Increment_ConstructFromValues_ExpectIncrementToIncrementTheCell()
        {
            // Arrange
            var expectedTape = Tape.LoadState(new[] { 23 }, 0);
            var actualTape = Tape.LoadState(new[] { 22 }, 0);

            // Act
            actualTape.Increment();

            // Assert
            Assert.AreEqual(expectedTape, actualTape);

        }

        [Test]
        public void Decrement_ConstructFromValues_ExpectDecrementToDecrementTheCell()
        {
            // Arrange
            var expectedTape = Tape.LoadState(new[] { 44 }, 0);
            var actualTape = Tape.LoadState(new[] {45}, 0);

            // Act
            actualTape.Decrement();

            // Assert
            Assert.AreEqual(expectedTape, actualTape);
        }

        [Test]
        public void MoveForward_ConstructDefaultAndMoveFoward_ExpectPositionToEqualOne()
        {
            // Arrange
            var expectedTape = Tape.LoadState(new[] { 0, 0 }, 1);
            var actualTape = Tape.Default;

            // Act
            actualTape.MoveForward(); // Method being tested.

            // Assert
            Assert.AreEqual(expectedTape, actualTape);

        }

        [Test]
        public void MoveForward_ConstructFromMultiCellListMoveForwardAndMutate_ExpectSecondCellMatchesValue()
        {
            // Arrange
            var cells = new [] { 1, 3, 5, 7, 11, 13, 17, 19 };
            var expectedTape = Tape.LoadState(cells, 5);
            var actualTape = Tape.LoadState(cells, 4);

            // Act
            actualTape.MoveForward();

            // Assert
            Assert.AreEqual(expectedTape, actualTape);

        }

        [Test]
        public void MoveBackward_ConstructFromMutiCellListMoveBackwardAndMutate_ExpectSecondFromLastCellMatchesValue()
        {
            // Arrange - construct a tape from a list and set current cell to last value in list.
            var values = new [] { 19, 17, 13, 11, 7, 5, 3, 1 };
            var expectedTape = Tape.LoadState(values, 6);
            var actualTape = Tape.LoadState(values, 7);

            // Act
            actualTape.MoveBackward();

            // Assert
            Assert.AreEqual(expectedTape, actualTape);
        }

        [Test]
        public void MoveBackward_ConstructFromSingleCellListMoveBackwardAndMutate_ExpectFirstCellToMatch()
        {
            // Arrange
            var expectedTape = Tape.LoadState(new[] { 0, 0 }, 0);
            var actualTape = Tape.Default;

            // Act
            actualTape.MoveBackward();

            // Assert
            Assert.AreEqual(expectedTape, actualTape);

        }

        [Test]
        public void Wrap_PassNull_ExpectArgumentNullException()
        {
            try
            {
                Tape.LoadState(null, 0);
                Assert.Fail("Expect ArgumentNullException");
            }
            catch (ArgumentNullException)
            { }
        }

        [Test]
        public void Wrap_PassBadPosition_ExpectArgumentException()
        {
            try
            {
                Tape.LoadState(new LinkedList<int>(), 55);
                Assert.Fail("Expected an ArgumentException");
            }
            catch (ArgumentException)
            { }
        }

        [Test]
        public void LoadState_WithPositionLessThanZero_ShouldThrowException()
        {
            try
            {
                // Act
                Tape.LoadState(new[] { 1, 3, 5 }, -1);
                Assert.Fail("Expected ArgumentOutOfRangeException");

            }
            catch (ArgumentOutOfRangeException)
            {}
        }

        [Test]
        public void LoadState_WithPositionSetToLengthOfArray_ShouldThrowException()
        {
            try
            {
                // Act
                Tape.LoadState(new[] { 0 }, 1);

                Assert.Fail("Expected ArgumentOutOfRangeException");
            }
            catch (ArgumentOutOfRangeException)
            {
            }
        }

        // ReSharper restore InconsistentNaming


        // bugs: if increment or decrement wrap. But that's a limitation of our design. Not of any of our methods. 
    }

}
