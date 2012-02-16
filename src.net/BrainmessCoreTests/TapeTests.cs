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
            Assert.AreEqual(0, Tape.Default().Current);
        }

        [Test]
        public void Current_ConstructPrePopulatedTape_ExpectCurrentToMatch()
        {
            // Assemble
            const int expectedValue = 55;
            var list = new LinkedList<int>();
            list.AddFirst(expectedValue);

            // Act
            var tape = Tape.LoadState(list);

            // Assert - The first value in the list should equal the value of the current cell on the tape.
            Assert.AreEqual(expectedValue, tape.Current);
        }

        [Test]
        public void SetCurrent_StateShouldChange()
        {
            // Arrange
            var tape = Tape.LoadState(new[] { 1, 2, 1 }, 1);

            // Act
            tape.Current = 23;

            // Assert
            CollectionAssert.AreEqual(new[] { 1, 23, 1 }, tape.GetState().Cells);
        }

        [Test]
        public void Increment_ConstructFromValues_ExpectIncrementToIncrementTheCell()
        {
            // Assemble
            var list = new LinkedList<int>(new [] {22});
            var tape = Tape.LoadState(list);

            // Act
            tape.Increment();

            // Assert
            Tape.State state = tape.GetState();
            Assert.AreEqual(23, state.Cells[state.Position]);

        }

        [Test]
        public void Decrement_ConstructFromValues_ExpectDecrementToDecrementTheCell()
        {
            // Assemble
            var list = new LinkedList<int>(new[] {45});
            var tape = Tape.LoadState(list);

            // Act
            tape.Decrement();

            // Assert
            Tape.State state = tape.GetState();
            Assert.AreEqual(44, state.Cells[state.Position]);
        }

        [Test]
        public void MoveForward_ConstructDefaultAndMoveFoward_ExpectPositionToEqualOne()
        {
            // Assemble
            var tape = Tape.Default();

            // Act
            tape.MoveForward(); // Method being tested.

            // Assert
            Tape.State state = tape.GetState();
            Assert.AreEqual(1, state.Position);


        }

        [Test]
        public void MoveForward_ConstructFromMultiCellListMoveForwardAndMutate_ExpectSecondCellMatchesValue()
        {
            // Assemble
            var initialList = new [] { 1, 3, 5, 7, 11, 13, 17, 19 };
            const int position = 4;
            var tape = Tape.LoadState(initialList, position);

            // Act
            tape.MoveForward();

            // Assert
            var state = tape.GetState();
            Assert.AreEqual(5, state.Position);

        }

        [Test]
        public void MoveBackward_ConstructFromMutiCellListMoveBackwardAndMutate_ExpectSecondFromLastCellMatchesValue()
        {
            // Assemble - construct a tape from a list and set current cell to last value in list.
            var initialList = new [] { 19, 17, 13, 11, 7, 5, 3, 1 };
            const int startPosition = 7;
            var tape = Tape.LoadState(initialList, startPosition);

            // Act
            tape.MoveBackward();

            // Assert
            var state = tape.GetState();
            const int endingPosition = 6;
            Assert.AreEqual(endingPosition, state.Position);
        }

        [Test]
        public void MoveBackward_ConstructFromSingleCellListMoveBackwardAndMutate_ExpectFirstCellToMatch()
        {
            // Assemble
            var tape = Tape.Default();

            // Act
            tape.MoveBackward();

            // Assert
            var state = tape.GetState();
            Assert.AreEqual(0, state.Position);

        }

        [Test]
        public void Wrap_PassNull_ExpectArgumentNullException()
        {
            try
            {
                Tape.LoadState(null);
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
