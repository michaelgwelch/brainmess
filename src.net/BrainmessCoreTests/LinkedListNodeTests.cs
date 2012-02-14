using System;
using System.Collections.Generic;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Test = Microsoft.VisualStudio.TestTools.UnitTesting.TestMethodAttribute;

namespace Welch.Brainmess
{
    // ReSharper disable InconsistentNaming
    [TestClass]
    public class LinkedListNodeTests
    {
        [Test]
        public void MoveForward_NodeIsNotLinkedToAList_ExpectArgumentException()

        {
            // Arrange
            var node = new LinkedListNode<int>(35);

            try
            {
                // Act
                node = node.MoveForward();

                // Assert
                Assert.Fail("Expected ArgumentException");
            }
            catch (ArgumentException)
            {
            }

        }

        [Test]
        public void MoveForward_NextIsNotNull_ExpectTheNextIsReturned()
        {
            // Arrange
            LinkedList<int> list = new LinkedList<int>(new int[] { 1, 3, 5, 7 });
            var before = list.Find(3);
            var after = list.Find(7);

            // Act
            var cell = before.MoveForward();
            var expectedCellValue = 5;

            // Assert
            AssertMatchingCell(before, cell, expectedCellValue, after);

        }

        [Test]
        public void MoveForward_NextIsNull_ExpectNewDefaultNode()
        {
            // Arrange
            LinkedList<int> list = new LinkedList<int>(new int[] { 1, 3, 4, 7 });
            var before = list.Last;
            LinkedListNode<int> after = null;

            // Act
            var cell = before.MoveForward();
            var expectedCellValue = 0; // default value of a new cell

            // Assert
            AssertMatchingCell(before, cell, expectedCellValue, after);
        }

        [Test]
        public void MoveBackward_NodeIsNotLinkedToAList_ExpectArgumentException()
        {
            // Arrange
            var node = new LinkedListNode<int>(35);

            try
            {
                // Act
                node = node.MoveBackward();

                // Assert
                Assert.Fail("Expected ArgumentException");
            }
            catch (ArgumentException)
            {
            }

        }

        [Test]
        public void MoveBackward_PreviousIsNotNull_ExpectThePreviousIsReturned()
        {
            // Arrange
            LinkedList<int> list = new LinkedList<int>(new [] { 1, 3, 5, 7 });
            var before = list.Find(3);
            var after = list.Find(7);

            // Act
            var cell = after.MoveBackward();
            var expectedCellValue = 5;

            // Assert
            AssertMatchingCell(before, cell, expectedCellValue, after);

        }

        [Test]
        public void MoveBackward_PreviousIsNull_ExpectNewDefaultNode()
        {
            // Arrange
            LinkedList<int> list = new LinkedList<int>(new [] { 1, 3, 4, 7 });
            var after = list.First;
            LinkedListNode<int> before = null;

            // Act
            var cell = after.MoveBackward();
            var expectedCellValue = 0; // default value of a new cell

            // Assert
            AssertMatchingCell(before, cell, expectedCellValue, after);
        }

        /// <summary>
        /// Checks to make sure that <paramref name="before"/> is the node before <paramref name="cell"/>,
        /// and <paramref name="after"/> is the node after <paramref name="cell"/>, and that <paramref name="cell"/>
        /// has the value of <paramref name="expectedCellValue"/>.
        /// </summary>
        private static void AssertMatchingCell<T>(LinkedListNode<T> before, LinkedListNode<T> cell, T expectedCellValue, LinkedListNode<T> after)
        {
            Assert.AreSame(before, cell.Previous);
            Assert.AreSame(after, cell.Next);
            Assert.AreEqual(expectedCellValue, cell.Value);
        }
    }
    // ReSharper restore InconsistentNaming
}
