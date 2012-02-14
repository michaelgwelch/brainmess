using System;
using System.Collections.Generic;
using NUnit.Framework;

namespace Welch.Brainmess
{
    [TestFixture]
    public class LinkedListNodeTests
    {
        [Test]
        public void MoveForward_NextIsNotNull_ExpectTheNextIsReturned()
        {
            // Arrange
            LinkedList<int> list = new LinkedList<int>(new int[] {1,3,5,7});           
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
            LinkedList<int> list = new LinkedList<int>(new int[] {1,3,4,7});
            var before = list.Last;
            LinkedListNode<int> after = null;
            
            // Act
            var cell = before.MoveForward();
            var expectedCellValue = 0; // default value of a new cell
            
            // Assert
            AssertMatchingCell(before, cell, expectedCellValue, after);
        }
        
        private static void AssertMatchingCell<T>(LinkedListNode<T> before, LinkedListNode<T> cell, T expectedCellValue, LinkedListNode<T> after)
        {
            Assert.AreSame(before, cell.Previous);
            Assert.AreSame(after, cell.Next);
            Assert.AreEqual(expectedCellValue, cell.Value);
        }
    }
}

