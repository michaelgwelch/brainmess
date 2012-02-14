using System;
using NUnit.Framework;

namespace Welch.Brainmess
{
    [TestFixture]
    public class BracketsTests
    {

        [Test]
        public void FindMatch_ForwardWithNoNesting()
        {
            // Arrange
            //                           1111111111222222
            //                 01234567890123456789012345
            string sequence = "abcd[   djskd dksj] dsksld";
            
            // Act
            var matchIndex = Brackets.FindMatch(sequence, 4);
            
            // Assert
            Assert.AreEqual(18, matchIndex);
        }
        
        [Test]
        public void FindMatch_ForwardWithOneNestedPair()
        {
            // Arrange
            //                           1111111111222222
            //                 01234567890123456789012345
            string sequence = "abcd[   [jskd ]ksj] dsksld";
            
            // Act
            var matchIndex = Brackets.FindMatch(sequence, 4);
            
            // Assert
            Assert.AreEqual(18, matchIndex); 
        }
        
        [Test]
        public void FindMatch_ForwardWithTwoPairsEachNestOneLevelDeep()
        {
            // Arrange
            //                           1111111111222222
            //                 01234567890123456789012345
            string sequence = "a[cd[  ][jskd lksj] dsks]d";
            
            // Act
            var matchIndex = Brackets.FindMatch(sequence, 1);
            
            // Assert
            Assert.AreEqual(24, matchIndex);             
        }
        
        [Test]
        public void FindMatch_ForwardWithATwoLevelNesting()
        {
            // Arrange
            //                           1111111111222222
            //                 01234567890123456789012345
            string sequence = "abcd[   [j[kd ]ks]h] dsksld";
            
            // Act
            var matchIndex = Brackets.FindMatch(sequence, 4);
            
            // Assert
            Assert.AreEqual(19, matchIndex); 
        }
        
        
                [Test]
        public void FindMatch_BackwardWithNoNesting()
        {
            // Arrange
            //                           1111111111222222
            //                 01234567890123456789012345
            string sequence = "abcd[   djskd dksj] dsksld";
            
            // Act
            var matchIndex = Brackets.FindMatch(sequence, 18);
            
            // Assert
            Assert.AreEqual(4, matchIndex);
        }
        
        [Test]
        public void FindMatch_BackwardWithOneNestedPair()
        {
            // Arrange
            //                           1111111111222222
            //                 01234567890123456789012345
            string sequence = "abcd[   [jskd ]ksj] dsksld";
            
            // Act
            var matchIndex = Brackets.FindMatch(sequence, 18);
            
            // Assert
            Assert.AreEqual(4, matchIndex); 
        }
        
        [Test]
        public void FindMatch_BackwardWithTwoPairsEachNestOneLevelDeep()
        {
            // Arrange
            //                           1111111111222222
            //                 01234567890123456789012345
            string sequence = "a[cd[  ][jskd lksj] dsks]d";
            
            // Act
            var matchIndex = Brackets.FindMatch(sequence, 24);
            
            // Assert
            Assert.AreEqual(1, matchIndex);             
        }
        
        [Test]
        public void FindMatch_BackwardWithATwoLevelNesting()
        {
            // Arrange
            //                           1111111111222222
            //                 01234567890123456789012345
            string sequence = "abcd[   [j[kd ]ks]h] dsksld";
            
            // Act
            var matchIndex = Brackets.FindMatch(sequence, 19);
            
            // Assert
            Assert.AreEqual(4, matchIndex); 
        }
    }
}

