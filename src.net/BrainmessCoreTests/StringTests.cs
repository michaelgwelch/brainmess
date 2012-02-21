using System;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace Welch.Brainmess
{
    /// <summary>
    /// Tests Welch.Brainmess.StringExtensions extension methods.
    /// </summary>
    [TestClass]
    public class StringTests
    {
        // ReSharper disable InconsistentNaming
        [TestMethod]
        public void FindMatch_StartingAtNonJumpCharacter_ExpectArgumentException()
        {
            // Arrange
            // Arrange
            //                           1111111111222222
            //                 01234567890123456789012345
            const string sequence = "abcd[   djskd dksj] dsksld";

            try
            {
                // Act
                sequence.FindMatch(3);

                // Assert
                Assert.Fail("Expected ArgumentException");
            }
            catch(ArgumentException)
            {
                
            }
        }

        [TestMethod]
        public void FindMatch_ForwardWithNoNesting()
        {
            // Arrange
            //                                 1111111111222222
            //                       01234567890123456789012345
            const string sequence = "abcd[   djskd dksj] dsksld";

            // Act
            var matchIndex = sequence.FindMatch(4);

            // Assert
            Assert.AreEqual(18, matchIndex);
        }

        [TestMethod]
        public void FindMatch_ForwardWithOneNestedPair()
        {
            // Arrange
            //                           1111111111222222
            //                 01234567890123456789012345
            const string sequence = "abcd[   [jskd ]ksj] dsksld";

            // Act
            var matchIndex = sequence.FindMatch(4);

            // Assert
            Assert.AreEqual(18, matchIndex);
        }

        [TestMethod]
        public void FindMatch_ForwardWithTwoPairsEachNestOneLevelDeep()
        {
            // Arrange
            //                           1111111111222222
            //                 01234567890123456789012345
            const string sequence = "a[cd[  ][jskd lksj] dsks]d";

            // Act
            var matchIndex = sequence.FindMatch(1);

            // Assert
            Assert.AreEqual(24, matchIndex);
        }

        [TestMethod]
        public void FindMatch_ForwardWithATwoLevelNesting()
        {
            // Arrange
            //                           1111111111222222
            //                 01234567890123456789012345
            const string sequence = "abcd[   [j[kd ]ks]h] dsksld";

            // Act
            var matchIndex = sequence.FindMatch(4);

            // Assert
            Assert.AreEqual(19, matchIndex);
        }


        [TestMethod]
        public void FindMatch_BackwardWithNoNesting()
        {
            // Arrange
            //                           1111111111222222
            //                 01234567890123456789012345
            const string sequence = "abcd[   djskd dksj] dsksld";

            // Act
            var matchIndex = sequence.FindMatch(18);

            // Assert
            Assert.AreEqual(4, matchIndex);
        }

        [TestMethod]
        public void FindMatch_BackwardWithOneNestedPair()
        {
            // Arrange
            //                           1111111111222222
            //                 01234567890123456789012345
            const string sequence = "abcd[   [jskd ]ksj] dsksld";

            // Act
            var matchIndex = sequence.FindMatch(18);

            // Assert
            Assert.AreEqual(4, matchIndex);
        }

        [TestMethod]
        public void FindMatch_BackwardWithTwoPairsEachNestOneLevelDeep()
        {
            // Arrange
            //                           1111111111222222
            //                 01234567890123456789012345
            const string sequence = "a[cd[  ][jskd lksj] dsks]d";

            // Act
            var matchIndex = sequence.FindMatch(24);

            // Assert
            Assert.AreEqual(1, matchIndex);
        }

        [TestMethod]
        public void FindMatch_BackwardWithATwoLevelNesting()
        {
            // Arrange
            //                           1111111111222222
            //                 01234567890123456789012345
            const string sequence = "abcd[   [j[kd ]ks]h] dsksld";

            // Act
            var matchIndex = sequence.FindMatch(19);

            // Assert
            Assert.AreEqual(4, matchIndex);
        }
        // ReSharper restore InconsistentNaming

    }
}
