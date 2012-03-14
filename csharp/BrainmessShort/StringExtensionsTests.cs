using NUnit.Framework;

namespace BrainmessShort
{
    [TestFixture]
    public class StringExtensionsTests
    {
        [Test]
        public void FindMatch_WithSmallestValidStringStartingWithLeftBracket()
        {
            // Arrange
            const string program = "[]";

            // Act
            var actual = program.FindMatch(0);

            // Assert
            const int expected = 1;
            Assert.AreEqual(expected, actual);
        }

        [Test]
        public void FindMatch_WithSmallestValidStringStartingWithRightBracket()
        {
            // Arrange
            const string program = "[]";

            // Act
            var actual = program.FindMatch(1);

            // Assert
            const int expected = 0;
            Assert.AreEqual(expected, actual);
        }

        [Test]
        public void FindMatch_WithSinglePairOfBracketsInAStringStartingWithLeftBracket()
        {
            // Arrange
            //                      012345678901234 
            const string program = "++++[>>>>>>]...";

            // Act
            var actual = program.FindMatch(4);

            // Assert
            const int expected = 11;
            Assert.AreEqual(expected, actual);
        }
    }
}
