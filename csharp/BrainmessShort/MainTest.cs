using System;
using System.IO;
using System.Text;
using NUnit.Framework;

namespace BrainmessShort
{
    [TestFixture]
    public class MainTest
    {
        [Test]
        public void RunHelloWorld()
        {
            var builder = RedirectOutput();
            Brainmess.Main(new[] {"../../../../scripts/hello.bm"});
            Assert.AreEqual("Hello World!", builder.ToString());
        }

        [Test]
        public void RunDoubleWith2Expect4()
        {
            var builder = RedirectOutput();
            SetInput("2");
            Brainmess.Main(new[] { "../../../../scripts/double.bm" });
            Assert.AreEqual("4", builder.ToString());
        }

        [Test]
        public void RunDoubleWith0Expect0()
        {
            var builder = RedirectOutput();
            SetInput("0");
            Brainmess.Main(new[] { "../../../../scripts/double.bm" });
            Assert.AreEqual("0", builder.ToString());
        }

        [Test]
        public void RunFibonacci()
        {
            var builder = RedirectOutput();
            Brainmess.Main(new[] {"../../../../scripts/fibonacci.bm"});
            Assert.AreEqual("1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89", builder.ToString());
        }

        /// <summary>
        /// Redirects standard output to a StringBuilder and returns
        /// that StringBuilder.
        /// </summary>
        /// <returns></returns>
        private static StringBuilder RedirectOutput()
        {
            StringBuilder builder = new StringBuilder();
            TextWriter writer = new StringWriter(builder);
            Console.SetOut(writer);
            return builder;
        }

        /// <summary>
        /// Creates a text reader that is initialized with 
        /// <paramref name="input"/>. Then sets standard input to read
        /// from that reader.
        /// </summary>
        /// <param name="input"></param>
        private static void SetInput(string input)
        {
            TextReader reader = new StringReader(input);
            Console.SetIn(reader);
        }
    }
}
