using System;
using System.IO;

namespace Welch.Brainmess
{

    class MainClass
    {
        public static void Main(string[] args)
        {
            CommandLine commandLine = CommandLine.Read(args);
            if (!commandLine.Valid)
            {
                Console.WriteLine("Usage: brainmess script.bm");
            }

            if (File.Exists(commandLine.Path))
            {
                var fileContents = File.ReadAllText(commandLine.Path);
                var interpreter = new Interpreter(new Program(fileContents));
                interpreter.Run();
            }
        }


        public class CommandLine    
        {
            public string Path { get; private set; }
            public bool Valid { get; private set; }
            public static CommandLine Read(string[] args)
            {
                var valid = args.Length == 1;
                return valid
                           ? new CommandLine {Valid = true, Path = args[0]}
                           : new CommandLine();
            }
        }



    }

    public static class StreamExtensions
    {
        public static TextReader CreateReader(this Stream stream)
        {
            return new StreamReader(stream);
        }
    }
}
