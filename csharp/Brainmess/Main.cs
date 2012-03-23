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
                var file = File.OpenRead(commandLine.Path).CreateReader();
                var interpreter = new Interpreter(new Program(file.ReadToEnd()));
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
}
