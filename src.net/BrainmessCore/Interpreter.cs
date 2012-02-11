using System;
using System.IO;

namespace Welch.Brainmess
{
	public class Interpreter
	{
		public Interpreter (ProgramStream program, Tape tape, TextReader input, TextWriter output)
		{
			_program = program;
			_tape = tape;
			_input = input;
			_output = output;
		}
		
		private ProgramStream _program;
		
		private Tape _tape;
		

		
		TextWriter _output;
		TextReader _input;


		
		// uses "program" data, "tape" data, has information about instructions. Seems
		// like an interaction between instructions, tape, and program.
		
		// Refactorings you can use. Extract method on each case body.
		
		// Unit testing this requires that you write "programs" to exercise all possibilities.
		// Which is probably doable but tricky. 
		public void Run()
		{
			while(!_program.EndOfProgram)
			{
				Instruction currentInstruction = _program.Fetch();
				currentInstruction.Execute(_program, _tape, _input, _output);
			}
			
		}
	}
}

