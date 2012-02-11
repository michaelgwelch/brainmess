using System;
using System.IO;

namespace BrainMessSimple
{
	public class Instruction
	{
		private readonly Action<ProgramStream, Tape, TextReader, TextWriter> _action;
		private Instruction (Action<ProgramStream, Tape, TextReader, TextWriter> action)
		{
			_action = action;
		}
		
		public void Execute(ProgramStream program, Tape tape, TextReader input, TextWriter output)
		{
			_action(program, tape, input, output);
		}
		
		public static readonly Instruction MoveForward = 
			new Instruction((program, tape, input, output) => tape.MoveForward());
		
		public static readonly Instruction MoveBackward = 
			new Instruction((program, tape, input, output) => tape.MoveBackward());
		
		public static readonly Instruction Increment =
			new Instruction((program, tape, input, output) => tape.Increment());
		
		public static readonly Instruction Decrement =
			new Instruction((program, tape, input, output) => tape.Decrement());
		
		public static readonly Instruction Input =
			new Instruction((program, tape, input, output) => tape.Current = input.Read());
		
		public static readonly Instruction Output =
			new Instruction((program, tape, input, output) => output.Write((char)tape.Current));
		
		public static readonly Instruction TestAndJumpFoward =
			new Instruction((program, tape, input, output) => {if (tape.Current == 0) program.JumpForward();});

		public static readonly Instruction TestAndJumpBackward =
			new Instruction((program, tape, input, output) => {if (tape.Current != 0) program.JumpBackward();});
		
		public static readonly Instruction NoOperation = 
			new Instruction((program, tape, input, output) => {});
		
		public static Instruction FromInt(int characterValue)
		{
			switch(characterValue)
			{
			case '>':
				return MoveForward;
			case '<':
				return MoveBackward;
			case '+':
				return Increment;
			case '-':
				return Decrement;
			case '.':
				return Output;
			case ',':
				return Input;
			case '[':
				return TestAndJumpFoward;
			case ']':
				return TestAndJumpBackward;
			default:
				return NoOperation;
			}
		}
		                                                          
	}
	
	
}

