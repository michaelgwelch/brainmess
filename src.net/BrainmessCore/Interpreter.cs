using System;
using System.IO;

namespace Welch.Brainmess
{
	public class Interpreter : IExecutionContext
	{
		// this class had all the references to all the dependencies so it makes sense to make it implement
		// IExecutionContext. That decouples all of the Instrucions for the fact that there are 4 types of objects.
		
		public Interpreter (ProgramStream program, Tape tape, TextReader input, TextWriter output)
		{
			_program = program;
			_tape = tape;
			_input = input;
			_output = output;
		}

		private readonly ProgramStream _program;
		private readonly Tape _tape;
		private readonly TextWriter _output;
		private readonly TextReader _input;



		public void Run()
		{
			while (!_program.EndOfProgram)
			{
				Instruction currentInstruction = _program.Fetch();
				currentInstruction.Execute(this);
			}

		}
		
		#region Implement IExecutionContext explicitly
		void IExecutionContext.MoveTapeForward()
		{
			_tape.MoveForward();
		}
		
		void IExecutionContext.MoveTapeBackward()
		{
			_tape.MoveBackward();
		}
		
		int IExecutionContext.CurrentTapeValue
		{
			get
			{
				return _tape.Current;
			}
			set
			{
				_tape.Current = value;
			}
		}
		
		void IExecutionContext.IncrementTape()
		{
			_tape.Increment();
		}
		
		void IExecutionContext.DecrementTape()
		{
			_tape.Decrement();
		}
		
		void IExecutionContext.WriteToOutput(char characterValue)
		{
			_output.Write(characterValue);
		}
		
		int IExecutionContext.ReadFromInput()
		{
			return _input.Read();
		}
		
		void IExecutionContext.JumpProgramForward()
		{
			_program.JumpForward();
		}
		
		void IExecutionContext.JumpProgramBackward()
		{
			_program.JumpBackward();
		}
		#endregion
	}
}

