using System;
using System.Diagnostics;

namespace Welch.Brainmess
{
	/// <summary>
	/// A stream of Brainmess instructions. It maintains a program counter which points
	/// at the next instruction to execute. (Ed., everything is marked internal for a few reasons:
	/// 1) to indicate that really this isn't a general purpose class. It is used by the Interpreter
	/// and Instruction classes. 2) To show how it can still be tested as the clients of this class
	/// are tested 3) JumpForward and JumpBackward require they are called only in certain conditions
	/// which the Interpreter guarantees. 
	/// </summary>
	public class ProgramStream
	{
		// Mutable State
		int _programCounter = 0;
		
		// Imutable Data
		private readonly string _program;
		
		/// <summary>
		/// Initializes a new instance of the <see cref="Welch.Brainmess.ProgramStream"/> class
		/// with the characters from the specified program. The program counter is set to the first
		/// character in the program.
		/// </summary>
		public ProgramStream (string program)
		{
			_program = program;
		}

		/// <summary>
		/// Reads the Instruction at the program counter and returns it.
		/// </summary>
		internal Instruction Fetch()
		{
			var instruction = _program [_programCounter];
			_programCounter++;
			return Instruction.FromInt(instruction);
		}

		/// <summary>
		/// Gets a value indicating whether this instance is at the end of the program.
		/// If Fetch is called when EndOfProgram is true an exception will be thrown.
		/// </summary>
		/// <value>
		/// <c>true</c> if end of program; otherwise, <c>false</c>.
		/// </value>
		internal bool EndOfProgram { get { return _programCounter >= _program.Length; } }

		
		/// <summary>
		/// This method causes the program counter to move from current location to right after
		/// a matching ']' instruction. It only makes sense to be called if the interpreter is 
		/// executing a TestAndJumpForward instruction. If that is not the case, the results
		/// are not predictable.
		/// </summary>
		internal void JumpForward()
		{
            Debug.Assert(_program[_programCounter - 1] == '[');
            _programCounter = Brackets.FindMatch(_program, _programCounter - 1) + 1;
		}
		
		/// <summary>
		/// This method causes the program counter to move from current location to 
		/// a matching '[' instruction. It only makes sense to be called if the interpreter is 
		/// executing a TestAndJumpBackward instruction. If that is not the case, the results
		/// are not predictable.
		/// </summary>
		internal void JumpBackward()
		{
			Debug.Assert(_program [_programCounter - 1] == ']');
            _programCounter = Brackets.FindMatch(_program, _programCounter - 1);
 		}

	}
}

