using System;

namespace Welch.Brainmess
{
	// mainly created to make test assembly simpler - I could put factory methods on Interpreter and use those in tests.
	// rather than build up 4 different types and then pass them to Execute methods
	public interface IExecutionContext
	{
		void MoveTapeForward();

		void MoveTapeBackward();
		
		int CurrentTapeValue { get; set; }
		
		void IncrementTape();

		void DecrementTape();
		
		int ReadFromInput();

		void WriteToOutput(char character);
		
		void JumpProgramForward();

		void JumpProgramBackward();
	}
}

