using System;

namespace BrainMessSimple
{
	/// <summary>
	/// A stream of Brainmess instructions.
	/// </summary>
	public class ProgramStream
	{
		// This sorta feels like a class. Two pieces of state that are very closely
		// related. Look out for stuff like this. Notice that they are always used 
		// together.
		int _programCounter = 0;
		private readonly string _program;
		
		public ProgramStream (string program)
		{
			_program = program;
		}
		
		// Should be part of program class.
		public char Fetch()
		{
			var instruction = _program[_programCounter];
			_programCounter++;
			return instruction;
		}
		
		// should be part of program class.
		public bool EndOfProgram { get { return _programCounter >= _program.Length; } }
		
		// Should be part of "program" class. It only uses that data.
		public void JumpForward()
		{
			// Precondition: Program Counter is pointing to the instruction immediately following a '[' instruction
			System.Diagnostics.Debug.Assert(_program[_programCounter-1] == '[');
			int nestLevel = 1;
			
			// Invariant: The nestLevel tells us how
			// deeply nested the program counter is. A nest level of 0 means we are outside of the current loop.
			// A nest level of 1 means we are inside the current loop. Etc. By current loop we mean the one started
			// by the instruction that caused this method to be called. Namely the one we check for in Debug.Assert 
			// above. In addition, if an iteration causes the nest level to change, we know that the instruction
			// that caused the change is directly to the left of the program counter.
			
			// Initialization: The Loop Invariant holds at prior to loop after initialization. The Debug.Assert
			//                 checks to make sure that we are indeed inside the current loop and we initialize 
			//                 nestLevel to 1 to indicate that. (Trivially the nestLevel changed and the program
			//                 counter is to the right of the instruction that caused the change).
			
			// Maintenance (inductive step): Check that the loop invariant holds at the end of each iteration of loop.
			//     We have 4 cases to test: 1) the program counter is now pointing at '[', 2) the program counter
			//     is now pointing at ']', or 3) the program counter is pointing at something other than '[' or ']',
			//     and finally 4) the program counter is passed the last instruction.
			//     Assume nestLevel is currently n.
			//     Case 1: We read a ']' instruction and therefore decrease the nestLevel by 1 and increment the 
			//             program counter. Since we just moved outside of a loop we expect the nestLevel to be n-1
			//             and this is indeed the case. In addition, the nest level just changed and the instruction
			//             that caused it is just to the left of program counter.
			//     Case 2: We read a '[' instruction and move the program counter forward 1. We have just entered
			//             a new loop and expect that the nestLevel is now (n+1). And this is indeed the case because
			//             the conditional increments nestLevel if we see a '['. In addition we know the program
			//             counter is direclty to the right of an instruction that caused the nest level to change.
			//     Case 3: We don't see an instruction related to a jump and therefore our nest level has not
			//             changed and therefore the nestLevel is still n. Nest level didn't change and therefore
			//             we can't say that the program counter is directly to the right of a jump instruction.
			//     Case 4: Program Terminates due to programCounter larger than size of program.
			//             The loop invariant still holds.
			
			// Termination: At beginning program counter is q. At each iteration the program counter is incremented
			//     by 1. Therefore there are 2 cases: 1) The end of the program is reached without finding a match
			//     and 2) a match is found. In the first case, we terminate with an exception. In the second case,
			//     we find a match and the nestLevel goes to 0 and we exit the loop. And by the loop invariant
			//     we know that since the nest level just changed we know the program counter is directly to the
			//     right of the matching ']' (which satisfies our post condition).
			while(nestLevel > 0)
			{
				var currentInstruction = _program[_programCounter];
				if (currentInstruction == ']') nestLevel--;
				else if (currentInstruction == '[') nestLevel++;
				_programCounter++;
			}
			
			// Postcondition: Program Counter is pointing to the instruction immediately following the matching
			//                ']' instruction. Or we terminate the program due to index out of bounds exception.
		}
		
		// This only "plays" around with program. Seems like it should be part of
		// "program" class.
		public void JumpBackward()
		{
			System.Diagnostics.Debug.Assert(_program[_programCounter-1] == ']');
			_programCounter -= 2;
			int nestLevel = 1;
			
			
			// discuss how to do LOOPS/ALGORITHMS PROPERY
			// precondition/postcondition
			// -- making progress and terminates
			// -- loop invariant
			// -- induction (base case, inductive case)
			
			// PreCondition - program counter is pointing at the character to "left" of ']' and nestLevel equals 1
			// Loop Invariant: the nestLevel indicates how deeply nested inside of loops we are relative to the loop
			//                 signfied by the ']' instruction that we are executing. Level 1 means this loop.
			//                 0 means we are not inside of this loop. 2 means we are nested one loop deeper than
			//                 this loop. After an iteration of the loop, the program counter is decreased by 1.
			//
			// Base Case: When we first encounter the loop nestLevel is 1 and we guarantee that 
			//            Debug.Assert and the initialization above. We know we are inside of 1 loop.
			// Inductive Case: Assume we are at nest level n and program counter q. If the current instruction is a
			//                 ']' then we increment the nestLevel to (n+1). If the current instruction is a '['
			//                  then we decrement the nestLevel to (n-1). For both cases the loop invariant holds.
			// Termination: The loop is not guaranteed to terminate "gracefully". If the program has semantic errors
			//              (like a non-matching '[' for a ']') this loop will terminate with an index out of bounds
			//              exception. If the program has no semantic errors (all loop chars have matches) then
			//              we will find the matching '[' and the program counter will be pointing to the character
			//              preceding it when we exit the loop (Note: this means if the match is at program counter 0,
			//              then the program counter will actually be -1 when we leave the loop.) And by the loop
			//              invariant we know that the nestLevel will then be 0 and we will exit the loop. 
			//              Since the instruction is supposed to leave the program counter pointing at the '['
			//              we must increment it by 1.
			while(nestLevel > 0)
			{
				var currentInstruction = _program[_programCounter];
				if (currentInstruction == ']') nestLevel++;
				else if (currentInstruction == '[') nestLevel--;
				_programCounter--;
			}
			
			_programCounter++;
			
		}

	}
}

