using System;
using System.Collections.Generic;
using System.IO;
namespace BrainMessSimple
{
	// Note: Testing is important. But so is thinking. In this program I've proved the correctness
	// of the only two "algorithms" I had to write. Testing is great to try to make sure you 
	// didn't miss anything. But the upfront thinking and proof will help you construct a better solution.
	class MainClass
	{
		public static void Main (string[] args)
		{
			var instance = new MainClass();
			instance.program = 
				@"+++++++++++ number of digits to output
> #1
+ initial number
>>>> #5
++++++++++++++++++++++++++++++++++++++++++++ (comma)
> #6
++++++++++++++++++++++++++++++++ (space)
<<<<<< #0
[
> #1
3
copy #1 to #7
[>>>>>>+>+<<<<<<<-]>>>>>>>[<<<<<<<+>>>>>>>-]
<
divide #7 by 10 (begins in #7)
[
  >
  ++++++++++  set the divisor #8
  [
    subtract from the dividend and divisor
    -<-
    if dividend reaches zero break out
      copy dividend to #9
      [>>+>+<<<-]>>>[<<<+>>>-]
      set #10
      +
      if #9 clear #10
      <[>[-]<[-]]
      if #10 move remaining divisor to #11
      >[<<[>>>+<<<-]>>[-]]
    jump back to #8 (divisor possition)
<< ]
  if #11 is empty (no remainder)
  increment the quotient #12
  >>> #11
  copy to #13
  [>>+>+<<<-]>>>[<<<+>>>-]
  set #14
  +
  if #13 clear #14
  <[>[-]<[-]]
  if #14 increment quotient
  >[<<+>>[-]]
  <<<<<<< #7
]
quotient is in #12 and remainder is in #11
>>>>> #12
if #12 output value plus offset to ascii 0
[++++++++++++++++++++++++++++++++++++++++++++++++.[-]]
subtract #11 from 10
++++++++++  #12 is now 10
< #11
[->-<]
> #12
4
output #12 even if it’s zero
  ++++++++++++++++++++++++++++++++++++++++++++++++.[-]
  <<<<<<<<<<< #1
  check for final number
  copy #0 to #3
  <[>>>+>+<<<<-]>>>>[<<<<+>>>>-]
  <- #3
  if #3 output (comma) and (space)
  [>>.>.<<<[-]]
  << #1
  [>>+>+<<<-]>>>[<<<+>>>-]<<[<+>-]>[<+>-]<<<-
]";
			instance.program = @",
------------------------------------------------
[->++<]
>
++++++++++++++++++++++++++++++++++++++++++++++++.";
	
			/*
			instance.program = @"
++++++++[<+++++++++>-]<.>+++++[<++++++>-]<-.
+++++++..+++.>++++++++[<<++++>>-]<<.
>>++++[<------>-]<.>++++[<++++++>-]<.
+++.------.--------.<+.
";*/
			
			instance.tape = new LinkedList<int>();
			instance.currentCell = instance.tape.AddFirst(0);
			
			instance.input = Console.In;
			instance.output = new StreamWriter(Console.Out);
			
			instance.Interpret();
			
			instance.output.Close();
			instance.input.Close();
		}
		
		
		// This sorta feels like a class. Two pieces of state that are very closely
		// related. Look out for stuff like this. Notice that they are always used 
		// together.
		int programCounter = 0;
		string program;
		
		// This sorta feels like a class, for same reasons.
		LinkedList<int> tape;
		LinkedListNode<int> currentCell;
		
		StreamWriter output;
		TextReader input;
		
		// uses "program" data, "tape" data, has information about instructions. Seems
		// like an interaction between instructions, tape, and program.
		
		// Refactorings you can use. Extract method on each case body.
		
		// Unit testing this requires that you write "programs" to exercise all possibilities.
		// Which is probably doable but tricky. 
		public void Interpret()
		{
			while(!EndOfProgram)
			{
				char currentInstruction = Fetch();
				switch (currentInstruction)
				{
				case '>':
					currentCell = currentCell.Next;
					if (currentCell == null) currentCell = tape.AddLast(0);
					break;
				case '<':
					currentCell = currentCell.Previous;
					if (currentCell == null) currentCell = tape.AddFirst(0);
					break;
				case '+':
					currentCell.Value++;
					break;
				case '-':
					currentCell.Value--;
					break;
				case '.':
					output.Write((char)currentCell.Value);
					break;
				case ',':
					currentCell.Value = input.Read();
					break;
				case '[':
					if (currentCell.Value == 0) JumpForward();
					break;
				case ']':
					if (currentCell.Value != 0) JumpBackward();
					break;
				default:
					break;
				}
			}
			
		}
		
		// Should be part of "program" class. It only uses that data.
		private void JumpForward()
		{
			// Precondition: Program Counter is pointing to the instruction immediately following a '[' instruction
			System.Diagnostics.Debug.Assert(program[programCounter-1] == '[');
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
				var currentInstruction = program[programCounter];
				if (currentInstruction == ']') nestLevel--;
				else if (currentInstruction == '[') nestLevel++;
				programCounter++;
			}
			
			// Postcondition: Program Counter is pointing to the instruction immediately following the matching
			//                ']' instruction. Or we terminate the program due to index out of bounds exception.
		}
		
		// This only "plays" around with program. Seems like it should be part of
		// "program" class.
		private void JumpBackward()
		{
			System.Diagnostics.Debug.Assert(program[programCounter-1] == ']');
			programCounter -= 2;
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
				var currentInstruction = program[programCounter];
				if (currentInstruction == ']') nestLevel++;
				else if (currentInstruction == '[') nestLevel--;
				programCounter--;
			}
			
			programCounter++;
			
		}
		
		// Should be part of program class.
		private char Fetch()
		{
			var instruction = program[programCounter];
			programCounter++;
			return instruction;
		}
		
		// should be part of program class.
		private bool EndOfProgram { get { return programCounter >= program.Length; } }
	}
}
