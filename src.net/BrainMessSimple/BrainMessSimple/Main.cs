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
			
			instance._program = new ProgramStream(
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
]");
			
			instance._program = new ProgramStream(@",
------------------------------------------------
[->++<]
>
++++++++++++++++++++++++++++++++++++++++++++++++.");
	
			/*
			instance._program = new ProgramStream( @"
++++++++[<+++++++++>-]<.>+++++[<++++++>-]<-.
+++++++..+++.>++++++++[<<++++>>-]<<.
>>++++[<------>-]<.>++++[<++++++>-]<.
+++.------.--------.<+.
");*/
			
			instance.tape = new LinkedList<int>();
			instance.currentCell = instance.tape.AddFirst(0);
			
			instance.input = Console.In;
			instance.output = Console.Out;
			
			instance.Interpret();
			
			instance.output.Close();
			instance.input.Close();
		}
		
		
		private ProgramStream _program;
		
		// This sorta feels like a class, for same reasons.
		LinkedList<int> tape;
		LinkedListNode<int> currentCell;
		
		TextWriter output;
		TextReader input;
		
		// uses "program" data, "tape" data, has information about instructions. Seems
		// like an interaction between instructions, tape, and program.
		
		// Refactorings you can use. Extract method on each case body.
		
		// Unit testing this requires that you write "programs" to exercise all possibilities.
		// Which is probably doable but tricky. 
		public void Interpret()
		{
			while(!_program.EndOfProgram)
			{
				char currentInstruction = _program.Fetch();
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
					if (currentCell.Value == 0) _program.JumpForward();
					break;
				case ']':
					if (currentCell.Value != 0) _program.JumpBackward();
					break;
				default:
					break;
				}
			}
			
		}
		
		

	}
}
