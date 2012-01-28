using System;
using System.Collections.Generic;
using System.IO;
namespace BrainMessSimple
{
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
			System.Diagnostics.Debug.Assert(program[programCounter-1] == '[');
			int nestLevel = 1;
			
			while(nestLevel > 0)
			{
				var currentInstruction = program[programCounter];
				if (currentInstruction == ']') nestLevel--;
				else if (currentInstruction == '[') nestLevel++;
				programCounter++;
			}
			
		}
		
		// This only "plays" around with program. Seems like it should be part of
		// "program" class.
		private void JumpBackward()
		{
			System.Diagnostics.Debug.Assert(program[programCounter-1] == ']');
			programCounter -= 2;
			int nestLevel = 1;
			
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
