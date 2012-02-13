using System;
using System.Collections.Generic;
using System.IO;
namespace Welch.Brainmess
{
	// Note: Testing is important. But so is thinking. In this program I've proved the correctness
	// of the only two "algorithms" I had to write. Testing is great to try to make sure you 
	// didn't miss anything. But the upfront thinking and proof will help you construct a better solution.
	class MainClass
	{
		public static void Main (string[] args)
		{
			
			var program = new ProgramStream(
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
		/*	
			program = new ProgramStream(@",
------------------------------------------------
[->++<]
>
++++++++++++++++++++++++++++++++++++++++++++++++.");
	
		/*	
			program = new ProgramStream( @"
++++++++[<+++++++++>-]<.>+++++[<++++++>-]<-.
+++++++..+++.>++++++++[<<++++>>-]<<.
>>++++[<------>-]<.>++++[<++++++>-]<.
+++.------.--------.<+.
");*/
			var tape = Tape.Zeros();
			
			var input = Console.In;
			var output = Console.Out;
			
			Interpreter interpreter = new Interpreter(program, tape, input, output);
			interpreter.Run();
			/*
			output.Close();
			input.Close();*/
			
		}
		
		

		
		

	}
}
