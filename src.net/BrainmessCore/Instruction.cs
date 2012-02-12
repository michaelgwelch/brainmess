using System;
using System.IO;

namespace Welch.Brainmess
{
	/// <summary>
	/// Represents a Brainmess operation. There are 9 of them: the 8 defined explicitly by 
	/// Brainmess and a 9th one called NoOperation which does nothing.
	/// </summary>
	public class Instruction
	{
		private readonly Action<IExecutionContext> _action;

		private Instruction (Action<IExecutionContext> action)
		{
			_action = action;
		}

		public void Execute(IExecutionContext context)
		{
			_action(context);
		}

		// the exeuction context breaks the instrucion's dependencies on so many objects,
		// but it is still responsible for figuring out what should happen to which object (but thru method names).
		
		// but you could easily argue that each of these actions should just be one line of code and the real
		// logic is in the context (which is the Interpeter)
		
		public static readonly Instruction MoveForward =
            new Instruction (context => context.MoveTapeForward());
		
		public static readonly Instruction MoveBackward =
            new Instruction (context => context.MoveTapeBackward());
		
		public static readonly Instruction Increment =
            new Instruction (context => context.IncrementTape());
		
		public static readonly Instruction Decrement =
            new Instruction (context => context.DecrementTape());
		
		public static readonly Instruction Input =
            new Instruction (context => context.CurrentTapeValue = context.ReadFromInput());
		
		public static readonly Instruction Output =
            new Instruction (context => context.WriteToOutput((char)context.CurrentTapeValue));
		
		public static readonly Instruction TestAndJumpFoward =
            new Instruction (context => 
		{
			if (context.CurrentTapeValue == 0)
			{
				context.JumpProgramForward();
			} 
		});
		
		public static readonly Instruction TestAndJumpBackward =
            new Instruction (context => {
			if (context.CurrentTapeValue != 0)
			{
				context.JumpProgramBackward();
			} });
		
		public static readonly Instruction NoOperation =
            new Instruction (context => { });

		public static Instruction FromInt(int characterValue)
		{
			switch (characterValue)
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

