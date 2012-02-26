package welch.brainmess;

// In this implementation is Instruction really pulling its weight?
// In it's form it is a) switching on character to create an Instruction
// b) Using the instruction to call appropriate method on IExecutionContext.
// It certainly is easy to understand, and the predefined instances make
// it easy to create an Instruction and test it.

/**
 * Represents a Brain Mess Instruction.
 * @author Michael Welch
 *
 */
public enum Instruction
{
	/**
	 * Represents the instruction that moves the tape forward one cell.
	 */
	MOVE_FORWARD
	{
		public void execute(ExecutionContext context) { context.moveForward(); }
	},
	
	/**
	 * Represents the instruction that moves the tape backward one cell.
	 */
	MOVE_BACKWARD
	{
		public void execute(ExecutionContext context) { context.moveBackward(); }
	},
	
	/**
	 * Represents the instruction that increments the value of the current cell.
	 */
	INCREMENT
	{
		public void execute(ExecutionContext context) { context.increment(); }
	},
	
	/**
	 * Represents the instruction that decrements the value of the current cell.
	 */
	DECREMENT
	{
		public void execute(ExecutionContext context) { context.decrement(); }
	},
	
	/**
	 * Represents the instruction that inputs a character and stores its integer
	 * value on the tape at the current location.
	 */
	INPUT
	{
		public void execute(ExecutionContext context) { context.input(); }
	},
	
	/**
	 * Represents the instruction that reads the value of the current cell and outputs
	 * the character it represents.
	 */
	OUTPUT
	{
		public void execute(ExecutionContext context) { context.ouput(); }
	},
	
	/**
	 * Represents the test and jump forward instruction.
	 */
	TEST_AND_JUMP_FORWARD
	{
		public void execute(ExecutionContext context) { context.testAndJumpForward(); }
	},
	
	/**
	 * Represents the test and jump backward instruction.
	 */
	TEST_AND_JUMP_BACKWARD
	{
		public void execute(ExecutionContext context) { context.testAndJumpBackward(); }
	},
	
	/**
	 * A no operation instruction. Used for all non-instruction characters in the program.
	 */
	NO_OPERATION
	{
		public void execute(ExecutionContext context) { }
	};
	
	
	/**
	 * Executes the current instruction.
	 * @param context The execution environment in which to execute the instruction.
	 */
	public abstract void execute(ExecutionContext context);
	
	/**
	 * Parses an Instruction from the specified character value.
	 * @param value
	 * @return
	 */
	public static Instruction parseInstruction(char value)
	{
		switch(value)
		{
		case '>':
			return Instruction.MOVE_FORWARD;
			
		case '<':
			return Instruction.MOVE_BACKWARD;
			
		case '+':
			return Instruction.INCREMENT;
			
		case '-':
			return Instruction.DECREMENT;
			
		case '.':
			return Instruction.OUTPUT;
			
		case ',':
			return Instruction.INPUT;
			
		case '[':
			return Instruction.TEST_AND_JUMP_FORWARD;
			
		case ']':
			return Instruction.TEST_AND_JUMP_BACKWARD;
			
		default:
			return Instruction.NO_OPERATION;
		}
	}
	
}
