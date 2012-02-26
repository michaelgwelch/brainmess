package welch.brainmess;

/**
 * Represents and encapsulates the entire environment that a brainmess program runs within.
 * This exposes all of the methods that an instruction needs access to execute itself.
 * @author mgwelch
 *
 */
public interface ExecutionContext {

	/**
	 * Tells the tape to move forward
	 */
	public abstract void moveForward();

	/**
	 * Tells the tape to move backward.
	 */
	public abstract void moveBackward();

	/**
	 * Increments the current cell of the tape by one.
	 */
	public abstract void increment();

	/**
	 * Decrements the current cell of the tape by one.
	 */
	public abstract void decrement();

	/**
	 * Reads a character from the input and writes its
	 * integer value to the current cell of the tape.
	 */
	public abstract void input();

	/**
	 * Reads the value from the current cell of the tape
	 * and writes it as a character to the output.
	 */
	public abstract void ouput();

	/**
	 * Tests the current cell of the tape. If it is 0 this jumps the
	 * program forward to the the matching ']' instruction.
	 * The next instruction to be fetched will then be the one
	 * immediately following the ']' instruction.
	 */
	public abstract void testAndJumpForward();

	/**
	 * Tests the current cell. If it is not 0 this jumps back
	 * to the matching '[' instruction. The next instruction
	 * to be fetched will then be the '[' instruction.
	 */
	public abstract void testAndJumpBackward();

}