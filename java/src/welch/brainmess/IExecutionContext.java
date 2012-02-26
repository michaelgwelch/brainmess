package welch.brainmess;

public interface IExecutionContext {

	public abstract void moveForward();

	public abstract void moveBackward();

	public abstract void increment();

	public abstract void decrement();

	public abstract void input();

	public abstract void ouput();

	/**
	 * Tests the current cell. If it is 0 this jumps the
	 * program forward to the matching ']' instruction.
	 * The next instruction to be fetched will then be the one
	 * immeidately following the ']' instruction.
	 */
	public abstract void testAndJumpForward();

	/**
	 * Tests the current cell. If it is not 0 this jumps back
	 * to the matching '[' instruction. The next instruction
	 * to be fetched will then be the one immediately following the
	 * '['.
	 */
	public abstract void testAndJumpBackward();

}