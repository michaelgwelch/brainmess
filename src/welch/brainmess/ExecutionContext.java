package welch.brainmess;


import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.PrintStream;
import java.io.PrintWriter;
import java.io.Reader;

/**
 * Represents the execution environment for executing a brain mess program.
 * It also has a main method that is the entry point for executing a program.
 * @author Michael Welch
 *
 */
public class ExecutionContext
{

	private final Tape tape;
	private final Program program;
	private final PrintWriter output;
	private final Reader input;
	
	
	public ExecutionContext(Program program, PrintStream output, InputStream input)
	{
		this.program = program;
		this.output = new PrintWriter(output);
		this.input = new InputStreamReader(input);
		tape = new Tape();
	}
	
	/**
	 * Executes a brain mess program.
	 * @param args The first value in args is expected to be the name of a file
	 * that contains the brain mess program to execute.
	 */
	public static void main(String[] args)
	{
		Program p = new Program("/Users/mgwelch/helloworld.bm");
		ExecutionContext context = new ExecutionContext(p, System.out, System.in);
		
		while (!p.isEndOfProgram())
		{
			p.fetch().execute(context);
		}
	}
	
	
	public void moveForward()
	{
		tape.moveForward();
		
	}

	public void moveBackward()
	{
		tape.moveBackward();
	}

	public void increment()
	{
		tape.increment();
	}
	

	public void decrement()
	{
		tape.decrement();
	}

	public void input()
	{
		int value;
		try
		{
			value = input.read();
		} catch (IOException e)
		{
			throw new RuntimeException("An input exception was encountered. Program must terminate", e);
		}
		tape.setCurrent(value);
	}

	public void ouput()
	{
		int value = tape.getCurrent();
		output.write((char)value);
		output.flush();
	}

	/**
	 * Tests the current cell. If it is 0 this jumps the
	 * program forward to the matching ']' instruction.
	 * The next instruction to be fetched will then be the one
	 * immeidately following the ']' instruction.
	 */
	public void testAndJumpForward()
	{
		if (tape.getCurrent() == 0)
		{
			program.jumpForward();
		}
	}

	/**
	 * Tests the current cell. If it is not 0 this jumps back
	 * to the matching '[' instruction. The next instruction
	 * to be fetched will then be the one immediately following the
	 * '['.
	 */
	public void testAndJumpBackward()
	{
		if (tape.getCurrent() != 0)
		{
			program.jumpBackward();
		}
	}

}

	