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
public class ExecutionContext implements IExecutionContext
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
		Program p = new Program(args[0]);
		IExecutionContext context = new ExecutionContext(p, System.out, System.in);
		
		while (!p.isEndOfProgram())
		{
			p.fetch().execute(context);
		}
	}
	
	
	/* (non-Javadoc)
	 * @see welch.brainmess.IExecutionContext#moveForward()
	 */
	@Override
	public void moveForward()
	{
		tape.moveForward();
		
	}

	/* (non-Javadoc)
	 * @see welch.brainmess.IExecutionContext#moveBackward()
	 */
	@Override
	public void moveBackward()
	{
		tape.moveBackward();
	}

	/* (non-Javadoc)
	 * @see welch.brainmess.IExecutionContext#increment()
	 */
	@Override
	public void increment()
	{
		tape.increment();
	}
	

	/* (non-Javadoc)
	 * @see welch.brainmess.IExecutionContext#decrement()
	 */
	@Override
	public void decrement()
	{
		tape.decrement();
	}

	/* (non-Javadoc)
	 * @see welch.brainmess.IExecutionContext#input()
	 */
	@Override
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

	/* (non-Javadoc)
	 * @see welch.brainmess.IExecutionContext#ouput()
	 */
	@Override
	public void ouput()
	{
		int value = tape.getCurrent();
		output.write((char)value);
		output.flush();
	}

	/* (non-Javadoc)
	 * @see welch.brainmess.IExecutionContext#testAndJumpForward()
	 */
	@Override
	public void testAndJumpForward()
	{
		if (tape.getCurrent() == 0)
		{
			program.jumpForward();
		}
	}

	/* (non-Javadoc)
	 * @see welch.brainmess.IExecutionContext#testAndJumpBackward()
	 */
	@Override
	public void testAndJumpBackward()
	{
		if (tape.getCurrent() != 0)
		{
			program.jumpBackward();
		}
	}

}

	