package welch.brainmess;

import org.junit.Test;
import static org.junit.Assert.*;

// In these tests I take a different approach than normal. I compare
// expected string representations of tapes with the actual strings 
// returned by toString of the tape under test.

// I normally wouldn't do this. However, I see Fowler and lots
// of java test examples that do this. Seems very fragile. What if you
// want to change your string representation? Every test breaks.
// However, it is very easy to understand the test.

public class TestTape
{
	@Test
	public void testConstructor()
	{
		// Should have one element set to 0.
		assertEquals("[*0*]", new Tape().toString());
	}
	
	@Test
	public void testIncrement()
	{
		Tape tape = new Tape();
		tape.increment();
		assertEquals("[*1*]", tape.toString());
	}
	
	@Test
	public void testBackwardFromBeginning()
	{
		Tape tape = new Tape();
		// increment first cell, so we can verify new cell comes before
		tape.increment();
		tape.moveBackward();
		assertEquals("[*0*, 1]", tape.toString());
	}
	
	@Test
	public void testForwardFromEnd()
	{
		Tape tape = new Tape();
		// increment first cell, so we can verify new cell comes after
		tape.increment();
		tape.moveForward();
		assertEquals("[1, *0*]", tape.toString());
	}
	
	@Test
	public void testDecrement()
	{
		Tape tape = new Tape();
		tape.decrement();
		assertEquals("[*-1*]", tape.toString());
	}
	
	@Test
	public void testCurrentIndexOnNewTape()
	{
		Tape tape = new Tape();
		assertEquals("[*0*]", tape.toString());
	}
	
	@Test
	public void testBackward()
	{
		// test backward from location other than beginning
		Tape tape = new Tape();
		tape.moveForward();
		tape.moveBackward();
		assertEquals("[*0*, 0]", tape.toString());
	}
	
	@Test
	public void testForward()
	{
		// test forward from location other than end
		Tape tape = new Tape();
		tape.moveBackward();
		tape.moveBackward();
		assertEquals("[*0*, 0, 0]", tape.toString());
	}
	

}
