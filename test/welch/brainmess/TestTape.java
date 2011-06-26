package welch.brainmess;

import org.junit.Test;
import static org.junit.Assert.*;

public class TestTape
{
	@Test
	public void testConstructor()
	{
		// Should have one element set to 0.
		assertEquals("[0]", new Tape().toString());
	}
	
	@Test
	public void testIncrement()
	{
		Tape tape = new Tape();
		tape.increment();
		assertEquals("[1]", tape.toString());
	}
	
	@Test
	public void testBackwardFromBeginning()
	{
		Tape tape = new Tape();
		// increment first cell, so we can verify new cell comes before
		tape.increment();
		tape.moveBackward();
		assertEquals("[0, 1]", tape.toString());
	}
	
	@Test
	public void testForwardFromEnd()
	{
		Tape tape = new Tape();
		// increment first cell, so we can verify new cell comes after
		tape.increment();
		tape.moveForward();
		assertEquals("[1, 0]", tape.toString());
	}
	
	@Test
	public void testDecrement()
	{
		Tape tape = new Tape();
		tape.decrement();
		assertEquals("[-1]", tape.toString());
	}
	
	@Test
	public void testCurrentIndexOnNewTape()
	{
		Tape tape = new Tape();
		assertEquals(0, tape.currentIndex());
	}
	
	@Test
	public void testBackward()
	{
		// test backward from location other than beginning
		Tape tape = new Tape();
		tape.moveForward();
		tape.moveBackward();
		assertEquals(0, tape.currentIndex());
	}
	
	@Test
	public void testForward()
	{
		// test forward from location other than end
		Tape tape = new Tape();
		tape.moveBackward();
		tape.moveBackward();
		assertEquals(0, tape.currentIndex());
	}
	
	@Test
	public void testForwardSeries()
	{
		Tape tape = new Tape();
		for (int i = 0; i < 10; i++)
		{
			assertEquals(i, tape.currentIndex());
			tape.moveForward();
		}
		
		for (int i = 10; i >= 0; i--)
		{
			assertEquals(i, tape.currentIndex());
			tape.moveBackward();
		}
		
	}
	
	@Test
	public void testBackwardSeries()
	{
		Tape tape = new Tape();
		for (int i = 0; i < 10; i++)
		{
			assertEquals(0, tape.currentIndex());
			tape.moveBackward();
		}
		
		for (int i = 0; i < 10; i++)
		{
			assertEquals(i, tape.currentIndex());
			tape.moveForward();
		}
	}

}
