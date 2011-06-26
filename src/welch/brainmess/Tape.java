package welch.brainmess;

import java.util.LinkedList;
import java.util.List;

/**
 * Represents the tape of a Brainmess machine.
 * @author Michael Welch
 *
 */
public class Tape
{
	/**
	 * The list used to store the tape values. This is 
	 * only stored so that we can do a toString on it.
	 */
	private final List<Integer> list;
	
	/**
	 * A traveler over list.
	 */
	private final ListTraveler<Integer> traveler;
	
	/**
	 * Creates a tape with one cell whose value is 0. New cells
	 * are added as necessary. Every new cell is created with a value of 0.
	 */
	public Tape()
	{
		this.list = new LinkedList<Integer>();
		this.traveler = ListTraveler.wrap(list, 0);
	}
	
	/**
	 * Increment the value of the current cell by 1.
	 */
	public void increment()
	{
		traveler.setCurrent(traveler.getCurrent() + 1);
	}
	
	/**
	 * Decrement the value of the current cell by 1.
	 */
	public void decrement()
	{
		traveler.setCurrent(traveler.getCurrent() - 1);
	}
	
	/**
	 * Gets the value of the current cell.
	 * @return
	 */
	public int getCurrent()
	{
		return traveler.getCurrent();
	}
	
	/**
	 * Sets the value of the current cell.
	 * @param value
	 */
	public void setCurrent(int value)
	{
		traveler.setCurrent(value);
	}
	
	/**
	 * Advances the tape one cell.
	 */
	public void moveForward()
	{
		traveler.moveNext();
	}
	
	/**
	 * Rewinds the tape one cell.
	 */
	public void moveBackward()
	{
		traveler.movePrevious();
	}
	
	/**
	 * Returns the index value of the current cell.
	 * @return
	 */
	public int currentIndex()
	{
		return traveler.currentIndex();
	}
	
	/**
	 * Converts this instance into its string representation.
	 */
	public String toString()
	{
		return list.toString();
	}
	
	
}
