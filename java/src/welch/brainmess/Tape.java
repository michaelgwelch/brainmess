package welch.brainmess;

import java.util.LinkedList;
import java.util.List;
import java.util.concurrent.atomic.AtomicReference;

/**
 * Represents the tape of a Brainmess machine.
 * @author Michael Welch
 *
 */
public class Tape
{

	
	// Do not access this directly. Use tapeFormatter() getter.
	// It sets this instance only when needed. It is also thread-safe in setting
	// this value.
	private final AtomicReference<ListFormatter<Integer>> TAPE_FORMATTER = new AtomicReference<ListFormatter<Integer>>();
	
	
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
	private int currentIndex()
	{
		return traveler.currentIndex();
	}
	
	private ListFormatter<Integer> tapeFormatter() {
		if (TAPE_FORMATTER.get() == null) {
			TAPE_FORMATTER.compareAndSet(null, new ListFormatter<Integer>("[", "]", ", ", new TapeElementFormatter()));
		}
		return TAPE_FORMATTER.get();
	}
	
	/**
	 * Converts this instance into its string representation.
	 */
	public String toString()
	{
		return tapeFormatter().format(list);
	}
	
	private class TapeElementFormatter implements ListFormatter.ElementFormatter<Integer> {

		@Override
		public String format(Integer value, int index) {
			if (value == null) throw new NullPointerException("Attempted to format a null integer");
		    final int currentIndex = currentIndex();
		    return (currentIndex == index) ? String.format("*%d*", value) : value.toString();
		}
		
	}
	
}
