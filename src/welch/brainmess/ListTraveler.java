package welch.brainmess;

import java.util.List;
import java.util.ListIterator;

/**
 * A variation of a ListIterator. Unlike a ListIterator, a ListTraveller
 * has a current element. It also creates elements in the list as needed to
 * allow for moveNext and movePrevious to always succeed. As such it has no
 * hasNext or hasPrevious methods.
 * @author Michael Welch
 *
 * @param <E> The type of the elements to store in the list.
 */
public class ListTraveler<E> 
{

	// An iterator over a list. We always make sure the cursor of the
	// iterator is positioned after the cell we consider to be the current value. 
	private final ListIterator<E> iterator;
	private final E defaultValue;
	
	private ListTraveler(ListIterator<E> iterator, E defaultValue)
	{
		this.iterator = iterator;
		this.defaultValue = defaultValue;
		moveNext();
	}
	
	/**
	 * Wraps a list inside of a ListTraveler.
	 * @param <E> The type of the element in the list
	 * @param list The list that we will travel
	 * @param defaultValue If a new node must be added to the list, it will be given this value.
	 * @return
	 */
	public static <E> ListTraveler<E> wrap(List<E> list, E defaultValue)
	{
		return new ListTraveler<E>(list.listIterator(), defaultValue);
	}
	
	/**
	 * Advances the traveler to the next element. If there is no next element,
	 * a new element will be created with the default value. 
	 */
	public void moveNext()
	{
		if (iterator.hasNext())
		{
			iterator.next();
		}
		else
		{
			addCell();
		}
	}

	/**
	 * Adds a new cell with the default value, it then makes sure that the iterator's
	 * state is adjusted so that it is ready for a set.
	 */
	private void addCell()
	{
		iterator.add(defaultValue);
		
		// After an add, the set/get methods of iterator may not be called
		// until next or previous are called. We call both to prepare the iterator
		// for the next potential set or get, while maintaining position.
		iterator.previous();
		iterator.next();
	}
	
	/**
	 * Advances the traveler to the previous cell. If there is not previous
	 * cell, then a new cell is added with the default value.
	 */
	public void movePrevious()
	{
		// The cursor is always after what we consider to be the
		// current element. To maintain this, we must back up twice and then 
		// go forward once.
		iterator.previous();
		
		if (iterator.hasPrevious())
		{
			iterator.previous();
			iterator.next();
		}
		else
		{
			addCell();
		}
		

	}

	/**
	 * Returns the value of the current element.
	 * @return The value of the current element.
	 */
	public E getCurrent()
	{
		// An iterator doesn't have a current value. To make it look like
		// it does
		// we backup and then go forward.
		iterator.previous();
		return iterator.next();
	}
	
	/**
	 * Sets the value of the current cell to the specified value.
	 * @param value
	 */
	public void setCurrent(E value)
	{
		iterator.set(value);
	}
	
	/**
	 * Returns the index of the current element.
	 * @return
	 */
	public int currentIndex()
	{
		return iterator.previousIndex();
	}
	
	
	
}
