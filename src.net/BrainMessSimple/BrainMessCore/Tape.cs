using System;
using System.Collections.Generic;

namespace Welch.Brainmess
{
	public class Tape
	{
		// This sorta feels like a class, for same reasons.
		LinkedList<int> tape;
		LinkedListNode<int> currentCell;
		
		public Tape ()
		{
			// this stuff used to be done in Main. WTF? What a mess to have to know how
			// to set up each set of data.
			tape = new LinkedList<int> ();
			currentCell = tape.AddFirst(0);
			
		}
		
		public void MoveForward()
		{
			currentCell = currentCell.Next;
			if (currentCell == null)
			{
				currentCell = tape.AddLast(0);
			}
		}
		
		public void MoveBackward()
		{
			currentCell = currentCell.Previous;
			if (currentCell == null)
			{
				currentCell = tape.AddFirst(0);
			}
		}
		
		public void Increment()
		{
			currentCell.Value++;
		}
		
		public void Decrement()
		{
			currentCell.Value--;
		}
		
		public int Current
		{
			get
			{
				return currentCell.Value;
			}
			set {
				currentCell.Value = value;
			}
		}
	}
}

