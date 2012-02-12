using System;
using System.Collections.Generic;
using System.Linq;
using NUnit.Framework;

namespace Welch.Brainmess
{
	[TestFixture()]
	public class TapeTests
	{
		
		// Some bad smells. Several of my tests require that my Act phase uses two different methods
		// The one I'm testing and another one to cause a side effect.
		
		// Another thing that permeates all of this is that I construct tapes in Assembly phases
		// But I don't really have any tests of the static constructors.
		
		
		[Test()]
		public void Current_ConstructDefault_ExpectCurrentIsZero()
		{
			// The "simplest" test. We create an instance and expect that Current is 0
			Assert.AreEqual(0, Tape.Zeros().Current);
		}
		
		[Test()]
		public void Current_ConstructPrePopulatedTape_ExpectCurrentToMatch()
		{
			// Assemble
			int expectedValue = 55;		
			var list = new LinkedList<int> ();
			list.AddFirst(expectedValue);
			
			// Act
			var tape = Tape.Wrap(list);
			
			// Assert - The first value in the list should equal the value of the current cell on the tape.
			Assert.AreEqual(expectedValue, tape.Current);
		}

		[Test]
		public void Increment_ConstructFromValues_ExpectIncrementToIncrementTheCell()
		{
			// Assemble
			int expectedValue = 23;
			
			var list = new LinkedList<int> ();
			var firstCell = list.AddFirst(expectedValue - 1);
			
			var tape = Tape.Wrap(list);
			
			// Act
			tape.Increment();
			
			// Assert - Note I am using firstCell to check the result rather than Current (because I don't want this test to rely on Current)
			Assert.AreEqual(expectedValue, firstCell.Value);
			
		}
		
		[Test]
		public void Decrement_ConstructFromValues_ExpectDecrementToDecrementTheCell()
		{
			// Assemble
			int expectedValue = 45;
			
			var list = new LinkedList<int> ();
			var firstCell = list.AddFirst(expectedValue + 1);
			
			var tape = Tape.Wrap(list);
			
			// Act
			tape.Decrement();
			
			// Assert
			Assert.AreEqual(expectedValue, firstCell.Value);
		}
		
		[Test]
		public void MoveForward_ConstructFromSingleCellListMoveFowardAndMutate_ExpectLastCellMatchedMutatedValue()
		{
			// This test is going to use Current setter to mutate a known cell to help us validate that
			// MoveForward did work correctly by checking that the correct cell was mutated. 
			

			// Assemble
			var expectedValue = -234;
			var list = new LinkedList<int> ();
			var tape = Tape.Wrap(list);
			
			// Act
			tape.MoveForward(); // Method being tested.
			tape.Current = expectedValue; // use Current to mutate tape 
			
			// Assert
			Assert.AreEqual(expectedValue, list.Last.Value);
			
			
		}

		[Test]
		public void MoveForward_ConstructFromMultiCellListMoveForwardAndMutate_ExpectSecondCellMatchesValue()
		{
			// Assemble
			var initialList = new int[] {1,3,5,7,11,13,17,19};
			var expectedList = new int[] {1,9,5,7,11,13,17,19}; // expect second element to change.
			
			var list = new LinkedList<int> (initialList);
			var tape = Tape.Wrap(list);
			
			// Act
			tape.MoveForward();
			tape.Current = expectedList [1]; // grap the value from second cell
			
			// Assert
			CollectionAssert.AreEqual(expectedList, list.ToArray());
			
		}
		
		[Test]
		public void MoveBackward_ConstructFromMutiCellListMoveBackwardAndMutate_ExpectSecondFromLastCellMatchesValue()
		{
			// Assemble - construct a tape from a list and set current cell to last value in list.
			var initialList = new int[] {19,17,13,11,7,5,3,1};
			var expectedList = new int[]{19,17,13,11,7,5,9,1};
			
			var list = new LinkedList<int> (initialList);
			var tape = Tape.Wrap(list, Tape.InitialCursorPosition.Tail);
			
			// Act
			tape.MoveBackward();
			tape.Current = expectedList [expectedList.Length - 2]; // grab the value second from the end
			
			// Assert
			CollectionAssert.AreEqual(expectedList, list.ToArray());
		}
		
		[Test]
		public void MoveBackward_ConstructFromSingleCellListMoveBackwardAndMutate_ExpectFirstCellToMatch()
		{
			// Assemble
			var expectedValue = -456;
			var list = new LinkedList<int> ();
			var tape = Tape.Wrap(list);
			
			// Act
			tape.MoveBackward();
			tape.Current = expectedValue;
			
			// Assert
			Assert.AreEqual(expectedValue, list.First.Value);
			
		}
	
		[Test]
		public void Wrap_PassNull_ExpectArgumentNullException()
		{
			try
			{
				Tape.Wrap(null);
				Assert.Fail("Expect ArgumentNullException");
			} catch (ArgumentNullException)
			{
			}
		}
		
		[Test]
		public void Wrap_PassBadPosition_ExpectArgumentOutOfRangeException()
		{
			try
			{
				Tape.Wrap(new LinkedList<int> (), (Tape.InitialCursorPosition)55);
				Assert.Fail("Expected an ArgumentOutOfRangeException");
			} catch (ArgumentOutOfRangeException)
			{
			}
		}
		
		
		// bugs: if increment or decrement wrap. But that's a limitation of our design. Not of any of our methods. 
	}
}

