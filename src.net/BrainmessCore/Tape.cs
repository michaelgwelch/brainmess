using System;
using System.Diagnostics;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Linq;

namespace Welch.Brainmess
{
    /// <summary>
    /// Represents an infinite tape of numbers. An instance of this tape is used as a memory store during the execution of a
    /// Brainmess program. The tape maintains a cursor which points to a cell. This cell is considered the "current" cell.
    /// The methods MoveForward, MoveBackward, Current, Increment, and Decrement all take action relative to the current' cell.
    /// (Do the methods on this really match a tape or a "tape drive"? Perhaps the name of this class
    /// is not exactly correct. One doesn't think of a tape moving itself forward or backward, something does that to the 
    /// tape. In any case, it is a convenient name, but if it were to cause confusion it should be changed.)
    /// </summary>
    public class Tape
    {

        // Mutabale State
        private LinkedListNode<int> _currentCell;

        /// <summary>
        /// Creates a tape with a value of 0 in every cell. The tape is "infinite" in both directions. The
        /// cursor is set to somewhere in the "middle".
        /// </summary>
        public static Tape Default()
        {
            return LoadState(Enumerable.Range(0,1));
        }

        /// <summary>
        /// Creates a tape that has the specified values located sequentially somewhere in the middle
        /// of the tape. The cursor is set to point to the first value or last value in the list according
        /// to the <paramref name="position"/> parameter.
        /// </summary>
        /// <remarks>
        /// The instance of Tape that is returned maintains a reference to <paramref name="cells"/>. This means
        /// that any changes to the list affect the Tape and vice versa. This is useful for
        /// testing purposes but strictly speaking isn't what one expects to find when running a Brainmess program.
        /// </remarks>
        /// <exception cref='ArgumentNullException'>
        /// Is thrown when cells is <see langword="null" /> .
        /// </exception>
        /// <exception cref="ArgumentOutOfRangeException">
        /// If a value other than <see cref="InitialCursorPosition.Head"/> or <see cref="InitialCursorPosition.Tail"/>
        /// is passed in for position.
        /// </exception>
        public static Tape LoadState(IEnumerable<int> cells, int currentCell = 0)
        {
            if (cells == null) throw new ArgumentNullException("cells");
            var array = cells.ToArray(); 
            
            if (array.Length == 0) throw new ArgumentException("This collection is expected to have at least one element. Use Tape.Default if you don't want to customize the state.", "cells");
            if (currentCell < 0) throw new ArgumentOutOfRangeException("currentCell");
            if (currentCell > array.Length - 1) throw new ArgumentOutOfRangeException("currentCell");

            var list = new LinkedList<int>(array);
            int counter = currentCell;
            var currentNode = list.First;
            
            while(counter > 0)
            {
                currentNode = currentNode.Next;
                counter--;
            }

            return new Tape(list, currentNode);
        }

        public State GetState()
        {
            return State.From(this);
        }

        public class State
        {
            public int Position { get; private set; }
            public ReadOnlyCollection<int> Cells { get; private set; }
            public static State From(Tape tape)
            {
                var list = tape._currentCell.List;
                var node = list.First;
                var position = 0;
                while(node != tape._currentCell)
                {
                    node = node.Next;
                    position++;
                }

                return new State()
                           {
                               Cells = new ReadOnlyCollection<int>(tape._currentCell.List.ToArray()),
                               Position = position
                           };
            }
        }

        private Tape(LinkedList<int> cells, LinkedListNode<int> position)
        {
            // This is a private constructor so I expect I'm calling it properly.
            // However, these asserts are to make sure I don't forget. They document the constraints.
            Debug.Assert(cells != null);
            Debug.Assert(cells.Any());
            Debug.Assert(position.List == cells);

            _currentCell = position;
        }

        /// <summary>
        /// Moves the cursor forward one position.
        /// </summary>
        public void MoveForward()
        {
            _currentCell = _currentCell.MoveForward();
        }

        /// <summary>
        /// Moves the cursor backward one position.
        /// </summary>
        public void MoveBackward()
        {
            _currentCell = _currentCell.MoveBackward();
        }

        /// <summary>
        /// Increments the value at the cursor.
        /// </summary>
        public void Increment()
        {
            _currentCell.Value++;
        }

        /// <summary>
        /// Decrements the value at the cursor.
        /// </summary>
        public void Decrement()
        {
            _currentCell.Value--;
        }

        /// <summary>
        /// Gets or sets the value at the cursor.
        /// </summary>
        public int Current
        {
            get
            {
                return _currentCell.Value;
            }
            set
            {
                _currentCell.Value = value;
            }
        }
    }
}

