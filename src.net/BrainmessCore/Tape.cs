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
    /// is not exactly correct. One doesn't think of a tape moving itself forward or backward. Something does that to the 
    /// tape. In any case, it is a convenient name, but if it were to cause confusion it should be changed.)
    /// </summary>
    public class Tape
    {

        // Mutable State
        private LinkedListNode<int> _currentCell;

        /// <summary>
        /// Creates a tape with a value of 0 in every cell. 
        /// </summary>
        public static Tape Default
        {
            get { return LoadState(Enumerable.Range(0, 1)); }
        }

        /// <summary>
        /// Creates a tape that has the specified values located sequentially somewhere in the middle
        /// of the tape. The cursor is set to point to the cell indicated by <paramref name="position"/>.
        /// All other cells on the tape are set to 0.
        /// </summary>
        /// <param name="cells">A sequence of integers to load onto the new tape.</param>
        /// <param name="position">An "index" into <paramref name="cells"/> that indicates which one should
        /// be considered the current cell for the initial state of the tape.</param>
        public static Tape LoadState(IEnumerable<int> cells, int position = 0)
        {
            if (cells == null) throw new ArgumentNullException("cells");
            var array = cells.ToArray(); 
            
            if (array.Length == 0) throw new ArgumentException("This collection is expected to have at least one element. Use Tape.Default if you don't want to customize the state.", "cells");
            if (position < 0) throw new ArgumentOutOfRangeException("position");
            if (position > array.Length - 1) throw new ArgumentOutOfRangeException("position");

            var list = new LinkedList<int>(array);
            int counter = position;
            var currentNode = list.First;
            
            // Find the linked list node that corresponds to the specified position.
            while(counter > 0)
            {
                Debug.Assert(currentNode != null); // This is used to remove Resharper warning. Our checks above gaurantee this will never throw.
                currentNode = currentNode.Next;
                counter--;
            }

            return new Tape(currentNode);
        }

        /// <summary>
        /// Clones the state of this instance and returns it as an 
        /// instance of <see cref="State"/>.
        /// </summary>
        /// <returns></returns>
        public State GetState()
        {
            return State.From(this);
        }

        /// <summary>
        /// Represents the state of a Tape at a specific point in time.
        /// All of the cells that have ever been visited are copied into an array
        /// named Cells, and the current cell is indicated by an index into that
        /// array named Position.
        /// </summary>
        public class State
        {
            /// <summary>
            /// Indicate the cell that was current at the moment this instance
            /// was created.
            /// </summary>
            public int Position { get; private set; }
            
            /// <summary>
            /// The sequence of integers that were on the tape (in the order they were on the 
            /// tape) at the moment this instance was created.
            /// </summary>
            public ReadOnlyCollection<int> Cells { get; private set; }

            /// <summary>
            /// Creates an instance of <see cref="State"/> by copying the state information
            /// from <paramref name="tape"/>.
            /// </summary>
            /// <param name="tape"></param>
            /// <returns></returns>
            public static State From(Tape tape)
            {
                var list = tape._currentCell.List;
                var node = list.First;
                var position = 0;
                while(node != tape._currentCell)
                {
                    Debug.Assert(node != null); // This quiets the Resharper warning. We no for sure this will never be null.
                    node = node.Next;
                    position++;
                }

                return new State
                           {
                               Cells = new ReadOnlyCollection<int>(tape._currentCell.List.ToArray()),
                               Position = position
                           };
            }
        }

        private Tape(LinkedListNode<int> startingCell)
        {
            // This is a private constructor so I assume that I'll always call it correctly.
            // The Debug.Assert leaves a check in place that documents the assumption and catches
            // any errors during unit tests in case I refactor and forget.

            // make sure that startingCell is linked into a list.
            Debug.Assert(startingCell.List != null);
            _currentCell = startingCell;
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

