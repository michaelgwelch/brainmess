using System;
using System.Diagnostics;
using System.Collections.Generic;
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
        /// <summary>
        /// Used to indicate the initial cursor position when initializing a tape with a set of values from a list.
        /// </summary>
        public enum InitialCursorPosition
        {
            /// <summary>
            /// Indicates that the initial cursor position should be located on the cell that corresponds
            /// to the first value in the list used to initialize a tape.
            /// </summary>
            Head,

            /// <summary>
            /// Inidicates that the initial cursor position should be located on the cell that corresponds
            /// to the last value in the list used to initialize a tape.
            /// </summary>
            Tail
        }

        // Mutabale State
        private LinkedListNode<int> _currentCell;

        /// <summary>
        /// Creates a tape with a value of 0 in every cell. The tape is "infinite" in both directions. The
        /// cursor is set to somewhere in the "middle".
        /// </summary>
        public static Tape Zeros()
        {
            var cells = new LinkedList<int>();

            return Wrap(cells);
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
        public static Tape Wrap(LinkedList<int> cells, InitialCursorPosition position = InitialCursorPosition.Head)
        {
            if (cells == null)
            {
                throw new ArgumentNullException("cells");
            }
            if (!Enum.IsDefined(typeof(InitialCursorPosition), position))
            {
                throw new ArgumentOutOfRangeException("position");
            }

            if (!cells.Any())
            {
                cells.AddFirst(0);
            }

            return new Tape(cells, position);
        }

        private Tape(LinkedList<int> cells, InitialCursorPosition position)
        {
            // This is a private constructor so I expect I'm calling it properly.
            // However, these asserts are to make sure I don't forget. They document the constraints.
            Debug.Assert(cells != null);
            Debug.Assert(cells.Any());
            Debug.Assert(Enum.IsDefined(typeof(InitialCursorPosition), position));

            _currentCell = (position == InitialCursorPosition.Head ? cells.First : cells.Last);
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

