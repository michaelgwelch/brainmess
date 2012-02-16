using System;
using System.Collections.Generic;

namespace Welch.Brainmess
{
    /// <summary>
    /// Extension methods for LinkedListNode class
    /// </summary>
    public static class LinkedListNode
    {
        /// <summary>
        /// Returns the next node after this one. If this is the last node in the list, then
        /// a new node is added after this one (with the default value of T) and that node is returned.
        /// If <paramref name="node"/> is not in a list then an exception is thrown.
        /// </summary>
        public static LinkedListNode<T> MoveForward<T>(this LinkedListNode<T> node)
        {
            if (node.List == null) throw new ArgumentException("The specified node is not linked into a list", "node");
            return node.Next ?? node.List.AddLast(default(T));
        }

        /// <summary>
        /// Returns the node that is before this one. If this node is the first one in the list,
        /// then a new node is added to the front of the list (with the default value of T) and that node
        /// is returned. If <paramref name="node"/> is not part of a list then an exception is thrown.
        /// </summary>
        public static LinkedListNode<T> MoveBackward<T>(this LinkedListNode<T> node)
        {
            if (node.List == null) throw new ArgumentException("The specified node is not linked into a list", "node");
            return node.Previous ?? node.List.AddFirst(default(T));
            
        }
    }
}

