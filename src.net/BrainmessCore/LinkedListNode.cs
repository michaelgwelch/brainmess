using System;
using System.Collections.Generic;

namespace Welch.Brainmess
{
    public static class LinkedListNode
    {
        public static LinkedListNode<T> MoveForward<T>(this LinkedListNode<T> node)
        {
            if (node.List == null) throw new ArgumentException("node", "The specified node is not linked into a list");
            
            LinkedListNode<T> next = node.Next;
            if (next == null)
            {
                next = node.List.AddLast(default(T));
            }
            return next;
        }
        
        public static LinkedListNode<T> MoveBackward<T>(this LinkedListNode<T> node)
        {
            if (node.List == null) throw new ArgumentException("node", "The specified node is not linked into a list");

            LinkedListNode<T> previous = node.Previous;
            if (previous == null)
            {
                previous = node.List.AddFirst(default(T));
            }
            return previous;
        }
    }
}

