using System;
using System.Collections.Generic;

namespace Welch.Brainmess
{
    public static class LinkedListNode
    {
        public static LinkedListNode<T> MoveForward<T>(this LinkedListNode<T> node)
        {
            if (node.List == null) throw new ArgumentException("The specified node is not linked into a list", "node");
            return node.Next ?? node.List.AddLast(default(T));
        }

        public static LinkedListNode<T> MoveBackward<T>(this LinkedListNode<T> node)
        {
            if (node.List == null) throw new ArgumentException("The specified node is not linked into a list", "node");
            return node.Previous ?? node.List.AddFirst(default(T));
            
        }
    }
}

