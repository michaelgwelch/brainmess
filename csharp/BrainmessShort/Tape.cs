using System;

namespace BrainmessShort
{
    public class Tape
    {
        private readonly int[] tape = new int[5000];
        private int tc = 2500;
        
        public int MoveForward()
        {
            return tc++;
        }
        
        public int MoveBackward()
        {
            return tc--;
        }
        
        public int Increment()
        {
            return tape[tc]++;
        }
        
        public int Decrement()
        {
            return tape[tc]--;
        }
        
        public int Current
        {
            get
            {
                return tape[tc];
            }
            set
            {
                tape[tc] = value;
            }
        }
    }
}

