using System;

namespace BrainmessShort
{
    public class Tape
    {
        private readonly int[] tape = new int[5000];
        private int tc = 2500;
        
        public void MoveForward()
        {
            tc++;
        }
        
        public void MoveBackward()
        {
            tc--;
        }
        
        public void Increment()
        {
            tape[tc]++;
        }
        
        public void Decrement()
        {
            tape[tc]--;
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

