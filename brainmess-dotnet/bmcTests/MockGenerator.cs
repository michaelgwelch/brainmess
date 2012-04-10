using System;
using Bmc;

namespace bmcTests
{
    public class MockGenerator : IGenerator
    {
        public int TapeAdvances { get; private set; }
        public int TapeBacks { get; private set; }
        public int ValueIncrements { get; private set; }
        public int ValueDecrements { get; private set; }
        public int Writes { get; private set; }
        public int Reads { get; private set; }
        public int LoopBegins { get; private set; }
        public int LoopEnds { get; private set; }
        public int ProgramFinalizes { get; private set; }

        public void FinalizeProgram ()
        {
            ProgramFinalizes++;
        }

        public void MoveTape (int x)
        {
            if(x > 0)
            {
                TapeAdvances+=x;
            }
            if(x < 0)
            {
                TapeBacks+=x;
            }
        }

        public void AddValue (int x)
        {
            if(x > 0)
            {
                ValueIncrements+=x;
            }
            if(x < 0)
            {
                ValueDecrements+=x;
            }
        }

        public void WriteCurrent ()
        {
            Writes++;
        }

        public void ReadAndStoreInput ()
        {
            Reads++;
        }

        public void BeginLoop ()
        {
            LoopBegins++;
        }

        public void EndLoop ()
        {
            LoopEnds++;
        }

        public void FinalizeProgram()
        {
            ProgramFinalizes++;
        }
    }
}

