using System;

namespace Bmc
{
    public interface IGenerator
    {
        void FinalizeProgram();
        void MoveTape(int x);
        void AddValue(int x);
        void WriteCurrent();
        void ReadAndStoreInput();
        void BeginLoop();
        void EndLoop();
    }
}

