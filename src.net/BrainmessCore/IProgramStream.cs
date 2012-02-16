namespace Welch.Brainmess
{
    public interface IProgramStream
    {
        /// <summary>
        /// Reads the Instruction at the program counter and returns it.
        /// </summary>
        Instruction Fetch();

        /// <summary>
        /// Gets a value indicating whether this instance is at the end of the program.
        /// If Fetch is called when EndOfProgram is true an exception will be thrown.
        /// </summary>
        /// <value>
        /// <c>true</c> if end of program; otherwise, <c>false</c>.
        /// </value>
        bool EndOfProgram { get; }

        /// <summary>
        /// This method causes the program counter to move from current location to right after
        /// a matching ']' instruction. It only makes sense to be called if the interpreter is 
        /// executing a TestAndJumpForward instruction. If that is not the case, the results
        /// are not predictable.
        /// </summary>
        void JumpForward();

        /// <summary>
        /// This method causes the program counter to move from current location to 
        /// a matching '[' instruction. It only makes sense to be called if the interpreter is 
        /// executing a TestAndJumpBackward instruction. If that is not the case, the results
        /// are not predictable.
        /// </summary>
        void JumpBackward();
    }
}