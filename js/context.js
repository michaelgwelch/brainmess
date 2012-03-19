var Context = function (prog, outputCallback) {
    var tape = new Tape();
    var outputString = "";
    return {
        endOfProgram: function () { return prog.endOfProgram(); },
        fetch: function () { return prog.fetch(); },
        forward: function () { tape.forward(); },
        backward: function () { tape.backward(); },
        inc: function () { tape.inc(); },
        dec: function () { tape.dec(); },
        testAndJumpForward: function () {
            if (tape.get() === 0) prog.jumpForward();
        },
        testAndJumpBackward: function () {
            if (tape.get() !== 0) prog.jumpBackward();
        },
        input: function (charCode) {
            tape.set(charCode);
        },
        output: function () {
            // deal with appending strings later.
            outputString += String.fromCharCode(tape.get());
            if (outputCallback) outputCallback(outputString);
        }
    };
};

