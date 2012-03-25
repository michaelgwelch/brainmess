"use strict";
var Context = function (prog, inputCallback, outputCallback) {
    var tape = new Tape();
    var outputString = "";
    var expectingInput = false;
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
            // If we have a charCode then we are actually
            // getting input, otherwise we are just
            // preparing for input - we call the inputCallback
            // to inform the program host that we expect input.
            if (charCode) {
                if (expectingInput) {
                    tape.set(charCode);
                    expectingInput = false;
                } else {
                    // this is a bug int he program if this happens. user shouldn't see this
                    throw new Error("Invalid operation. Input was passed to context when context wasn't expecting it.");
                }
            } else {
                expectingInput = true;
                inputCallback();
            }
        },
        output: function () {
            // deal with appending strings later.
            outputString += String.fromCharCode(tape.get());
            if (outputCallback) outputCallback(outputString);
        },
        memory: function () {
            var cells = [];
            var it = tape.iterator();
            for (var pair in it) {
                cells.push(pair[1]);
            }
            return cells;
        }
    };
};

