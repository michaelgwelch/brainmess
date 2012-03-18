var Context = function(prog, input, output) {
    var tape = new Tape();
    return {
        forward: function() { tape.forward(); },
        backward: function() { tape.backward(); },
        inc: function() { tape.inc(); },
        dec: function() { tape.dec(); },
        testAndJumpForward: function() {
            if (tape.get() === 0) prog.jumpForward();
        },
        testAndJumpBackward: function() {
            if (tape.get() !== 0) prog.jumpBackward();
        },
        input: function() { 
            tape.set(input());
        },
        output: function() {
            output(tape.get());
        }
    };
}

