var Context = function() {
    return {
        forward: function(prog, tape, input, output) { tape.forward(); },
        backward: function(prog, tape, input, output) { tape.backward(); },
        inc: function(prog, tape, input, output) { tape.inc(); },
        dec: function(prog, tape, input, output) { tape.dec(); },
        testAndJumpForward: function(prog, tape, input, output) {
            if (tape.get() === 0) prog.jumpForward();
        },
        testAndJumpBackward: function(prog, tape, input, output) {
            if (tape.get() !== 0) prog.jumpBackward();
        },
        input: function(prog, tape, input, output) { 
            tape.set(input());
        },
        output: function(prog, tape, input, output) {
            output(tape.get());
        }
    };
}

