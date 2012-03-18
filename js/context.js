var Context = function(prog, inputNode, output) {
    var tape = new Tape();
    return {
        enableInput: function() { 
            inputNode.disabled=false; 
            inputNode.value="";
        },
        disableInput: function() {
            inputNode.disabled = true;
        },
        endOfProgram: function() { return prog.endOfProgram(); },
        fetch: function() { return prog.fetch(); },
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
        input: function(charCode) { 
            tape.set(charCode);
        },
        output: function() {
            output(tape.get());
        }
    };
}

