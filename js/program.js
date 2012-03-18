var Program = function(prog) {
    var pc = 0;

    return {
        endOfProgram: function() {
            return pc >= prog.length;
        },
        fetch: function() {
            return prog[pc++]
        },
        jumpForward: function() {
            pc = prog.findMatch(pc-1) + 1;
        },
        jumpBackward: function() {
            pc = prog.findMatch(pc-1);
        }
    }
};

String.prototype.findMatch = function(index) {
    var increment;
    if (this[index] === "[") increment = 1;
    else if (this[index]  === "]") increment = -1;
    index += increment;
    var nestLevel = 1;
    while(nestLevel > 0) {
        var instruction = this[index];
        if (instruction === "[") nestLevel += increment;
        else if (instruction === "]") nestLevel -= increment;
        index += increment;
    }
    return index - increment;
}

