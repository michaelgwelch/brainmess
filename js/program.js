var Program = function(programString) {
    var pc = 0;

    return {
        endOfProgram: function() {
            return pc >= programString.length;
        },
        fetch: function() {
            return programString[pc++]
        },
        jumpForward: function() {
            pc = programString.findMatch(pc - 1) + 1;
        },
        jumpBackward: function() {
            pc = programString.findMatch(pc - 1);
        }
    }
};

String.prototype.findMatch = function (index) {
    var increment;
    if (this[index] === "[") increment = 1;
    else if (this[index] === "]") increment = -1;
    index += increment;
    var nestLevel = 1;
    while (nestLevel > 0) {
        var instruction = this[index];
        if (instruction === "[") nestLevel += increment;
        else if (instruction === "]") nestLevel -= increment;
        index += increment;
    }
    return index - increment;
};

