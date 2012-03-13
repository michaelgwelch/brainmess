var Tape = function() {
    var cells = [0];
    var index = 0;

    return {
        inc: function() { cells[index]++; },
        dec: function() { cells[index]--; },
        forward: function() { 
            index++;
            if (index === cells.length) cells.push(0);
        },
        backward: function() { 
            if (index > 0) index--;
            else cells.unshift(0);
        },
        get: function() { return cells[index]; },
        set: function(v) { cells[index] = v; },
        toString: function() { 
            var result = "";
            var i = 0;
            for(i = 0; i < cells.length; i++) {
                if (i !== 0) result += ",";
                if (i === index) result += "->";
                result += cells[i].toString();
            }
            return result;
        },
        index: function() { return index; }
    }
};
