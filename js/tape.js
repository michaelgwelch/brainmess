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
        backward: function () {
            // for now block going to left of 0 so that presentation is easier
            // Tape will always have a beginning.
            if (index == 0) throw new Error("Invalid state of tape. Can't move backward. Already at beginning");
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
        index: function () { return index; }
    }
};

// A function that can inspect a tape string
// and return an object that contains a string representation
// of the current cell, the cells prior to the current cell (prefix),
// and the cells after the current cell (suffix)
var TapeSlitter = function () {
    return {
        split: function (tapeString) {
            tapeString += ",";

            var indexOfCurrent = tapeString.indexOf("->");
            var prefixString = tapeString.substring(0, indexOfCurrent - 1);

            var endOfCurrent = tapeString.indexOf(",", indexOfCurrent);
            var currentString = tapeString.substring(indexOfCurrent + 2, endOfCurrent);

            var endString = tapeString.substring(endOfCurrent + 1, tapeString.length - 1);


            return {
                prefix: prefixString,
                current: currentString,
                suffix: endString,
            };
        }
    }
}



