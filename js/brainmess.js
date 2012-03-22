// There are many different "events" on this object
// move -
// modify
// newCell




var Brainmess = function() {
    var context = undefined;
    var inputEvent = undefined;
    var instructionExecutedCallback = undefined;
    var execute = function(singleStep) {
        while(!context.endOfProgram()) {
            var sendCallback = true;
            var i = context.fetch();
            if (i === ",") {
                inputEvent();
                break;// break out of while
            }
            switch(i) {
                case ">": 
                    context.forward();
                    break;
                case "<":
                    context.backward();
                    break;
                case "+":
                    context.inc();
                    break;
                case "-":
                    context.dec();
                    break;
                case ".":
                    context.output();
                    break;
                case "[":
                    context.testAndJumpForward();
                    break;
                case "]":
                    context.testAndJumpBackward();
                    break;
                default:
                    sendCallback = false;
                    break;
        

            }
            if (sendCallback) {
                instructionExecutedCallback(context.memory());
                if (singleStep) break;
            }
        }
    };

    return {
        // creates a new context based on paramters
        // and starts execution of the program
        run: function(programText, inputCallback, outputCallback, singleStep) {
            var p = new Program(programText);
            context = new Context(p, outputCallback);
            inputEvent = inputCallback;
            execute(singleStep);
        },
        resume: function(charCode, singleStep) {
            context.input(charCode);
            execute(singleStep);
        },
        nextStep: function(singleStep) {
            execute(singleStep)
        },
        
        instructionExecuted: function(callback) {
            instructionExecutedCallback = callback;
        },
        
        memory: function() {
            return context.memory();
        }
    };

};

