// There are many different "events" on this object
// move -
// modify
// newCell




var Brainmess = function() {
    var context = undefined;

    var execute = function(singleStep) {
        while(!context.endOfProgram()) {
            var noop = false;
            var i = context.fetch();
            if (i === ",") {
                context.input();
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
                    noop = true;
                    break;
        

            }
            if (!noop && singleStep) {
                break; // out of while loop - we are singleStepping, but not thru NoOps.
            }
        }
    };

    return {

        run: function (programText, inputCallback, outputCallback, singleStep) {
            var p = new Program(programText);
            context = new Context(p, inputCallback, outputCallback);
            inputEvent = inputCallback;
            execute(singleStep);
            return {
                resume: function (singleStep, charCode) {
                    if (charCode) {
                        context.input(charCode);
                    }
                    execute(singleStep);
                },
                memoryView: function () {
                    return context.memory();
                }
            };
        },

    };

};

