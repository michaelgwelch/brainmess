var Brainmess = function() {
    var context = undefined;

    var execute = function() {
        while(!context.endOfProgram()) {
            var i = context.fetch();
            if (i === ",") {
                context.enableInput();
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
        

            }
        }
    };

    return {
        // creates a new context based on paramters
        // and starts execution of the program
        run: function(programText, inputNode, outputCallback) {
            var p = new Program(programText);
            context = new Context(p, inputNode, outputCallback);
            execute();
        },
        resume: function(event) {
            context.disableInput();
            context.input(event.charCode);
            execute();
        },
    };

};
var brainmess = new Brainmess();
function main() {
    brainmess.run(document.getElementById("prog").value, 
        document.getElementById("input"),
        function(newText) {
            myViewModel.programOutput(newText);
        });
}
