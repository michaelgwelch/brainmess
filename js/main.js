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
        run: function(programText, inputNode, outputNode) {
            var p = new Program(programText);
            var outputText = document.createTextNode("");
            if (outputNode.firstChild) {
                outputNode.removeChild(outputNode.firstChild);
            }
            outputNode.appendChild(outputText);
            var output = function(char)  {
                outputText.appendData(String.fromCharCode(char));
            };
            context = new Context(p, inputNode, output);
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
        document.getElementById("output"));
}
