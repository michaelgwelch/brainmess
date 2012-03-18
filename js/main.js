function main() {
    var programText = document.getElementById("prog").value;
    var p = new Program(programText);
    var input = undefined;
    var preTag = document.getElementById("output");
    var outputText = document.createTextNode("");
    preTag.appendChild(outputText);
    var output = function(char)  {
       outputText.appendData(String.fromCharCode(char));
    }


    var context = new Context(p, input, output);

    while(!p.endOfProgram()) {
        var i = p.fetch();
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
            case ",":
                context.input();
                break;
            case "[":
                context.testAndJumpForward();
                break;
            case "]":
                context.testAndJumpBackward();
                break;
	

        }
    }

}
