"use strict";

function BrainmessTapeCell(val, current) {

    var self = this;

    self.value = ko.observable(val);
    self.isCurrent = ko.observable(current);

};

function BrainmessViewModel() {

    var self = this;
    var brainmess = new Brainmess();

    // properties
    self.singleStep = ko.observable(false);
    self.output = ko.observable("");
    self.tapeCells = ko.observableArray([]);
    self.program = ko.observable(helloWorld);

    var runContext = undefined;

    // Private methods
    var prepareForInput = function() {
        self.inputEnabled(true);
    };

    var displayOutput = function (newText) {
        self.output(newText);
    };



    // Operations - only called on MoveForward, so creates a 0 cell that is current
    
    self.chooseHello = function() {
        self.program(helloWorld);
    };
    
    self.chooseFibo = function() {
        self.program(fibo);
    }
    self.run = function () {
        runContext = brainmess.run($("#program").val(),
            prepareForInput,
            displayOutput,
            self.singleStep());

    };
    
    self.sendInput = function (data, event) {
        runContext.resume(event.keyCode, self.singleStep());
    };

    self.resume = function () {
        runContext.resume(self.singleStep());
    }

    self.moveTo = function (from, to) {
        self.tapeCells()[from].isCurrent(false);
        self.tapeCells()[to].isCurrent(true);
    };
    
    self.addCell = function (index, value) {
        var cells = self.tapeCells();

        // we expect index to always be at the end of list
        if (index !== cells.length) throw new Error("unexpected action from model");
        // we expect value to be 0 but we can handle anything

        cells.push(new BrainmessTapeCell(value));
    };
    
    // The cell specified by index has had its value changed
    // The index isn't really needed as we can loop thru and find the
    // current cell.
    self.modifyCell = function (index, value) {
        self.tapeCells()[index].value(value);
    };

};



ko.applyBindings(new BrainmessViewModel());