
function BrainmessTapeCell(val, current) {

    this.value = ko.observable(val);
    this.isCurrent = ko.observable(current);

};

function BrainmessViewModel() {

    self = this;

    var currentIndex = function () {
        var i;
        var cells = self.tapeCells();
        for (i = 0; i < cells.length; i++) {
            if (cells[i].isCurrent()) return i;
        }
    };
    // Operations - only called on MoveForward, so creates a 0 cell that is current
    self.addCell = function () {
        self.tapeCells()[self.tapeCells().length - 1].isCurrent(false);
        self.tapeCells.push(new BrainmessTapeCell(0, true));
    }
    self.forward = function () {
        var index = currentIndex();
        self.tapeCells()[index].isCurrent(false);
        self.tapeCells()[index + 1].isCurrent(true);
    };
    self.backward = function () {
        var index = currentIndex();
        self.tapeCells()[index].isCurrent(false);
        self.tapeCells()[index - 1].isCurrent(true);
    };
    self.inc = function () {
        var index = currentIndex();
        var value = self.tapeCells()[index].value();
        self.tapeCells()[index].value(parseInt(value) + 1);
    };
    self.dec = function () {
        var index = currentIndex();
        var value = self.tapeCells()[index].value();
        self.tapeCells()[index].value(parseInt(value) - 1);
    };
    self.output = ko.observable("");
    self.tapeCells = ko.observableArray([
            new BrainmessTapeCell("32"),
            new BrainmessTapeCell("45"),
            new BrainmessTapeCell("1"),
            new BrainmessTapeCell("23", true)
        ]);

};



ko.applyBindings(new BrainmessViewModel());