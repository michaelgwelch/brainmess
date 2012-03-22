
function BrainmessTapeCell(val, current) {

    this.value = ko.observable(val);
    this.isCurrent = ko.observable(current);

};

function BrainmessViewModel() {

    self = this;
    var brainmess = new Brainmess();

    var currentIndex = function () {
        var i;
        var cells = self.tapeCells();
        for (i = 0; i < cells.length; i++) {
            if (cells[i].isCurrent()) return i;
        }
    };
    
    self.output = ko.observable("");
    self.tapeCells = ko.observableArray([
                                         new BrainmessTapeCell("0", true)
                                         ]);

    // Operations - only called on MoveForward, so creates a 0 cell that is current

    // forward, backward, inc, dec should be delted at some point
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
    

    self.moveTo = function (from, to) {
        self.tapeCells()[from].isCurrent(false);
        self.tapeCells()[to].isCurrent(true);
    }
    
    self.addCell = function(index, value) {
        var cells = self.tapeCells();
        
        // we expect index to always be at the end of list
        if (index !== cells.length) throw new Error("unexpected action from model");
        // we expect value to be 0 but we can handle anything
        
        cells.push(new BrainmessTapeCell(value));
    }
    
    // The cell specified by index has had its value changed
    // The index isn't really needed as we can loop thru and find the
    // current cell.
    self.modifyCell = function(index, value){
        self.tapeCells()[index].value(value);
    }

};



ko.applyBindings(new BrainmessViewModel());