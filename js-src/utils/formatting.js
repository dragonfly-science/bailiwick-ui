import d3 from 'd3';

var commasFormatter = function(value) {
    var f = "" + value;

    if (f.length > 4) {
        var val = d3.format('.1r')(value);

        if (val.charAt(1) === '0') {
            return d3.format('.1s')(val);
        }
        return d3.format('.2s')(val);
    }

    return d3.format('n')(value);
};


var moneyFormatter = d3.format(',g');
var percentageFormatter = function(value) {
    var int = parseInt(value);

    if (int < 10) {
        return d3.format('.1r')(value);
    }

    return d3.format('.2r')(value);
}

export default function formatting(units, value) {
  switch (units) {
    case "million dollars":
        return moneyFormatter(value) + " M";
    case "dollars":
        return moneyFormatter(value);
    case "percentage":
        return percentageFormatter(value) + " %";
  }
    return commasFormatter(value);
}
