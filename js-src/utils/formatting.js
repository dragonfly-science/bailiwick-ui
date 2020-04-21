import d3 from 'd3';

let getFormatter = function(chart, transform) {
    if (chart === null || chart.length === 0) {
        return "none";
    }

    var trans = _.filter(chart.chartTransforms, function(i) {
        return i.transformName === transform;
    });

    var formatter = null;

    if (trans.length > 0) {
        formatter = trans[0].transformFormatter;
    }

    return formatter;
};

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

let format = function (formatter, value) {
  switch (formatter) {
    case "million dollars":
        return moneyFormatter(value) + " M";
    case "dollars":
        return moneyFormatter(value);
    case "percentage":
        return percentageFormatter(value) + " %";
    case "none":
        return value;
    case "reformat":
        return value.replace(/\.0+([kM])$/, "$1")
          .replace(/^0[kM]$/, "0");
  }
    return commasFormatter(value);
}

export { format, getFormatter }
