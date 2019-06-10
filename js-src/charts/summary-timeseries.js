import d3 from 'd3';

export default function(element, labelledData, activeLabelName) {
    let base = d3.select(element).select('.d3-attach'),
        svg = base.select('svg').empty() ? base.append('svg') : base.select('svg'),
        yearFormat = d3.time.format('%Y').parse,
        margin = {
            top: 15,
            right: 0,
            bottom: 30,
            left: 48
        },
        baseW = 225,
        baseH = 120,
        width = baseW - margin.left - margin.right,
        height = baseH - margin.top - margin.bottom;

    svg.attr('width', baseW);
    svg.attr('height', baseH);

    var data = labelledData.map(function (d) {
        return d[1];
    });

    var x = d3.time.scale()
        .range([0, width]);

    var y = d3.scale.linear()
        .range([height, 0]);

    var xAxis = d3.svg.axis()
        .scale(x)
        .ticks(5)
        .innerTickSize(3)
        .outerTickSize(0)
        .tickFormat(function (t) {
            return t.getFullYear().toString().replace(/^20/, "'");
        })
        .orient("bottom");

    var yAxis = d3.svg.axis()
        .scale(y)
        .ticks([4])
        .outerTickSize(0)
        .innerTickSize(-width)
        .tickFormat(function (t) {
            return '$' + t / 1000 + ' k';
        })
        .orient("left");

    var line = d3.svg.line()
        .x(function (d) {
            return x(yearFormat(d[0].toString()));
        })
        .y(function (d) {
            return y(d[1]);
        });

    var g = svg.selectAll('g').data([data]);
    var gEnter = g.enter()
        .append("g")
        .attr("transform", "translate(" + margin.left + "," + margin.top + ")");

    var yMax = Math.ceil(d3.max(d3.merge(data), function (d) {
        return d[1];
    }) / 100000) * 100000;
    x.domain(d3.extent(data[0], function (d) {
        return yearFormat(d[0].toString());
    }));
    y.domain([0, yMax]);

    gEnter.append("g")
        .attr("class", "x axis")
        .attr("transform", "translate(0," + height + ")")
        .append("text")
        .attr("x", 50)
        .attr("dy", "2.5em")
        .text("Year");

    g.selectAll("g.x")
        .call(xAxis);

    gEnter.append("g")
        .attr("class", "y axis")
        .attr("transform", "translate(-7,0)");

    g.selectAll("g.y")
        .call(yAxis);

    var linePlot = g.selectAll("path.line").data(labelledData);
    linePlot.enter()
        .append("path");

    linePlot.attr("d", function (d) {
        return line(d[1]);
    }).attr("class", function (d, i) {
        if (activeLabelName === d[0]) {
            return "line active";
        }
        return "line";
    });

    linePlot.exit().remove();
}