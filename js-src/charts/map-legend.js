import d3 from 'd3';
import { computeTicks, getColours } from '../utils/utils'

export default function(width, height, scaledata) {
    // When we have comparision data, let's switch the scales.
    // if (none(this.get('bailiwick.compareArea'))) {
    //     this.defaultScale();
    //   } else {
    //     this.compareScale();
    //   }
    var base = d3.select(".indicator-map-legend");
    var svg = base.select('svg').empty() ? base.append('svg') : base.select('svg');

    // var colors = getColours();
    // var positive = d3.scale([colors['background-rear-positive-light'], colors['background-rear-positive']]),
    //     negative = d3.scale()
    //             .domain([-1, 0])
    //             .range([colors['background-rear-negative'], colors['background-rear-negative-light']]),
    //     zero = d3.scale([colors['background-rear-positive-light'], colors['background-rear-negative-light']]);

    // console.log(positive, negative, zero);

    //
    // Default Scale.
    //
    var scaleType = 'diverging';
    var vals = d3.values(scaledata);
    var extent = d3.extent(vals, function (v) {
        return v[0];
    });
    // var caption = this.getAttr("caption").get("text");
    // var formatter = this.getAttr("caption").get("formatter");
    var scale = d3.scale.linear().domain(extent).range([0, 1]),
        scaleF = function (v) { return scale(v); },
        thresholdBase = scale.ticks(7),
        threshold = d3.scale.threshold()
            .domain(thresholdBase)
            .range(thresholdBase.map(function (t) {
                return scaleF(t);
            })),
        linear = d3.scale.linear()
            .domain(thresholdBase)
            .range(thresholdBase.map(function (t) {
                return scaleF(t);
            }));

    if (
        scaleType === "diverging" ||
        (scaleType !== "sequential" && extent[0] * extent[1] < 0)
    ) {
        var max = Math.max(Math.abs(extent[0]), Math.abs(extent[1]));

        scale = d3.scale.linear().domain([-1 * max, 0, max]).range([-1, 0, 1]),
            thresholdBase = computeTicks(extent);
        // console.log('chroma', chroma)

        // scaleF = function(v) {
        //     var s = scale(v);
        //     if (s < 0) {
        //     return negative(s);
        //     } else if (s > 0) {
        //     return positive(s);
        //     }
        //     return zero;
        // };

        // threshold = d3.scale.threshold()
        //     .domain(thresholdBase)
        //     .range(thresholdBase.map(function(t) {
        //         return scaleF(t);
        //     }));
        // linear = d3.scale.linear()
        //     .domain(thresholdBase)
        //     .range(thresholdBase.map(function(t) {
        //         return scaleF(t);
        //     }));
    }

    //
    // End Default scale
    //
    var domain = linear.domain();
    extent = d3.extent(domain);
    var step = (extent[1] - extent[0]) / 100;
    var widthRange = window.innerWidth > 460 ? 380 : window.innerWidth - 120;

    // A position encoding for the key only.
    var x = d3.scale.linear()
        .domain(extent)
        .range([0, widthRange]);

    var xAxis = d3.svg.axis()
        .scale(x)
        .orient("bottom")
        .tickSize(13)
        .tickValues(window.innerWidth < 500 ? extent : domain);
    //   .tickFormat(function(d) {
    //     return d;
    //   });

    svg.empty();
    svg.data([scaledata])
        .attr("width", width)
        .attr("height", height);
    step = (width - 100) / 100;

    var sd = Array.from(Array(100).keys(), i => [i * step, scaledata[i][2]]);

    svg.selectAll('g').remove();

    var g = svg.selectAll("g")
        .data([sd])
        .enter()
        .append("g")
        .attr("class", "key")
        .attr("transform", "translate(50," + height * 1 / 3 + ")");

    var legend = g.selectAll("rect")
        .data(sd)
        .enter()
        .append("rect")
        .attr("height", 8)
        .attr("x", function (d) { return d[0] % innerWidth; })
        .attr("width", step)
        .style("fill", function (d) { return d[1]; })
        .style("stroke", function (d) { return d[1]; });

    g.selectAll(".caption").remove();
    let xa = g.call(xAxis);
    xa.append("text")
        .attr("class", "caption")
        .attr("y", -6)
        .text("caption goes here");
}