import _ from 'lodash';
import chroma from 'chroma-js';
import d3 from 'd3';
import rgbHex from 'rgb-hex';

import { computeTicks, getColours } from '../utils/utils'

function positiveScale(colours, min, max, steps) {
    var startColour = colours['background-rear-positive-light'],
        endColour = colours['background-rear-positive'];

    var scale = chroma.scale([rgbHex(startColour), rgbHex(endColour)]);
    var range = [scale(0)];

    for (var i = 0; i < steps; i++) {
        range.push(scale(i/steps));
    }

    range.push(scale(steps / steps));

    return d3.scale.ordinal()
                    .domain([min, max])
                    .range(range);
}

function negativeScale(colours, min, max, steps) {
    var startColour = colours['background-rear-negative-light'],
        endColour = colours['background-rear-negative'];

    var scale = chroma.scale([rgbHex(startColour), rgbHex(endColour)]);
    var range = [scale(0)];

    for (var i = 0; i < steps; i++) {
        range.push(scale(i/steps));
    }

    range.push(scale(steps / steps));

    return d3.scale.ordinal()
                    .domain([min, max])
                    .range(range);
}

/*
 * Generates Map legend based on supplied width, height & scale data
 * */
export default function(width, height, minimum, maximum, steps = 100) {

    // When we have comparision data, let's switch the scales.
    // if (none(this.get('bailiwick.compareArea'))) {
    //     this.defaultScale();
    //   } else {
    //     this.compareScale();
    //   }
    var base = d3.select(".indicator-map-legend"),
        svg = base.select('svg').empty() ? base.append('svg') : base.select('svg'),
        colours = getColours(),
        positive = positiveScale(colours, minimum, maximum, steps),
        negative = negativeScale(colours, minimum, maximum, steps),
        zero = chroma.scale([colours['background-rear-negative-light']]);

    //
    // Default Scale.
    //
    var scaleType = 'sequential';
    var vals = d3.values([minimum, maximum]);
    var extent = d3.extent(vals);

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
        (scaleType !== "sequential" && (extent[0] * extent[1]) < 0)
    ) {
        var max = Math.max(Math.abs(extent[0]), Math.abs(extent[1]));

        scale = d3.scale.linear().domain([-1 * max, 0, max]).range([-1, 0, 1]);
        thresholdBase = computeTicks(extent);

        scaleF = function(v) {
            var s = scale(v);
            if (s < 0) {
                return negative(s);
            } else if (s > 0) {
                return positive(s);
            }
            return zero;
        };

        threshold = d3.scale.threshold()
            .domain(thresholdBase)
            .range(thresholdBase.map(function(t) {
                return scaleF(t);
            }));
        linear = d3.scale.linear()
            .domain(thresholdBase)
            .range(thresholdBase.map(function(t) {
                return scaleF(t);
            }));
    }

    //
    // End Default scale
    //
    var domain = linear.domain();
    extent = d3.extent(domain);
    var step = (extent[1] - extent[0]) / steps;
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
    svg.data([1])
        .attr("width", width)
        .attr("height", height);
    step = (width - 100) / steps;

    var sd = Array.from(Array(steps).keys(), i => [i * step]);

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
        .style("fill", function (d, i) {
            return positive(d[0]);
        })
        .style("stroke", function (d) { return positive(d[0]); });

    g.selectAll(".caption").remove();

    let xa = g.call(xAxis);
    xa.append("text")
        .attr("class", "caption")
        .attr("y", -6)
        .text("caption goes here");

    return positive;
}
