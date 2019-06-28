import _ from 'lodash';
import chroma from 'chroma-js';
import d3 from 'd3';
import rgbHex from 'rgb-hex';

import { computeTicks, getColours } from '../utils/utils'

function positiveScale(colours, min, max) {
    if (_.isEmpty(colours)) {
        return null;
    }
    
    var startColour = colours['background-rear-positive-light'],
        endColour = colours['background-rear-positive'];

    var scale = chroma.scale([rgbHex(startColour), rgbHex(endColour)]);

    var sf = scale.domain([min, max]);
    return (function (val) {
        return sf(Number(val));
    });
}


/*
 * Generates Map legend based on supplied width, height & scale data
 * */
export default function(args, steps = 100) {//width, height, minimum, maximum, steps = 100) {

    // console.log('legend', args)
    let height = Number(args.height),
        width = Number(args.width),
        maximum = Number(args.max),
        minimum = Number(args.min),
        caption = args.label;

    var base = d3.select(".indicator-map-legend"),
        svg = base.select('svg').empty() ? base.append('svg') : base.select('svg'),
        colours = getColours(),
        positive = positiveScale(colours, minimum, maximum);

    var vals = d3.values([minimum, maximum]);
    var extent = d3.extent(vals);

    var widthRange = window.innerWidth > 460 ? 380 : window.innerWidth - 120;

    // A position encoding for the key only.
    var x = d3.scale.linear()
        .domain(extent)
        .range([0, widthRange]);

    var thresholdBase = x.ticks(7)
    var xaxisscale = d3.scale.linear()
        .domain([_.first(thresholdBase), _.last(thresholdBase)])
        .range([0,widthRange]);
    var xAxis = d3.svg.axis()
        .scale(xaxisscale)
        .ticks(7)
        .orient("bottom")
        .tickSize(13)
        .tickFormat(function(d) {
          return d;  // TODO Add the format function
        });

    svg.empty();
    svg.data([1])
        .attr("width", width)
        .attr("height", height);
    var step = (width - 100) / steps;
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
            return positive(x.invert(d[0]));
        })
        .style("stroke", function (d) { return positive(x.invert(d[0])); });

    g.selectAll(".caption").remove();

    let xa = g.call(xAxis);
    xa.append("text")
        .attr("class", "caption")
        .attr("y", -6)
        .text(caption);

    return positive;
}
