import _ from 'lodash';
import chroma from 'chroma-js';
import d3 from 'd3';
import rgbHex from 'rgb-hex';

import { format, getFormatter } from '../utils/formatting'
import { computeTicks, getColours } from '../utils/utils'

let positiveScale = function(min, max) {
    let colours = getColours();

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
let updateMapLegend = function(element, args, chart, steps = 100) {
    if (_.isEmpty(chart)) {
        return;
    }

    let maximum = Number(args.max),
        minimum = Number(args.min);

    if (minimum === 0 && maximum === 0) {
        return; // Data not loaded
    }

    let positive = positiveScale(minimum, maximum),
        base = d3.select(element),
        height = Number(args.height),
        width = Number(args.width),
        caption = args.label,
        transform = args.transform;

    if (!_.isEmpty(caption)) {
        caption = caption[0].toUpperCase() + caption.slice(1);
    }

    var svg = base.select('svg').empty() ? base.append('svg') : base.select('svg');
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

    // Let d3 choose precision of formatted ticks first
    var d3_xticks = xaxisscale.ticks(7).map(xaxisscale.tickFormat(7, "s"));
    var formatter = getFormatter(chart, transform);

    var xAxis = d3.svg.axis()
        .scale(xaxisscale)
        .ticks(7)
        .orient("bottom")
        .tickSize(13)
        .tickFormat(function(d, i) {
          if (formatter) {
            return format(formatter, d);
          }
          return format("reformat", d3_xticks[i]);
        });

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

}

export { positiveScale, updateMapLegend }
