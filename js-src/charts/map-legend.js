import _ from 'lodash';
import chroma from 'chroma-js';
import d3 from 'd3';
import rgbHex from 'rgb-hex';

import { computeTicks, getColours } from '../utils/utils'

/*
 * Generates Map legend based on supplied width, height & scale data
 * @todo - replace scaledata with min / max values
 * */
export default function(width, height, scaledata, steps = 100) {

    // console.log('scale', scaledata);
    let minimum = _.reduce(scaledata, function(m, v) {
        return Math.min(m, v[0]);
    }, Infinity);

    let maximum = _.reduce(scaledata, function(m, v) {
        return Math.max(m, v[0]);
    }, 0);

    // console.log(minimum, maximum);

    // When we have comparision data, let's switch the scales.
    // if (none(this.get('bailiwick.compareArea'))) {
    //     this.defaultScale();
    //   } else {
    //     this.compareScale();
    //   }
    var base = d3.select(".indicator-map-legend");
    var svg = base.select('svg').empty() ? base.append('svg') : base.select('svg');

    var colours = getColours();
    var startColour = colours['background-rear-positive-light'],
        endColour = colours['background-rear-positive'],
        negativeStartColour = colours['background-rear-negative-light'],
        negativeEndColour = colours['background-rear-negative'];

    var positiveScale = chroma.scale([rgbHex(startColour), rgbHex(endColour)]);
    var positiveScaleRange = [positiveScale(0)];
    for (var i = 0; i < steps; i++) {
        positiveScaleRange.push(positiveScale(i/steps));
    }
    positiveScaleRange.push(positiveScale(steps / steps));
    var positive = d3.scale.ordinal()
                    .domain([minimum, maximum])
                    .range(positiveScaleRange);

    var negativeScale = chroma.scale([rgbHex(negativeStartColour), rgbHex(negativeEndColour)]);
    var negativeScaleRange = [negativeScale(0)];
    for (var i = 0; i < steps; i++) {
        negativeScaleRange.push(negativeScale(i / steps));
    }
    negativeScaleRange.push(negativeScale(steps / steps));
    var negative = d3.scale.ordinal()
        .domain([minimum, maximum])
        .range(negativeScaleRange);
    
    var zero = chroma.scale([negativeStartColour]);

    //
    // Default Scale.
    //
    var scaleType = 'diverging';
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
        (scaleType !== "sequential" && extent[0] * extent[1] < 0)
    ) {
        // var max = Math.max(Math.abs(extent[0]), Math.abs(extent[1]));

        scale = d3.scale.linear().domain([-1 * maximum, 0, maximum]).range([-1, 0, 1]);
        // thresholdBase = computeTicks(extent);
        // // console.log('chroma', chroma)

        // // scaleF = function(v) {
        // //     var s = scale(v);
        // //     if (s < 0) {
        // //         return negative(s);
        // //     } else if (s > 0) {
        // //         return positive(s);
        // //     }
        // //     return zero;
        // // };

        // // threshold = d3.scale.threshold()
        // //     .domain(thresholdBase)
        // //     .range(thresholdBase.map(function(t) {
        // //         return scaleF(t);
        // //     }));
        // // linear = d3.scale.linear()
        // //     .domain(thresholdBase)
        // //     .range(thresholdBase.map(function(t) {
        // //         return scaleF(t);
        // //     }));
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
    svg.data([scaledata])
        .attr("width", width)
        .attr("height", height);
    step = (width - 100) / steps;

    var sd = Array.from(Array(steps).keys(), i => [i * step, scaledata[i][2]]);

    // console.log(sd, scaledata)

    svg.selectAll('g').remove();

    var g = svg.selectAll("g")
        .data([sd])
        .enter()
        .append("g")
        .attr("class", "key")
        .attr("transform", "translate(50," + height * 1 / 3 + ")");

    var returnVals = [];

    var legend = g.selectAll("rect")
        .data(sd)
        .enter()
        .append("rect")
        .attr("height", 8)
        .attr("x", function (d) { return d[0] % innerWidth; })
        .attr("width", step)
        .style("fill", function (d, i) {
            returnVals.push({
                value: scaledata[i][0],
                colour: positive(d[0]).hex()
            });

            return positive(d[0]); 
        })
        .style("stroke", function (d) { return positive(d[0]); });

    g.selectAll(".caption").remove();
    let xa = g.call(xAxis);
    xa.append("text")
        .attr("class", "caption")
        .attr("y", -6)
        .text("caption goes here");

    console.log(returnVals)
    return returnVals;
}