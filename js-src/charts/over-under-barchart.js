import d3 from 'd3';
import _ from 'lodash';
import { none, isEmpty, present } from '../utils/utils';

var margin, svg, width, height;
var x = d3.scale.ordinal();
var y = d3.scale.linear();
var xAxis = d3.svg.axis().scale(x).orient("top");
var yAxis = d3.svg.axis()
    .scale(y)
    .tickFormat(function (d) {
        return Math.abs(d);
    })
    .orient("left");

var barHeight = 240;

var percentageCaption = ["Percentage achieved", "Percentage not acheived"];
var absoluteCaption = ["Achieved", "Not acheived"];

export default function() {
    // setup.
    // var container = d3.select(this.get('element')).select('svg.d3-attach'),
    //     tooltipElem = d3.select(this.get('element')).select(".tooltip");
    // margin = { top: 25, right: 15, bottom: 40, left: 100 };
    // width = parseInt(container.style("width")) - margin.left - margin.right;
    // height = parseInt(container.style("height")) - margin.top - margin.bottom;

    // x = x.range([0, width]);
    // svg = container.selectAll("g").data([1]);
    // var svgEnter = svg.enter()
    //     .append("g");
    // svg
    //     .attr("width", width)
    //     .attr("transform", "translate(" + margin.left + "," + margin.top + ")");
    // this.setdata();
}