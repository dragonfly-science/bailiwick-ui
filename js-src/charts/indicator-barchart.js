import d3 from 'd3';
import { none, isEmpty, present } from '../utils/utils';

var percentageFormatter = d3.format('.1f');
var margin, svg, width, height, tooltipElem;

var y = d3.scale.ordinal();

var x = d3.scale.linear();

var maxLength = 35;

if (window.innerWidth < 400) {
  maxLength = 11;
} else if (window.innerWidth < 600) {
  maxLength = 25;
}

var yAxis = d3.svg.axis()
    .scale(y)
    .orient("left")
    .outerTickSize(0)
    .tickFormat(function(d) {
        if (d.length > maxLength) {
            return d[0].toUpperCase() + d.substring(1,maxLength) + '…';
        }
        else {
            return d[0].toUpperCase() + d.slice(1);
        }
    });

var barHeight = 20,
    barGap = 5;

var xAxis = d3.svg.axis();


export default function (element, data) {
    console.log('updateAreaBarchart', element, data);

    //
    // Set up
    //
    var base = d3.select(element).select('.d3-attach');
    var svg = base.select('svg').empty() ? 
                base.append('svg') : 
                base.select('svg').empty().append('svg');

    d3.select('.chart-inner')
        .classed({
            'default-timeseries': false,
            'basic-barchart': true,
            'area-treemap': false
        });
    // var data = params[0];
    // tooltipElem = d3.select(element').select(".tooltip");


    //var container = d3.select(this.get('element')).select('svg.d3-attach'),
    // let lmargin = 140;
    // if (window.innerWidth < 400) {
    //   lmargin = 100;
    // } else if (window.innerWidth < 600 && lmargin > 140) {
    //   lmargin = 180;
    // }
    
    // margin = {top: 5, right: 15, bottom: 40, left: lmargin};

    // width = parseInt(svg.style("width")) - margin.left - margin.right;
    // height = parseInt(svg.style("height")) - margin.top - margin.bottom;

    // x = x.range([0, width]);

    // var svgEnter = svg.enter()
    //   .append("g");
    // svg
    //   .attr("width", width)
    //   .attr("height", height)
    //   .attr("transform", "translate(" + margin.left + "," + margin.top + ")");


    // this.handleUnits();
    // this.setdata();


    // var container = d3.select(element).select('svg.d3-attach')
    //   , tooltipElem = d3.select(element).select(".tooltip")
    //   , margin = {
    //       top: 5,
    //       right: 25,
    //       bottom: 40,
    //       left: 140
    //     };
    //
    // width = parseInt(container.style("width")) - margin.left - margin.right;
    // height = parseInt(container.style("height")) - margin.top - margin.bottom;
}