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


export default function (element, params) {
    // console.log('updateAreaBarchart', element, data);
    // return false;

    //
    // Set up
    //
    var base = d3.select(element).select('.d3-attach');
    svg = base.select('svg').empty() ? base.append('svg') : base.select('svg');

    var data = params[0];

    if (isEmpty(data)) {
        return;
    }

    d3.select('.chart-inner')
        .classed({
            'default-timeseries': false,
            'basic-barchart': true,
            'area-treemap': false
        });
    // tooltipElem = d3.select(element').select(".tooltip");


    //var container = d3.select(this.get('element')).select('svg.d3-attach'),
    let lmargin = 140;
    if (window.innerWidth < 400) {
      lmargin = 100;
    } else if (window.innerWidth < 600 && lmargin > 140) {
      lmargin = 180;
    }
    
    margin = {top: 5, right: 15, bottom: 40, left: lmargin};

    width = parseInt(base.style("width")) - margin.left - margin.right;
    height = parseInt(base.style("height")) - margin.top - margin.bottom;

    svg.attr("preserveAspectRatio", "xMinYMin meet")
        .attr("viewBox", "0 0 481 474");

    x = x.range([0, width]);

    let g = svg.selectAll("g").data([1]);

    var svgEnter = g.enter().append("g");
    g
      .attr("width", width)
      .attr("height", height)
      .attr("transform", "translate(" + margin.left + "," + margin.top + ")");


    // Handle Units
    // TODO: handle formatting
    // var formatter = this.getAttr("caption").get("formatter");
    xAxis.scale(x)
        .orient("bottom")
        .ticks(window.innerWidth < 600 ? 2 : 3)
        // .tickFormat(function (d) {
        //     return formatter(d);
        // });
    
    // Set data
    console.log(data, params);
    // var fixedAxis = this.getAttr('config').axis,
        // feature = this.getAttr('feature'),
        // featureType = this.getAttr("featuretype"),
        // ftp = featureType || "",
    // let area = this.getAttr('area');

    // if (!(data && area)) {
    //     return;
    // }
    // var dataState = data.get('id') + "-" + area.get("id") + ftp + this.getAttr('transform');
    // //if (this.get("_dataState") !== dataState) {
    // var areaData;
    // if (none(feature)) {
    //     var defFeature = this.getAttr('defaultFeature');
    //     if (present(defFeature)) {
    //         featureType = defFeature.get('parent');
    //     }
    // }
    // if (none(featureType)) {
    //     var areaDataT = data.get("areas")[area.get("id")];
    //     if (none(areaDataT)) {
    //         return;
    //     }
    //     areaData = areaDataT.features;
    // } else {
    //     var a = data.get("areas")[area.get('id')];
    //     if (none(a)) {
    //         return;
    //     }
    //     var af = a.features;
    //     if (none(af)) {
    //         return;
    //     }
    //     areaData = af.children.find(function (d) {
    //         return d.name === featureType;
    //     });
    // }
    // if (none(fixedAxis)) {
    //     areaData.children = areaData.children.sort(function (a, b) {
    //         return d3.descending(a.absolute, b.absolute);
    //     });
    // } else {
    //     var m = d3.map(areaData.children, function (d) { return d.slug; });
    //     areaData.children = fixedAxis.map(function (d) {
    //         return m.get(d);
    //     });
    // }
    // if (this.getAttr('transform') === 'percentage') {
    //     this.set('plotdata', areaData.children.map(function (d) {
    //         d.value = d.percentage;
    //         d.dispValue = d.dispPercentage;
    //         return d;
    //     }));
    // } else {
    //     this.set('plotdata', areaData.children.filter(function (d) {
    //         return (typeof d !== 'undefined');
    //     }).map(function (d) {
    //         d.value = d.absolute;
    //         d.dispValue = d.dispAbsolute;
    //         return d;
    //     }));
    // }
    // this.set('_dataState', dataState);
    //}
}