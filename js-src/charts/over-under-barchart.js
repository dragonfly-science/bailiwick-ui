import d3 from 'd3';
import _ from 'lodash';
import chartSetup from '../utils/chart-setup';
import { none, isEmpty, present } from '../utils/utils';

var margin = {top: 25, right: 15, bottom: 40, left: 100};
var x = d3.scale.ordinal();
var y = d3.scale.linear();
var xAxis = d3.svg.axis().scale(x).orient("top");
var yAxis = d3.svg.axis()
  .scale(y)
  .tickFormat(function(d) {
    return Math.abs(d);
  })
  .orient("left");

var barHeight = 240;

var percentageCaption = ["Percentage achieved", "Percentage not acheived"];
var absoluteCaption = ["Achieved", "Not acheived"];

export default function(element, params) {
    console.log('Under Over', element, params);
    
    let setup = chartSetup(element, params, margin, {
        'default-timeseries': false,
        'basic-barchart': false,
        'area-treemap': false,
        'overunder-barchart': true
    });

    if (setup === null) {
        return;
    }

    let cache = setup.cache, 
        toCache = setup.toCache, 
        data = setup.data, 
        year = setup.year, 
        indicator = setup.indicator, 
        transform = setup.transform, 
        area = setup.area, 
        areaLevel = setup.areaLevel, 
        svg = setup.svg, 
        width = setup.width, 
        height = setup.height;
    //
    // setup.
    //
    // var container = d3.select(this.get('element')).select('svg.d3-attach'),
    // tooltipElem = d3.select(this.get('element')).select(".tooltip");
    // margin = {top: 25, right: 15, bottom: 40, left: 100};
    // width = parseInt(container.style("width")) - margin.left - margin.right;
    // height = parseInt(container.style("height")) - margin.top - margin.bottom;

    x = x.range([0, width]);
    let g = svg.selectAll("g").data([1]);
    var svgEnter = g.enter()
      .append("g");
    g
      .attr("width", width)
      .attr("height", height)
      .attr("transform", "translate(" + margin.left + "," + margin.top + ")");

    // this.setdata();
    console.log('data', data)

    //
    // Set data.
    //
    // var data = this.getAttr('data'),
    // feature = this.getAttr('feature'),
    // featureType = this.getAttr("featuretype"),
    // ftp = featureType || "",
    // mapping = this.getAttr('config').mapping,
    // area = this.getAttr('area');
    // if (!(data && area)) {
    //   return;
    // }
    // if (none(data.get("id").match("school-leavers"))) {
    //   return;
    // }
    // var dataState = data.get('id') + "-" + area.get("id") + feature + ftp + this.getAttr('transform');
    // if (this.get("_dataState") !== dataState) {
    //   var areaData;
    //   if (none(feature)) {
    //     var defFeature = this.getAttr('defaultFeature');
    //     if (present(defFeature)) {
    //       featureType = defFeature.get('parent');
    //     }
    //   }
    //   if (none(featureType)) {
    //     areaData = data.get("areas")[area.get("id")].features;
    //   } else {
    //     var a = data.get("areas")[area.get('id')].features;
    //     if (none(a)) {
    //       return;
    //     }
    //     areaData = a.children.find(function(d) {
    //       return d.name === featureType;
    //     });
    //   }
    //   var plotdata;
    //   if (this.getAttr('transform') === 'percentage') {
    //     plotdata = areaData.children.map(function(d) {
    //       d.value     = d.percentage;
    //       d.dispValue = d.dispPercentage;
    //       return d;
    //     });
    //   } else {
    //     plotdata = areaData.children.map(function(d) {
    //       d.value     = d.absolute;
    //       d.dispValue = d.dispAbsolute;
    //       return d;
    //     });
    //   }
    //   var mappedData = mapping.map(function(m) {
    //     return {label: m.label, values: m.dimensions.map(function(d) {
    //       return plotdata.filter(function(l) {
    //         return l.slug === d;
    //       })[0];
    //     })
    //     };
    //   });
    //   this.set('plotdata', mappedData);
    //   this.set('_dataState', dataState);


    //
    // Update
    //
    // var _this = this;
    // var plotdata = this.get('plotdata');
    // var feature = this.getAttr('feature');
    // var featureSlug = present(feature) ? feature.get("id") : null;
    // var transform = this.getAttr('transform');
    // if (none(plotdata)) {
    //   return;
    // }

    // yAxis.tickSize(-width + 10, 0)

    // x.domain(plotdata.map(function(d) {
    //   return d.label;
    // })).rangeRoundBands([0, width], 0.55, 0.3);

    // var over = Math.round(plotdata[0].values[0].value + plotdata[0].values[1].value),
    //     under = d3.max(plotdata, function(d) {
    //   return d.values[1].value;
    // });

    // height = (over + under) / over * barHeight;
    // svg.attr('height', height);
    // y.domain([-1 * under, over])
    //  .range([height, 0]);

    // var ySel = svg.selectAll("g.y.axis").data([plotdata]),
    //     ySelEnter = ySel.enter().append("g")
    //   .attr("class", "y axis");
    // ySel
    //   .call(yAxis);
    // ySel.exit().remove();

    // var yCaption = ySel.selectAll("text.caption")
    //   .data(transform === "percentage" ? percentageCaption : absoluteCaption);
    // var yCaptionEnter = yCaption.enter()
    //   .append("text")
    //   .attr("class", "caption")
    //   .attr("y", -60)
    //   .attr("transform", "rotate(270)");

    // yCaption
    //   .attr("x", function(d, i) {
    //     return -1 * i * height;
    //   })
    //   .attr("text-anchor", function(d, i) {
    //     return i ? "start" : "end";
    //   })
    //   .text(function(d) {
    //     return d;
    //   })

    //   yCaption.exit().remove();

    // var xSel = svg.selectAll("g.x.axis").data([plotdata]),
    //     xSelEnter = xSel.enter().append("g")
    //   .attr("class", "x axis");
    // xSel
    //   .call(xAxis);
    // xSel.exit().remove();

    // var overBar = svg.selectAll("rect.overbar")
    //   .data(plotdata.map(function(d) {
    //                     var o = d.values[0];
    //                     o.label = d.label;
    //                     return o;
    //   }));
    // var overBarEnter = overBar.enter().append("rect")
    //   .attr("class", "overbar")
    //   .on("click", function(d) {
    //     _this.sendAction("featureAction", d.slug);
    //   });
    // overBar
    //   .attr("y", function(d) {
    //     return y(d.value);
    //   })
    //   .attr("height", function(d) {
    //     return y(0) - y(d.value);
    //   })
    //   .attr("x", function(d) { return x(d.label); })
    //   .attr("width", x.rangeBand())
    //   .classed("active", function(d) {
    //     return d.slug === featureSlug
    //   });
    // overBar.exit().remove();

    // var underBar = svg.selectAll("rect.underbar")
    //   .data(plotdata.map(function(d) {
    //                     var o = d.values[1];
    //                     o.label = d.label;
    //                     return o;
    //   }));
    // var underBarEnter = underBar.enter().append("rect")
    //   .attr("class", "underbar")
    //   .on("click", function(d) {
    //     _this.sendAction("featureAction", d.slug);
    //   });
    // underBar
    //   .attr("y", function(d) {
    //     return y(0);
    //   })
    //   .attr("height", function(d) {
    //     return y(-1 * d.value) - y(0);
    //   })
    //   .attr("x", function(d) { return x(d.label); })
    //   .attr("width", x.rangeBand())
    //   .classed("active", function(d) {
    //     return d.slug === featureSlug
    //   });
    // underBar.exit().remove();

    // if (!Modernizr.touch) {
    // var tooltipElem = d3.select(this.get('element')).select(".tooltip");
    //   svg.selectAll("rect")
    //     .on("mouseover", function(d) {
    //       var tooltip = tooltipElem.selectAll('p')
    //         .data([d.name, d.dispValue]),
    //       tooltipEnter = tooltip.enter().append('p');

    //       tooltip.text(function(d) {
    //         if (present(d) && d.length >= 2) {
    //           return d[0].toUpperCase() + d.slice(1);
    //         }
    //         return d;
    //       }).classed("number", function(d, i) {
    //         return i === 1;
    //       }).classed("local", function(d, i) {
    //         return i === 1;
    //       });

    //       tooltipElem.style("visibility", "visible")
    //         .style("top", function() {
    //           return (d3.event.offsetY) + "px";
    //         })
    //       .style("left", function() {
    //         return (d3.event.offsetX) + "px";
    //       });

    //     }).on("mouseout", function(d) {
    //       tooltipElem.style("visibility", "hidden");
    //     });

    // }

    // var zline = d3.svg.line()
    //   .x(function(d) { 
    //     return d;
    //   })
    //   .y(function(d) {
    //     return y(0);
    //   });
    // var zeroLine = svg.selectAll("path.zeroline").data([[0, width]]);
    // var zeroLineEnter = zeroLine.enter().append('path')
    //   .attr("class", "zeroline");
    // zeroLine.attr("d", zline);
    // zeroLine.exit().remove();
}