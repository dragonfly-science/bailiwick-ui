import d3 from 'd3';
import _ from 'lodash';
import chartSetup from '../utils/chart-setup';
import { none, isEmpty, present } from '../utils/utils';

const margin = { top: 25, right: 15, bottom: 40, left: 100 };
const barHeight = 240;
const percentageCaption = ["Percentage achieved", "Percentage not acheived"];
const absoluteCaption = ["Achieved", "Not acheived"];
const nz = 'New Zealand';

export default function(element, params) {

    let setup = chartSetup(element, params, margin, 'overunder-barchart');

    if (setup === null) {
        return;
    }

    let cache = window.MBIECacheStorage,
        toCache = {},
        data = setup.data, 
        year = setup.year, 
        indicator = setup.indicator, 
        transform = setup.transform, 
        area = setup.area, 
        areaLevel = setup.areaLevel,
        feature = setup.feature,
        features = setup.features,
        chartData = setup.chartData,
        svg = setup.svg,
        base = setup.base,
        width = setup.width, 
        height = setup.height;
    
    let x = d3.scale.ordinal(),
        y = d3.scale.linear(),
        xAxis = d3.svg.axis().scale(x).orient("top"),
        yAxis = d3.svg.axis()
                  .scale(y)
                  .tickFormat(function (d) {
                    return Math.abs(d);
                  })
                  .orient("left");

    x = x.range([0, width]);
    svg.selectAll('g').remove();
    let g = svg.selectAll("g").data([1]);
    var svgEnter = g.enter()
      .append("g");
    g
      .attr("width", width)
      .attr("height", height)
      .attr("transform", "translate(" + margin.left + "," + margin.top + ")");

    var areaData = _.filter(data, function(i) {
        return i[0][1] === area;
    });

    areaData = _.reduce(areaData, function(res, v, k) {
        let values = {
            name: v[0][1],
            slug: v[0][0],
            display: '',
            value: 0,
            year: year,
            feature: _.last(v[0])
        }

        let yearVal = _.filter(v[1], function(o) {
            return o[0] === year;
        });

        if (!_.isEmpty(yearVal)) {
            values.value = Number(yearVal[0][1]);
            values.display = (yearVal[0][3]).trim();
            res.push(values);
        }

        return res;
    }, []);

    var mappedData = _.reduce(chartData.chartMapping, function(res, v, k) {
        let vals = _.map(v.mappingDimensions, function(d) {
            return {
                label: d,
                values: _.first(_.filter(areaData, function(a) {
                    return a.feature === d;
                }))
            }
        });

        let obj = {
            label: v.mappingLabel,
            values: vals
        };

        res.push(obj);
        return res;
    }, []);

    areaData = mappedData;

    if (none(areaData)) {
      return;
    }

    yAxis.tickSize(-width + 10, 0);

    x.domain(_.reduce(areaData, function(res, v, k) {
        res.push(v.label);
        return res;
    }, [])).rangeRoundBands([0, width], 0.55, 0.3);

    let over = _.reduce(areaData, function(max, val, key) {
        let above = _.filter(val.values, function(o) {
          return o.label.indexOf("above") !== -1;
        });

        if (!_.isEmpty(above)) {
          above = above[0];
          return Math.max(max, above.values.value);
        }
        return max;
    }, 0);

    let under = _.reduce(areaData, function(max, val, key) {
      let above = _.filter(val.values, function (o) {
        return o.label.indexOf("below") !== -1;
      });

      if (!_.isEmpty(above)) {
        above = above[0];
        return Math.max(max, above.values.value);
      }
      return max;
    }, 0);

    height = (over + under) / over * barHeight;
    svgEnter.attr('height', height);
    y.domain([-1 * under, over])
     .range([height, 0]);

    var ySel = svgEnter.selectAll("g.y.axis").data([areaData]),
        ySelEnter = ySel.enter().append("g").attr("class", "y axis");
    ySel
      .call(yAxis);
    ySel.exit().remove();

    var yCaption = ySel.selectAll("text.caption").data(
                    transform === "percentage" ? percentageCaption : absoluteCaption
                  );
    var yCaptionEnter = yCaption.enter()
      .append("text")
      .attr("class", "caption")
      .attr("y", -60)
      .attr("transform", "rotate(270)");

    yCaption
      .attr("x", function(d, i) {
        return -1 * i * height;
      })
      .attr("text-anchor", function(d, i) {
        return i ? "start" : "end";
      })
      .text(function(d) {
        return d;
      });

      yCaption.exit().remove();
    

    var xSel = svgEnter.selectAll("g.x.axis").data([areaData]),
        xSelEnter = xSel.enter().append("g")
      .attr("class", "x axis");
    xSel
      .call(xAxis);
    xSel.exit().remove();

    let overData = _.reduce(areaData, function(arr, val, key) {
        let above = _.filter(val.values, function (o) {
          return o.label.indexOf("above") !== -1;
        });

        if (!_.isEmpty(above)) {
          above = above[0];
          arr.push({
              value: above.values.value,
              label: val.label,
              name: above.values.name,
              dispValue: above.values.display,
              feature: above.values.feature
          });
        }

        return arr;
    }, []);
    
    let underData = _.reduce(areaData, function(arr, val, key) {
      let below = _.filter(val.values, function (o) {
        return o.label.indexOf("below") !== -1;
      });

      if (!_.isEmpty(below)) {
        below = below[0];
        arr.push({
          value: below.values.value,
          label: val.label,
          name: below.values.name,
          dispValue: below.values.display,
          feature: below.values.feature
        });
      }

      return arr;
    }, []);

    var overBar = svgEnter.selectAll("rect.overbar")
                    .data(overData);

    var overBarEnter = overBar.enter().append("rect")
      .attr("class", "overbar")
      .attr("data-bailiwick-feature", function (d) {
        return d.feature;
      })
      .on("click", function(d) {
        // _this.sendAction("featureAction", d.slug);
      });
    overBar
      .attr("y", function(d) {
        return y(d.value);
      })
      .attr("height", function(d) {
        return y(0) - y(d.value);
      })
      .attr("x", function(d) { return x(d.label); })
      .attr("width", x.rangeBand())
      .attr("data-bailiwick-feature", function (d) {
        return d.feature;
      })
      .classed("active", function(d) {
        return d.feature === feature
      });
    overBar.exit().remove();

    var underBar = svgEnter.selectAll("rect.underbar")
      .data(underData);
    var underBarEnter = underBar.enter().append("rect")
      .attr("class", "underbar")
      .attr("data-bailiwick-feature", function (d) {
        return d.feature;
      })
      .on("click", function(d) {
        // _this.sendAction("featureAction", d.slug);
      });
    underBar
      .attr("y", function(d) {
        return y(0);
      })
      .attr("height", function(d) {
        return y(-1 * d.value) - y(0);
      })
      .attr("x", function(d) { return x(d.label); })
      .attr("width", x.rangeBand())
      .classed("active", function(d) {
        return d.feature === feature
      });
    underBar.exit().remove();

    if (!Modernizr.touch) {
      var tooltipElem = d3.select(element).select(".tooltip");
      svgEnter.selectAll("rect")
        .on("mouseover", function(d) {
          var tooltip = tooltipElem.selectAll('p')
                          .data([features[d.feature], d.dispValue]),
              tooltipEnter = tooltip.enter().append('p');

          tooltip.text(function(d) {
            if (present(d) && d.length >= 2) {
              return d[0].toUpperCase() + d.slice(1);
            }
            return d;
          }).classed("number", function(d, i) {
            return i === 1;
          }).classed("local", function(d, i) {
            return i === 1;
          });

          tooltipElem.style("visibility", "visible")
            .style("top", function() {
              return (d3.event.offsetY) + "px";
            })
          .style("left", function() {
            return (d3.event.offsetX) + "px";
          });

        }).on("mouseout", function(d) {
          tooltipElem.style("visibility", "hidden");
        });
    }

    var zline = d3.svg.line()
      .x(function(d) { 
        return d;
      })
      .y(function(d) {
        return y(0);
      });
    
    var zeroLine = svgEnter.selectAll("path.zeroline").data([[0, width]]);
    var zeroLineEnter = zeroLine.enter().append('path')
                          .attr("class", "zeroline");
    zeroLine.attr("d", zline);
    zeroLine.exit().remove();

    base.classed('svg-loading', false);
}