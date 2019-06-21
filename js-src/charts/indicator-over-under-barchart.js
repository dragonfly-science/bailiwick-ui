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
    console.log('Under Over', arguments);
    
    let setup = chartSetup(element, params, margin, 'overunder-barchart');
    let nz = 'New Zealand';

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
        chartData = setup.chartData,
        svg = setup.svg,
        base = setup.base,
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
    // console.log('data', data)

    var areaData = _.filter(data, function(i) {
        return i[0][1] === area || area === nz;
    });

    // console.log(areaData[0]);

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

    // areaData = _.groupBy(areaData, 'feature');

    // console.log(data.length, areaData.length, )

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

    // console.log(areaData);

    var mappedData = _.reduce(chartData.chartMapping, function(res, v, k) {
        console.log(v);
        // console.log(v, k);
        let vals = _.map(v.mappingDimensions, function(d) {
            return {
                label: d,
                values: _.filter(areaData, function(a) {
                    return a.feature === d;
                })
            }
        });

        // console.log(v, vals);

        let obj = {
            label: v.mappingLabel,
            values: vals
        };

        res.push(obj);
        return res;
    }, []);
    // console.log(mappedData);

    areaData = mappedData;
    // return;

    // var mapping = 


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

    if (none(areaData)) {
      return;
    }

    yAxis.tickSize(-width + 10, 0);

    x.domain(_.reduce(areaData, function(res, v, k) {
        res.push(v.label);
        return res;
    }, [])).rangeRoundBands([0, width], 0.55, 0.3);

    // return;

    // var over = Math.round(plotdata[0].values[0].value + plotdata[0].values[1].value),

    // TODO: this is far too complex... CC = 9 currently.
    let over = _.reduce(areaData, function(max, val, key) {
        let maxVals = _.reduce(val.values, function(mm, vv, kk) {
            let results = _.reduce(vv.values, function(res, v, k) {
                if (v.feature.indexOf('above') !== -1) {
                    res.push(v.value);
                }

                return res;
            }, []);

            if (!_.isEmpty(results)) {
                return Math.max(mm, _.max(results));
            }

            return mm;
        }, 0);

        return Math.max(max, maxVals);
    }, 0);

    let under = _.reduce(areaData, function(max, val, key) {
        let maxVals = _.reduce(val.values, function(mm, vv, kk) {
            let results = _.reduce(vv.values, function(res, v, k) {
                if (v.feature.indexOf('below') !== -1) {
                    res.push(v.value);
                }

                return res;
            }, []);

            if (!_.isEmpty(results)) {
                return Math.max(mm, _.max(results));
            }

            return mm;
        }, 0);

        return Math.max(max, maxVals);
    }, 0);


    // return;
    // let under = _.reduce(areaData, function(max, val, key) {
    //     // console.log(key);
    //     if (key.indexOf('below') !== -1) {
    //         return Math.max(max, val[0].value);
    //     }
    //     return max;
    // }, 0);
    // let under = d3.max(areaData, function(d) {
    //     console.log(d);
    //   return d[0].value;
    // });

    

    height = (over + under) / over * barHeight;
    // console.log(over, under, height);
    svgEnter.attr('height', height);
    y.domain([-1 * under, over])
     .range([height, 0]);

    var ySel = svgEnter.selectAll("g.y.axis").data([areaData]),
        ySelEnter = ySel.enter().append("g").attr("class", "y axis");
    ySel
      .call(yAxis);
    ySel.exit().remove();

    var yCaption = ySel.selectAll("text.caption")
      .data(transform === "percentage" ? percentageCaption : absoluteCaption);
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


    // var overData = _.reduce(areaData, function(arr, val, key) {

    //     console.log(val);
    //     // console.log(key);
    //     // if (key.indexOf('above') !== -1) {
    //     //     // console.log(val, key)
    //     //     // return Math.max(max, val[0].value);
    //     //     arr.push({
    //     //         value: val[0].value,
    //     //         label: val[0].feature
    //     //     });
    //     // }
    //     return arr;
    // }, []);

    let overData = _.reduce(areaData, function(arr, val, key) {
        // console.log(val.values);
        let maxVals = _.reduce(val.values, function(res, vv, kk) {
            let results = _.reduce(vv.values, function(res, v, k) {
                if (v.feature.indexOf('above') !== -1) {
                    res.push({
                        value: v.value,
                        label: val.label,
                        feature: v.feature
                    });
                }

                return res;
            }, []);

            if (!_.isEmpty(results)) {
                res.push(results);
            }

            return res;
        }, []);

        // console.log(maxVals);

        if (!_.isEmpty(maxVals)) {
            arr.push(maxVals);
        }

        return arr;
    }, []);

    console.log('over', overData);


    // var underData = _.reduce(areaData, function(arr, val, key) {
    //     // console.log(key);
    //     if (key.indexOf('below') !== -1) {
    //         // console.log(val, key)
    //         // return Math.max(max, val[0].value);
    //         arr.push({
    //             value: val[0].value,
    //             label: val[0].feature
    //         });
    //     }
    //     return arr;
    // }, []);
    let underData = _.reduce(areaData, function(arr, val, key) {
        // console.log(val.values);
        let maxVals = _.reduce(val.values, function(res, vv, kk) {
            let results = _.reduce(vv.values, function(res, v, k) {
                if (v.feature.indexOf('below') !== -1) {
                    res.push({
                        value: v.value,
                        label: val.label,
                        feature: v.feature
                    });
                }

                return res;
            }, []);

            if (!_.isEmpty(results)) {
                res.push(results);
            }

            return res;
        }, []);

        // console.log(maxVals);

        if (!_.isEmpty(maxVals)) {
            arr.push(maxVals);
        }

        return arr;
    }, []);

    var overBar = svgEnter.selectAll("rect.overbar")
                    .data(overData);


    var overBarEnter = overBar.enter().append("rect")
      .attr("class", "overbar")
      .on("click", function(d) {
        // _this.sendAction("featureAction", d.slug);
      });
    overBar
      .attr("y", function(d) {
        return y(d[0][0].value);
      })
      .attr("height", function(d) {
        return y(0) - y(d[0][0].value);
      })
      .attr("x", function(d) { return x(d[0][0].label); })
      .attr("width", x.rangeBand())
      .classed("active", function(d) {
        return d[0][0].feature === feature
      });
    overBar.exit().remove();

    var underBar = svgEnter.selectAll("rect.underbar")
      .data(underData);
    var underBarEnter = underBar.enter().append("rect")
      .attr("class", "underbar")
      .on("click", function(d) {
        // _this.sendAction("featureAction", d.slug);
      });
    underBar
      .attr("y", function(d) {
        return y(0);
      })
      .attr("height", function(d) {
        return y(-1 * d[0][0].value) - y(0);
      })
      .attr("x", function(d) { return x(d[0][0].label); })
      .attr("width", x.rangeBand())
      .classed("active", function(d) {
        return d[0][0].feature === feature
      });
    underBar.exit().remove();

    if (!Modernizr.touch) {
    var tooltipElem = d3.select(element).select(".tooltip");
      svgEnter.selectAll("rect")
        .on("mouseover", function(d) {
        //   var tooltip = tooltipElem.selectAll('p')
        //     .data([d.name, d.dispValue]),
        //   tooltipEnter = tooltip.enter().append('p');

        //   tooltip.text(function(d) {
        //     if (present(d) && d.length >= 2) {
        //       return d[0].toUpperCase() + d.slice(1);
        //     }
        //     return d;
        //   }).classed("number", function(d, i) {
        //     return i === 1;
        //   }).classed("local", function(d, i) {
        //     return i === 1;
        //   });

        //   tooltipElem.style("visibility", "visible")
        //     .style("top", function() {
        //       return (d3.event.offsetY) + "px";
        //     })
        //   .style("left", function() {
        //     return (d3.event.offsetX) + "px";
        //   });

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