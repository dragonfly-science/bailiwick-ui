Math.trunc = Math.trunc || function(x) {
    return x < 0 ? Math.ceil(x) : Math.floor(x);
};
var none = function(obj) {
        return obj === null || obj === undefined;
    },
    isEmpty = function(obj) {
        return _.isEmpty(obj);
    },
    present = function(obj) {
        return isEmpty(obj) || (typeof obj === 'string' && /\S/.test(obj) === false);
    };


/**
 * Update the time series on the home page side bar - housePriceTimeSeries
 **/
var updateTimeSeries = function(element, labelledData, activeLabelName) {
    var base = d3.select(element).select('.d3-attach'),
        svg = base.select('svg').empty() ? base.append('svg') : base.select('svg'),
        yearFormat = d3.time.format('%Y').parse,
        margin = {
            top: 15,
            right: 0,
            bottom: 30,
            left: 48
        },
        baseW = 225,
        baseH = 120,
        width = baseW - margin.left - margin.right,
        height = baseH - margin.top - margin.bottom,
        _this = this;

    svg.attr('width', baseW);
    svg.attr('height', baseH);

    var data = labelledData.map(function(d) {
        return d[1];
    });

    var x = d3.time.scale()
        .range([0, width]);

    var y = d3.scale.linear()
        .range([height, 0]);

    var xAxis = d3.svg.axis()
        .scale(x)
        .ticks(5)
        .innerTickSize(3)
        .outerTickSize(0)
        .tickFormat(function(t) {
            return t.getFullYear().toString().replace(/^20/, "'");
        })
        .orient("bottom");

    var yAxis = d3.svg.axis()
        .scale(y)
        .ticks([4])
        .outerTickSize(0)
        .innerTickSize(-width)
        .tickFormat(function(t) {
            return '$' + t / 1000 + ' k';
        })
        .orient("left");

    var line = d3.svg.line()
        .x(function(d) {
            return x(yearFormat(d[0].toString()));
        })
        .y(function(d) {
            return y(d[1]);
        });

    var g = svg.selectAll('g').data([data]);
    var gEnter = g.enter()
        .append("g")
        .attr("transform", "translate(" + margin.left + "," + margin.top + ")");

    var yMax = Math.ceil(d3.max(d3.merge(data), function(d) {
        return d[1];
    }) / 100000) * 100000;
    x.domain(d3.extent(data[0], function(d) {
        return yearFormat(d[0].toString());
    }));
    y.domain([0, yMax]);

    gEnter.append("g")
        .attr("class", "x axis")
        .attr("transform", "translate(0," + height + ")")
        .append("text")
        .attr("x", 50)
        .attr("dy", "2.5em")
        .text("Year");

    g.selectAll("g.x")
        .call(xAxis);

    gEnter.append("g")
        .attr("class", "y axis")
        .attr("transform", "translate(-7,0)");

    g.selectAll("g.y")
        .call(yAxis);

    var linePlot = g.selectAll("path.line").data(labelledData);
    linePlot.enter()
        .append("path");

    linePlot.attr("d", function(d) {
        return line(d[1]);
    }).attr("class", function(d, i) {
        if (activeLabelName === d[0]) {
            return "line active";
        }
        return "line";
    });

    linePlot.exit().remove();
}

/**
 * Legend for the map
 **/
var updateMapLegend = function(width, height, scaledata) {
    var svg = d3.select(".legend").selectAll("svg")
      .data([scaledata])
      .enter()
      .append("svg")
      .attr("width", width)
      .attr("height", height);
    var step = (width-100)/100;

    var sd = Array.from(Array(100).keys(),
                 i => [i*step, scaledata[i][2]]);
    // console.log(sd);

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
      .attr("x", function(d) { return d[0]; })
      .attr("width", step)
      .style("fill", function(d) { return d[1]; })
      .style("stroke", function(d) { return d[1]; });
}

/**
 * Initialise a bar chart - used on the indicator.
 **/
var updateAreaBarchart = function(element, data) {
    console.log('updateAreaBarchart', element, data);
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

/**
 * Time series used on an indicator
 * * data: { year, selected area, areas }
 **/
var yearFormat = d3.time.format('%Y').parse;
var transforms = ["absolute", "indexed", "percapita"];
var x = d3.time.scale();

var y = d3.scale.linear();

var clipHeight = 393;
var clipWidth = 363;
var margin = {top: 40, right: 40, bottom: 40, left: 80};

var line = d3.svg.line()
  .x(function(d, i) { return x(d.date); })
  .y(function(d) { return y(d.v); });

// data: (data, year, indicator, transform)
/*
    Data requirements:
    ------------------
    There will need to be some modifications to the data provided
    in order to suit what the timeseries needs:
    1. Need area name as well as ID
    2. Need to know the area type (e.g. region, ta, ward)
*/
var updateIndicatorTimeSeries = function(element, data) {
    var transform = data[3];
    var indicator = data[2];
    var year = data[1];
    var svg = d3.select(element).select('svg.d3-attach');
    var legendDiv = d3.select(element).select('.legend');
    var width = parseInt(svg.style("width")) - margin.left - margin.right;
    var height = parseInt(svg.style("height")) - margin.top - margin.bottom;
    var legendWidth = window.innerWidth < 350 ? 320 : 420;
    var legendHeight = 50;

    if (isNaN(width) || isNaN(height)) {
        return;
    }

    // console.lof(data[0])

    /// Setup
    x.range([0, width]);
    y.range([height, 0]);

    voronoi = d3.geom.voronoi()
      .x(function(d) { return x(d.date); })
      .y(function(d) { return y(d.v); })
      .clipExtent([[-margin.left, -margin.top], [width + margin.right, height + margin.bottom]]);


    /// Update
    svg.selectAll('g').remove();
    var g = svg.selectAll('g').data([data[0]])
      , gEnter = g.enter()
                    .append("g")
                    .attr("transform", "translate(" + margin.left + "," + margin.top + ")");
    
    var years = _.uniq(_.reduce(data[0], function(result, value, key) {
        return _.reduce(value[1], function(res, v, k) {
            res.push(v[0]);
            return res;
        }, result);
    }, [])).sort(function(a, b) {
        return d3.ascending(a, b);
    }).map(function(y) {
        return yearFormat(y.toString());
    });

    var transformPos = transforms.indexOf(transform);
    var pos = transformPos === -1 ? 1 : transformPos + 1;
    var dispPos = transformPos === -1 ? 4 : transformPos + 4;

    var areas = data[0].map(function(a) {
        var area = {
            slug: a[0],
            name: a[0],
            dsArea: a[0],
            values: a[1].map(function (y) {
                var out = {};
                out.date = yearFormat(y[0].toString());
                out.v = y[pos];
                out.d = y[dispPos];
                console.log(y, out.v, out.d);
                return out;
            }).filter(function (d) {
                return d.v !== null;
            })
        };

        console.log(area);
  
        // TODO: not sure about this?
        // area.values.forEach(function(o) {
        //   o.area = area;
        // });

        return area;
        // return [];
      });
    
    // TODO: once we have area type, we can filter the results.
    // ----
    //   .filter(function(a) {
    //       var valid = false;
  
    //       switch (a.name) {
    //           case compareAreaName:
    //               valid = true;
    //               break;
    //           case areaName:
    //               valid = true;
    //               break;
    //           case 'New Zealand':
    //               valid = true;
    //               break;
    //           default:
    //               break;
    //       }
  
    //       var level = a.dsArea.get('level');
  
    //       return level === areaType || valid;
    //   });

    var xExtent = d3.extent(years);
    x.domain(xExtent);

    var yExtent = d3.extent(d3.merge(areas.map(function (a) {
        return a.values.map(function (d) {
            return d.v;
        });
    })));


    // TODO: activate when we have zoom
    // if (this.get("zoomedIn")) {
    //     let areaVals = findAreaBounds(areas, areaName);

    //     if (typeof compareAreaName != 'undefined') {
    //         let compareVals = findAreaBounds(areas, compareAreaName);

    //         yExtent = [Math.min(...[areaVals[0], compareVals[0]])
    //             , Math.max(...[areaVals[1], compareVals[1]])];
    //     } else {
    //         yExtent = areaVals;
    //     }

    //     let diff = Math.abs(yExtent[1] - yExtent[0]);

    //     yExtent[0] -= (.1 * diff);
    //     yExtent[1] += (.1 * diff);
    // }

    y.domain(yExtent).nice();
    var tickFreq = Math.trunc(years.length / 9) + 1,
        ticks = years.filter(function (t, i) {
            return (i % tickFreq) === 0;
        });

    if (window.innerWidth < 600) {
        ticks = d3.extent(years);
    }

    /*
     * Generate the vertical line for the selected year.
     * */
    var xAxisYears = d3.svg.axis()
        .scale(x)
        .tickValues(years)
        .orient("bottom");

    gEnter.append("g")
        .attr("class", "axis axis--x-hidden")
        .attr("transform", "translate(0," + height + ")")
        .call(xAxisYears);

    var allTicks = svg.call(xAxisYears).selectAll(".tick"),
        lineXpos = 0,
        previousYear = (new Date(years[0])).getFullYear();

    allTicks.each(function (data, i) {
        var tick = d3.select(this),
            date = new Date(data),
            fullYear = date.getFullYear(),
            targetYear = parseInt(year);
            transform = d3.transform(tick.attr("transform")).translate;

        if (targetYear === fullYear) {
            lineXpos = transform[0];
        }
    });

    if (lineXpos <= 0) {
        lineXpos = 1;
    }

    gEnter.append("line")
        .attr("x1", lineXpos)
        .attr("y1", 0)
        .attr("x2", lineXpos)
        .attr("y2", clipHeight)
        .attr("stroke-width", 1)
        .attr("stroke", "rgba(0,0,0,.5)")
        .attr("z", 100)
        .attr("class", "year-line");
    /*
     * End year line generation.
     * */

    var xAxis = d3.svg.axis()
        .scale(x)
        .tickValues(ticks)
        .orient("bottom");
    
    gEnter.append("g")
        .attr("class", "axis axis--x")
        .attr("transform", "translate(0," + height + ")")
        .attr("z", 100)
        .call(xAxis)
        .append("text")
        .attr("class", "caption")
        .attr("y", 5)
        .attr("x", width + 10)
        .text("Year");
    
        // TODO: formatting
    gEnter.append("g")
        .attr("class", "axis axis--y")
        .call(d3.svg.axis()
            .scale(y)
            .orient("left")
            .ticks(10)
            // .tickFormat(function (d) { return formatter(d); }))
        )
        .append("text")
        .attr("class", "caption title")
        .attr("y", -30)
        .attr("x", 0)
        .text(transform);
        // .text(
        //     bailiwick.label(transform)
        // );

    gEnter.append("clipPath")
        .attr("id", "clipper")
        .append("rect")
        .attr("x", 0)
        .attr("y", 0)
        .attr("width", clipWidth)
        .attr("height", clipHeight);

    // svg.selectAll(".axis--x .tick")
    //     .on("click", function (d) {
    //         let year = (new Date(d)).getFullYear() + '';

    //         let filter = _this.get('bailiwick.indicator').get('years').filter(function (y) {
    //             return y.get('name') === year;
    //         });

    //         if (filter.length === 1) {
    //             _this.set('bailiwick.year', filter[0]);
    //         }

    //         _this.transitionTo({ 'year': filter[0], 'area': d.area.dsArea });
    //     });

    var path = gEnter.append("g")
        .attr("class", "areas")
        .selectAll("path")
        .data(areas);
    path.enter()
        .append("path");
    
    path.attr("d", function (d) { 
        d.line = this;
        console.log(d, d.values.v, d.values.date);
        return line(d.values); 
    });

    // Data has not actually changed - no path is empty
    // but need need to update highlighing so grab everything.
    g.selectAll('g.areas').selectAll('path')
        .attr("class", function (d) {
            // if (d.name === areaName) {
            //     return "current-area";
            // } else if (d.name === compareAreaName) {
            //     return "compare-area";
            // } else if (d.name === hover) {
            //     return "area--hover";
            // } else if (d.name === "New Zealand") {
            //     return "new-zealand";
            // }
            return "no-highlight";
        }).attr("clip-path", "url(#clipper)");
    
    var focusElemEnter = gEnter.append("g")
        .attr("transform", "translate(-100,-100)")
        .attr("class", "focus");

    focusElemEnter.append("circle")
        .attr("r", 3.5);
    focusElemEnter.append("text")
        .attr("y", -10);

    focusElem = g.selectAll('g.focus');

    var voronoiGroup = gEnter.append("g")
        .attr("class", "voronoi")
        .attr("clip-path", "url(#clipper)");

    // console.log(d3.nest()
    //     .key(function (d) { return x(d.date) + "," + y(d.v); })
    //     .rollup(function (v) { return v[0]; })
    //     .entries(
    //         d3.merge(
    //             areas.map(
    //                 function (d) { return d.values; }
    //             )
    //         )
    //     )
    //     .map(function (d, i) { return d.values; }));

        // return false;

    var vg = voronoiGroup.selectAll("path")
        .data(
            voronoi(
                d3.nest()
                    .key(function (d) { return x(d.date) + "," + y(d.v); })
                    .rollup(function (v) { return v[0]; })
                    .entries(
                        d3.merge(
                            areas.map(
                                function (d) { return d.values; }
                            )
                        )
                    )
                    .map(function (d, i) { return d.values; })
            )
        )
        .enter().append("path")
        .attr("d", function (d) {
            console.log('d', present(d));
            return present(d) ? "M" + d.join("L") + "Z" : "";
        })
        .datum(function (d) {
            return present(d) ? d.point : null; 
        });
        // .on("click", function (d) {
        //     let filter = _this.get('bailiwick.indicator').get('years').filter(function (y) {
        //         return y.get('name') === d[0];
        //     });

        //     if (filter.length === 1) {
        //         _this.set('bailiwick.year', filter[0]);
        //     }

        //     _this.transitionTo({ 'year': filter[0], 'area': d.area.dsArea });
        // });

    // TODO: import Modernizr
    // if (!Modernizr.touch) {
    //     vg.on("mouseover", mouseover)
    //         .on("mouseout", mouseout)

    // }

    var legendClasses = ["active", "other"];
    var legendLabels = ["New Zealand", "Other"];
    // if (present(areaName) && areaName !== "New Zealand") {
    //     legendLabels.push(areaName);
    //     legendClasses = ["nz", "other", "active"];
    // }
    // if (present(compareAreaName) && compareAreaName !== "New Zealand" && compareAreaName !== areaName) {
    //     legendLabels.push(compareAreaName);
    //     legendClasses.push('compare');
    // }
    var legendData = d3.zip(legendLabels, legendClasses);

    var legend = legendDiv.selectAll("svg").data([legendData]);
    var legendEnter = legend.enter().append("svg");
    legend.attr("width", legendWidth)
        .attr("height", legendHeight);

    var legendG = legend.selectAll("g.key").data([legendData]);
    var legendGEnter = legendG
        .enter()
        .append("g")
        .attr("class", "key");
    legendG
        .attr("transform", "translate(" + (window.innerWidth < 350 ? 20 : margin.left) + "," + legendHeight / 3 + ")");

    var legendRects = legendG.selectAll("rect").data(legendData);
    var legendRectsEnter = legendRects
        .enter()
        .append("rect")
        .attr("height", 8)
        .attr("width", 55);
    legendRects
        .attr("x", function (d, i) {
            return Math.floor(i / 2) * 140;
        })
        .attr("y", function (d, i) {
            return (i % 2) * 20;
        })
        .attr("class", function (d) {
            return d[1];
        });
    legendRects.exit().remove();

    var legendTexts = legendG.selectAll("text").data(legendData);
    var legendTextsEnter = legendTexts
        .enter()
        .append("text")
        .attr("dx", "65px")
        .attr("dy", "0.7em");
    legendTexts
        .attr("x", function (d, i) {
            return Math.floor(i / 2) * 140;
        })
        .attr("y", function (d, i) {
            return (i % 2) * 20;
        })
        .text(function (d) {
            return d[0];
        });
    legendTexts.exit().remove();
    // this.$().removeClass("svg-loading");
}




