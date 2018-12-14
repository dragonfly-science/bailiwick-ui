/**
 * Set up the default time series.
 **/
import * as d3 from 'd3'
import $ from 'webpack-zepto'

function setupDefaultTimeSeries(element) {
    let margin = {
          top: 40,
          right: 40,
          bottom: 40,
          left: 80
      };
      
      let el = d3.select(element)
        , svg = el.select('svg.d3-attach')
        // , yearFormat = d3.timeFormat('%Y').parse
        , _this = this
        , width = parseInt(svg.style("width")) - margin.left - margin.right
        , height = parseInt(svg.style("height")) - margin.top - margin.bottom
        , x = d3.scaleTime()
        , y = d3.scaleLinear();

    // x.range([0, width]);
    // y.range([height, 0]);
    // 
    // voronoi = d3.geom.voronoi()
    //     .x(function(d) {
    //         return x(d.date);
    //     })
    //     .y(function(d) {
    //         return y(d.v);
    //     })
    //     .clipExtent([
    //         [-margin.left, -margin.top],
    //         [width + margin.right, height + margin.bottom]
    //     ]);

    // move?
    // var tooltipElem = d3.select(element).select(".tooltip");
}

function updateDefaultTimeSeries(element, data) {
    console.log(element, data)

  
  /// Update
  // var _this = this
  //     , data = this.getAttr('data')
  //     , feature = this.getAttr('feature')
  //     , indicator = this.getAttr('indicator')
  //     , transform = this.get('bailiwick.transformType')
  //     , areaName = this.getAttr('area').get('name')
  //     , areaType = this.get('bailiwick.areaType')
  //     , compareAreaName = this.get('bailiwick.compareArea.name')
  //     , hover = this.get("hover")
  //     , areaLookup = this.getAttr("areaLookup")
  //     , caption = this.getAttr("caption")
  //     , formatter = caption.get("formatter")
  //     , yearEndMonth = indicator.get("yearEndMonth")
  //     , yearCaption = yearEndMonth ? "Year to " + yearEndMonth : "Year"
  //     , bailiwick = this.get('bailiwick')
  //     ;
  // 
  //   this.$().addClass("svg-loading");
  // 
  //   if (!(data && svg)) {
  //     this.$().removeClass("svg-loading");
  //     return;
  //   }
  // 
  //   svg.selectAll('g').remove();
  //   var g = svg.selectAll('g').data([data])
  //     , gEnter = g.enter()
  //     .append("g")
  //     .attr("transform", "translate(" + margin.left + "," + margin.top + ")")
  //     ;
  // 
  //   var years = data.reduce(function(p, d) {
  //     return _.union(p, d.values.map(function(y) {
  //       return y[0];
  //     }));
  //   }, []).sort(function(a, b) {
  //     return d3.ascending(a, b);
  //   }).map(function(y) {
  //     return yearFormat(y);
  //   });
  // 
  //   var transformPos = transforms.indexOf(transform);
  //   var pos = transformPos === -1 ? 1 : transformPos + 1;
  //   var dispPos = transformPos === -1 ? 4 : transformPos + 4;
  //   var areas = data.map(function(a) {
  //     var area = { slug: a.slug,
  //       name: a.name,
  //       dsArea: areaLookup[a.name],
  //       values: a.values.map(function(y) {
  //         y.date = yearFormat(y[0]);
  //         y.v = y[pos];
  //         y.d = y[dispPos];
  //         return y;
  //     }).filter(function(d) {
  //         return d.v !== null;
  //       })
  //     };
  // 
  //     area.values.forEach(function(o) {
  //       o.area = area;
  //     });
  //     return area;
  //   }).filter(function(a) {
  //       var valid = false;
  // 
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
  // 
  //       var level = a.dsArea.get('level');
  // 
  //       return level === areaType || valid;
  //   });
  // 
  //   var xExtent = d3.extent(years);
  // 
  //   x.domain(xExtent);
  // 
  //   let yExtent = d3.extent(d3.merge(areas.map(function(a) {
  //     return a.values.map(function(d) {
  //       return d.v;
  //     });
  //   })));
  // 
  //   if ( this.get("zoomedIn") ) {
  //       let areaVals = findAreaBounds(areas, areaName);
  // 
  //       if ( typeof compareAreaName != 'undefined' ) {
  //           let compareVals = findAreaBounds(areas, compareAreaName);
  // 
  //           yExtent = [ Math.min (...[areaVals[0], compareVals[0]])
  //                           ,  Math.max (...[areaVals[1], compareVals[1]]) ];
  //       } else {
  //           yExtent = areaVals;
  //       }
  // 
  //       let diff = Math.abs ( yExtent[1] - yExtent[0] );
  // 
  //       yExtent[0] -= (.1 * diff);
  //       yExtent[1] += (.1 * diff);
  //   }
  // 
  //   y.domain(yExtent).nice();
  // 
  // 
  //   var tickFreq = Math.trunc(years.length / 9) + 1,
  //   ticks = years.filter(function(t, i) {
  //     return (i % tickFreq) === 0;
  //   });
  // 
  //   if (window.innerWidth < 600) {
  //     ticks = d3.extent(years);
  //   }
  // 
  // 
  //   /*
  //    * Generate the vertical line for the selected year.
  //    * */
  //    var xAxisYears = d3.svg.axis()
  //                    .scale(x)
  //                    .tickValues(years)
  //                    .orient("bottom");
  // 
  //    gEnter.append("g")
  //      .attr("class", "axis axis--x-hidden")
  //      .attr("transform", "translate(0," + height + ")")
  //      .call(xAxisYears);
  // 
  //    var allTicks = svg.call(xAxisYears).selectAll(".tick");
  // 
  //    // add year line indicator
  //    let year = this.get('bailiwick').get('year').get('name'),
  //        lineXpos = 0,
  //        previousYear = (new Date(years[0])).getFullYear();
  // 
  //    allTicks.each(function(data, i) {
  //        var tick = d3.select(this),
  //            date = new Date(data),
  //            fullYear = date.getFullYear(),
  //            targetYear = parseInt(year),
  //            transform = d3.transform(tick.attr("transform")).translate;
  // 
  //        if ( targetYear === fullYear ) {
  //            lineXpos = transform[0];
  //        }
  //    });
  // 
  //    if ( lineXpos <= 0 ) {
  //        lineXpos = 1;
  //    }
  // 
  // 
  //    gEnter.append("line")
  //        .attr("x1", lineXpos)
  //        .attr("y1", 0)
  //        .attr("x2", lineXpos)
  //        .attr("y2", clipHeight)
  //        .attr("stroke-width", 1)
  //        .attr("stroke", "rgba(0,0,0,.5)")
  //        .attr("z", 100)
  //        .attr("class","year-line");
  // 
  //   /*
  //    * End year line generation.
  //    * */
  // 
  //   var xAxis = d3.svg.axis()
  //                   .scale(x)
  //                   .tickValues(ticks)
  //                   .orient("bottom");
  // 
  // 
  // 
  //   gEnter.append("g")
  //     .attr("class", "axis axis--x")
  //     .attr("transform", "translate(0," + height + ")")
  //     .attr("z", 100)
  //     .call(xAxis)
  //     .append("text")
  //     .attr("class", "caption")
  //     .attr("y", 5)
  //     .attr("x", width + 10)
  //     .text("Year");
  // 
  //   gEnter.append("g")
  //     .attr("class", "axis axis--y")
  //     .call(d3.svg.axis()
  //         .scale(y)
  //         .orient("left")
  //         .ticks(10)
  //         .tickFormat(function(d) { return formatter(d); }))
  //     .append("text")
  //     .attr("class", "caption title")
  //     .attr("y", -30)
  //     .attr("x", 0)
  //     .text(bailiwick.label(transform));
  // 
  //     gEnter.append("clipPath")
  //         .attr("id", "clipper")
  //         .append("rect")
  //         .attr("x", 0)
  //         .attr("y", 0)
  //         .attr("width", clipWidth)
  //         .attr("height", clipHeight);
  // 
  //   svg.selectAll(".axis--x .tick")
  //       .on("click", function(d) {
  //           let year = (new Date(d)).getFullYear() + '';
  // 
  //           let filter = _this.get('bailiwick.indicator').get('years').filter(function(y) {
  //               return y.get('name') === year;
  //           });
  // 
  //           if (filter.length === 1) {
  //             _this.set('bailiwick.year', filter[0]);
  //           }
  // 
  //           _this.transitionTo({'year': filter[0], 'area': d.area.dsArea});
  //       });
  // 
  // 
  //   var path = gEnter.append("g")
  //     .attr("class", "areas")
  //     .selectAll("path")
  //     .data(areas);
  //   path.enter()
  //     .append("path");
  // 
  //   path.attr("d", function(d) { d.line = this; return line(d.values); });
  // 
  //   // Data has not actually changed - no path is empty
  //   // but need need to update highlighing so grab everything.
  //   g.selectAll('g.areas').selectAll('path')
  //     .attr("class", function(d) {
  //       if (d.name === areaName) {
  //         return "current-area";
  //       } else if (d.name === compareAreaName) {
  //         return "compare-area";
  //       } else if ( d.name === hover) {
  //         return "area--hover";
  //       } else if (d.name === "New Zealand") {
  //         return "new-zealand";
  //       }
  //       return "no-highlight";
  //     }).attr("clip-path", "url(#clipper)");
  // 
  //   var focusElemEnter = gEnter.append("g")
  //     .attr("transform", "translate(-100,-100)")
  //     .attr("class", "focus");
  // 
  //   focusElemEnter.append("circle")
  //     .attr("r", 3.5);
  //   focusElemEnter.append("text")
  //     .attr("y", -10);
  // 
  //   focusElem = g.selectAll('g.focus');
  // 
  //   var voronoiGroup = gEnter.append("g")
  //     .attr("class", "voronoi")
  //     .attr("clip-path", "url(#clipper)");
  // 
  //   var vg = voronoiGroup.selectAll("path")
  //     .data(voronoi(d3.nest()
  //           .key(function(d) { return x(d.date) + "," + y(d.v); })
  //           .rollup(function(v) { return v[0]; })
  //           .entries(d3.merge(areas.map(function(d) { return d.values; })))
  //           .map(function(d, i) { return d.values; })))
  //     .enter().append("path")
  //     .attr("d", function(d) {
  //       return present(d) ? "M" + d.join("L") + "Z" : "";
  //     })
  //   .datum(function(d) { return present(d) ? d.point : null; })
  //     .on("click", function(d) {
  //       let filter = _this.get('bailiwick.indicator').get('years').filter(function(y) {
  //           return y.get('name') === d[0];
  //       });
  // 
  //       if (filter.length === 1) {
  //         _this.set('bailiwick.year', filter[0]);
  //       }
  // 
  //       _this.transitionTo({'year': filter[0], 'area': d.area.dsArea});
  //     });
  // 
  //   if (!Modernizr.touch) {
  //     vg.on("mouseover", mouseover)
  //       .on("mouseout", mouseout)
  // 
  //   }
  //   var legendClasses = ["active", "other"];
  //   var legendLabels = ["New Zealand", "Other"];
  //   if (present(areaName) && areaName !== "New Zealand") {
  //     legendLabels.push(areaName);
  //     legendClasses = ["nz", "other", "active"];
  //   }
  //   if (present(compareAreaName) && compareAreaName !== "New Zealand" && compareAreaName !== areaName) {
  //     legendLabels.push(compareAreaName);
  //     legendClasses.push('compare');
  //   }
  //   var legendData = d3.zip(legendLabels, legendClasses);
  // 
  //   var legendWidth = this.get("legendWidth");
  //   var legendHeight = this.get("legendHeight");
  // 
  //   var legend = legendDiv.selectAll("svg").data([legendData]);
  //   var legendEnter = legend.enter().append("svg");
  //   legend.attr("width", legendWidth)
  //     .attr("height", legendHeight);
  // 
  //   var legendG = legend.selectAll("g.key").data([legendData]);
  //   var legendGEnter = legendG
  //     .enter()
  //     .append("g")
  //     .attr("class", "key");
  //   legendG
  //     .attr("transform", "translate(" + (window.innerWidth < 350 ? 20 : margin.left) +  "," + legendHeight / 3 + ")");
  // 
  //   var legendRects = legendG.selectAll("rect").data(legendData);
  //   var legendRectsEnter = legendRects
  //     .enter()
  //     .append("rect")
  //     .attr("height", 8)
  //     .attr("width", 55);
  //   legendRects
  //     .attr("x", function(d, i) {
  //       return Math.floor(i/2) * 140;
  //     })
  //     .attr("y", function(d, i) {
  //       return (i%2) * 20;
  //     })
  //     .attr("class", function(d) {
  //       return d[1];
  //     });
  //   legendRects.exit().remove();
  // 
  //   var legendTexts = legendG.selectAll("text").data(legendData);
  //   var legendTextsEnter = legendTexts
  //     .enter()
  //     .append("text")
  //     .attr("dx", "65px")
  //     .attr("dy", "0.7em");
  //   legendTexts
  //     .attr("x", function(d, i) {
  //       return Math.floor(i/2) * 140;
  //     })
  //     .attr("y", function(d, i) {
  //       return (i%2) * 20;
  //     })
  //   .text(function(d) {
  //     return d[0];
  //   });
  //   legendTexts.exit().remove();
  //   this.$().removeClass("svg-loading");
}

export {
    setupDefaultTimeSeries,
    updateDefaultTimeSeries
}