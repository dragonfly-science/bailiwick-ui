// Math.trunc = Math.trunc || function(x) {
//     return x < 0 ? Math.ceil(x) : Math.floor(x);
// };
// var none = function(obj) {
//         console.warn('function moved to utils module');
//         return obj === null || obj === undefined;
//     },
//     isEmpty = function(obj) {
//         console.warn('function moved to utils module');
//         return _.isEmpty(obj);
//     },
//     present = function(obj) {
//         console.warn('function moved to utils module');
//         return isEmpty(obj) || (typeof obj === 'string' && /\S/.test(obj) === false);
//     };

// get colours for mapping 
// var getColours = function() {
//     console.warn('get colours function moved to utils module.');
//     var colors = document.getElementsByClassName("colour-palette");
//     var colorChildren = colors[0].getElementsByClassName("colour");

//     var output = {};

//     _.each(colorChildren, function(value, i) {
//         var classNames = value.className,
//             el = document.getElementsByClassName(classNames),
//             bg = window.getComputedStyle( el[0] ,null).getPropertyValue('background-color');
//         // console.log('hi', el);
//         output[classNames.split(' ')[0]] = bg;
//     });

//     return output;

//     // .each(function(i, el) {
//     //     console.log(i, el)
//     //     // let classNames = $(el).attr('class');
//     //     // self.set(classNames.split(' ')[0], $(el).css('background-color'));
//     // });
// };

// var textSubstitution = function(s, bw, addCompareArea) {
//     var y = bw.get('year.name'),
//         fy = bw.get('indicator.firstYear.name'),
//         yem = bw.get('indicator.yearEndMonth'),
//         su = bw.get('indicator.subject'),
//         a = bw.get('area.name'),
//         sa = bw.get('area.name'),
//         f = bw.get('feature.name'),
//         fl = f || bw.get('indicator.topFeatureLabel'),
//         fp = f ? bw.get('feature.parent') : "",
//         d = bw.get('detail.name'),
//         dl = d || bw.get('indicator.topDetailLabel'),
//         ft = bw.get('indicator.featureText'),
//         p = bw.get("prevYear"),
//         ca = bw.get("compareArea.name");
//     if (present(addCompareArea) && present(ca)) {
//         a = "<span class='active'>" + a + "</span><span class='compare'> (and " + ca + ")</span>";
//     }
//     if (ft) {
//         var fs = bw.get('feature.id');
//         fl = ft[fs];
//     }
//     if (f) {
//         s = s.replace(/[\[\]]/g, '');
//     } else {
//         s = s.replace(/\[.+?\]/g, '');
//     }
//     if (d) {
//         s = s.replace(/[\{\}]/g, '');
//     } else {
//         s = s.replace(/\s?\{.+?\}/g, '');
//     }
//     s = s.replace(/\$year\$/g, y)
//         .replace(/\$firstYear\$/g, fy)
//         .replace(/\$yearEndMonth\$/g, yem)
//         .replace(/\$subject\$/g, su)
//         .replace(/\$area\$/g, a)
//         .replace(/\$selectedArea\$/g, sa)
//         .replace(/\$compareArea\$/g, ca)
//         .replace(/\$prevYear\$/g, p)
//         .replace(/\$feature\$/g, fl)
//         .replace(/\$featureType\$/g, fp)
//         .replace(/\$detail\$/g, dl);
//     return s.trim();
// }

// var label = function(transform) {
//     if (transforms.indexOf(transform) === -1) {
//         if (transform === "absolute") {
//             transform = "original";
//         } else if (transform === "regionalPercentage") {
//             transform = "feature-percentage";
//         } else if (transform === "rate") {
//             transform = "annual-rate";
//         } else {
//             return "";
//         }
//     }

//     // var l = textSubstitution(this.get("indicator.labels")[transform], this);
//     // return l[0].toUpperCase() + l.slice(1);
// };

/**
 * Update the time series on the home page side bar - housePriceTimeSeries
 **/
// var updateTimeSeries = function(element, labelledData, activeLabelName) {
//     var base = d3.select(element).select('.d3-attach'),
//         svg = base.select('svg').empty() ? base.append('svg') : base.select('svg'),
//         yearFormat = d3.time.format('%Y').parse,
//         margin = {
//             top: 15,
//             right: 0,
//             bottom: 30,
//             left: 48
//         },
//         baseW = 225,
//         baseH = 120,
//         width = baseW - margin.left - margin.right,
//         height = baseH - margin.top - margin.bottom,
//         _this = this;

//     svg.attr('width', baseW);
//     svg.attr('height', baseH);

//     var data = labelledData.map(function(d) {
//         return d[1];
//     });

//     var x = d3.time.scale()
//         .range([0, width]);

//     var y = d3.scale.linear()
//         .range([height, 0]);

//     var xAxis = d3.svg.axis()
//         .scale(x)
//         .ticks(5)
//         .innerTickSize(3)
//         .outerTickSize(0)
//         .tickFormat(function(t) {
//             return t.getFullYear().toString().replace(/^20/, "'");
//         })
//         .orient("bottom");

//     var yAxis = d3.svg.axis()
//         .scale(y)
//         .ticks([4])
//         .outerTickSize(0)
//         .innerTickSize(-width)
//         .tickFormat(function(t) {
//             return '$' + t / 1000 + ' k';
//         })
//         .orient("left");

//     var line = d3.svg.line()
//         .x(function(d) {
//             return x(yearFormat(d[0].toString()));
//         })
//         .y(function(d) {
//             return y(d[1]);
//         });

//     var g = svg.selectAll('g').data([data]);
//     var gEnter = g.enter()
//         .append("g")
//         .attr("transform", "translate(" + margin.left + "," + margin.top + ")");

//     var yMax = Math.ceil(d3.max(d3.merge(data), function(d) {
//         return d[1];
//     }) / 100000) * 100000;
//     x.domain(d3.extent(data[0], function(d) {
//         return yearFormat(d[0].toString());
//     }));
//     y.domain([0, yMax]);

//     gEnter.append("g")
//         .attr("class", "x axis")
//         .attr("transform", "translate(0," + height + ")")
//         .append("text")
//         .attr("x", 50)
//         .attr("dy", "2.5em")
//         .text("Year");

//     g.selectAll("g.x")
//         .call(xAxis);

//     gEnter.append("g")
//         .attr("class", "y axis")
//         .attr("transform", "translate(-7,0)");

//     g.selectAll("g.y")
//         .call(yAxis);

//     var linePlot = g.selectAll("path.line").data(labelledData);
//     linePlot.enter()
//         .append("path");

//     linePlot.attr("d", function(d) {
//         return line(d[1]);
//     }).attr("class", function(d, i) {
//         if (activeLabelName === d[0]) {
//             return "line active";
//         }
//         return "line";
//     });

//     linePlot.exit().remove();
// }

/**
 * Legend for the map
 **/
// var computeTicks = function(extent) {
//     console.warn('compute ticks function moved to module');

//     var nice = d3.scale.linear().domain(extent).nice().ticks();
//     if (nice.length < 9) {
//       return nice;
//     }
//     else if (nice.length % 2 === 0) {
//       let diff = nice[nice.length - 1] - nice[nice.length - 2];
//       nice.push(nice[nice.length - 1] + diff);
//     }
    
//     return nice.filter(function(d, i) {
//       return i % 2 === 0;
//     });
//   }
// var updateMapLegend = function(width, height, scaledata) {
//     // console.log('legend')
//     // When we have comparision data, let's switch the scales.

//     // if (none(this.get('bailiwick.compareArea'))) {
//     //     this.defaultScale();
//     //   } else {
//     //     this.compareScale();
//     //   }
//     var base = d3.select(".indicator-map-legend");
//     var svg = base.select('svg').empty() ? base.append('svg') : base.select('svg');

//     var colors = getColours();
//     // var positive = d3.scale([colors['background-rear-positive-light'], colors['background-rear-positive']]),
//     //     negative = d3.scale()
//     //             .domain([-1, 0])
//     //             .range([colors['background-rear-negative'], colors['background-rear-negative-light']]),
//     //     zero = d3.scale([colors['background-rear-positive-light'], colors['background-rear-negative-light']]);
    
//     // console.log(positive, negative, zero);

//     //
//     // Default Scale.
//     //
//     var scaleType = 'diverging';
//     var vals = d3.values(scaledata);
//     var extent = d3.extent(vals, function(v) {
//         return v[0];
//     });
//     // var caption = this.getAttr("caption").get("text");
//     // var formatter = this.getAttr("caption").get("formatter");
//     var scale = d3.scale.linear().domain(extent).range([0,1]),
//         scaleF = function(v) {return scale(v);};
//         thresholdBase = scale.ticks(7),
//         threshold = d3.scale.threshold()
//          .domain(thresholdBase)
//          .range(thresholdBase.map(function(t) {
//            return scaleF(t);
//          })),
//         linear = d3.scale.linear()
//          .domain(thresholdBase)
//          .range(thresholdBase.map(function(t) {
//            return scaleF(t);
//          }));

//     if (
//         scaleType === "diverging" ||
//         (scaleType !== "sequential" && extent[0] * extent[1] < 0)
//     ) {
//         var max = Math.max(Math.abs(extent[0]), Math.abs(extent[1]));

//         scale = d3.scale.linear().domain([-1 * max, 0, max]).range([-1, 0, 1]),
//         thresholdBase = computeTicks(extent);
//         // console.log('chroma', chroma)

//         // scaleF = function(v) {
//         //     var s = scale(v);
//         //     if (s < 0) {
//         //     return negative(s);
//         //     } else if (s > 0) {
//         //     return positive(s);
//         //     }
//         //     return zero;
//         // };

//         // threshold = d3.scale.threshold()
//         //     .domain(thresholdBase)
//         //     .range(thresholdBase.map(function(t) {
//         //         return scaleF(t);
//         //     }));
//         // linear = d3.scale.linear()
//         //     .domain(thresholdBase)
//         //     .range(thresholdBase.map(function(t) {
//         //         return scaleF(t);
//         //     }));
//     }

//     //
//     // End Default scale
//     //
//     var domain = linear.domain();
//     extent = d3.extent(domain);
//     var step = (extent[1] - extent[0]) / 100;
//     var widthRange = window.innerWidth > 460 ? 380 : window.innerWidth - 120;

//     // A position encoding for the key only.
//     var x = d3.scale.linear()
//       .domain(extent)
//       .range([0, widthRange]);

//     var xAxis = d3.svg.axis()
//       .scale(x)
//       .orient("bottom")
//       .tickSize(13)
//       .tickValues(window.innerWidth < 500 ? extent : domain);
//     //   .tickFormat(function(d) {
//     //     return d;
//     //   });
    
//     svg.empty();
//     svg.data([scaledata])
//       .attr("width", width)
//       .attr("height", height);
//     step = (width-100)/100;

//     var sd = Array.from(Array(100).keys(),
//                  i => [i*step, scaledata[i][2]]);
    
//     svg.selectAll('g').remove();

//     var g = svg.selectAll("g")
//       .data([sd])
//       .enter()
//       .append("g")
//       .attr("class", "key")
//       .attr("transform", "translate(50," + height * 1 / 3 + ")");

//     var legend = g.selectAll("rect")
//       .data(sd)
//       .enter()
//       .append("rect")
//       .attr("height", 8)
//       .attr("x", function(d) { return d[0] % innerWidth; })
//       .attr("width", step)
//       .style("fill", function(d) { return d[1]; })
//       .style("stroke", function(d) { return d[1]; });

//     g.selectAll(".caption").remove();
//     let xa = g.call(xAxis);
//     xa.append("text")
//     .attr("class", "caption")
//     .attr("y", -6)
//     .text("caption goes here");
// }

/**
 * Initialise a bar chart - used on the indicator.
 **/
// var updateAreaBarchart = function(element, data) {
//     console.log('updateAreaBarchart', element, data);
//     // var container = d3.select(element).select('svg.d3-attach')
//     //   , tooltipElem = d3.select(element).select(".tooltip")
//     //   , margin = {
//     //       top: 5,
//     //       right: 25,
//     //       bottom: 40,
//     //       left: 140
//     //     };
//     //
//     // width = parseInt(container.style("width")) - margin.left - margin.right;
//     // height = parseInt(container.style("height")) - margin.top - margin.bottom;
// }




