/**
 * Time series used on an indicator
 * 
 **/
import d3 from 'd3'
import _ from 'lodash'

import { none, isEmpty, present } from '../utils/utils';
import chartSetup from '../utils/chart-setup';
import formatting from '../utils/formatting';

let yearFormat = d3.time.format('%Y').parse;
let transforms = ["absolute", "indexed", "percapita"];
let x = d3.time.scale();

let y = d3.scale.linear();

let clipHeight = 393;
let clipWidth = 363;
let margin = { top: 40, right: 40, bottom: 40, left: 80 };

let line = d3.svg.line()
    .x(function (d, i) { return x(d.date); })
    .y(function (d) { return y(d.v); });

// data: [{year, rawNum, indexNum, headlineDisp, indexDisp}]
// @params: (data, current year, current indicator, transform, current area, current area type, chart ID)
export default function (element, params, feature) {
    let setup = chartSetup(element, params, margin, 'default-timeseries');

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
        svg = setup.svg,
        base = setup.base,
        width = setup.width, 
        height = setup.height;

    // current data...
    var currentYear = svg.attr('data-year'),
        currentIndicator = svg.attr('data-indicator'),
        currentArea = svg.attr('data-area'),
        currentTransform = svg.attr('data-transform'),
        currentLevel = svg.attr('data-level');

    var legendDiv = d3.select(element).select('.legend');
    var legendWidth = window.innerWidth < 350 ? 320 : 420;
    var legendHeight = 50;
    var tooltipElem = d3.select(element).select(".tooltip");

    /// Setup
    x.range([0, width]);
    y.range([height, 0]);

    let voronoi = d3.geom.voronoi()
        .x(function (d) { return x(d.date); })
        .y(function (d) { return y(d.v); })
        .clipExtent([[-margin.left, -margin.top], [width + margin.right, height + margin.bottom]]);

    /// Update
    svg.selectAll('g').remove();
    var g = svg.selectAll('g').data([data])
        , gEnter = svg.select('.g-enter');

    if (gEnter.empty()) {
        gEnter = g.enter().append("g").attr('class', 'g-enter');
    }

    gEnter
        .attr("transform", "translate(" + margin.left + "," + margin.top + ")");

    var years = [];

    if (!_.hasIn(cache.get(indicator), "years")) {
        years = _.uniq(_.reduce(data, function (result, value, key) {
            return _.reduce(value[1], function (res, v, k) {
                res.push(v[0]);
                return res;
            }, result);
        }, [])).sort(function (a, b) {
            return d3.ascending(a, b);
        }).map(function (y) {
            return yearFormat(y.toString());
        });

        toCache.years = years;
    } else {
        years = cache.get(indicator).years;
    }

    var areaKey = area + '-' + areaLevel + '-' + transform;
    var areas = [];

    if (!_.hasIn(cache.get(indicator), area) || (
        _.hasIn(cache.get(indicator), area) && 
        !_.hasIn(cache.get(indicator.area), areaKey)
    )) {
        var transformPos = transforms.indexOf(transform);
        var pos = transformPos === -1 ? 1 : transformPos + 1;
        var dispPos = transformPos === -1 ? 3 : transformPos + 3;

        areas = data.map(function (a) {
            var area = {
                slug: a[0][0],
                name: a[0][1],
                level: a[0][2],
                dsArea: a[0],
                values: a[1].map(function (y) {
                    var out = {};
                    out.date = yearFormat(y[0].toString());
                    out.v = Number(y[pos]);
                    out.d = y[dispPos];
                    return out;
                }).filter(function (d) {
                    return d.v !== null;
                })
            };

            // Used on mouse events on voroni
            area.values.forEach(function (o) {
                o.area = area;
            });

            return area;
        })
        .filter(function (a) {
            var valid = false;

            switch (a.name) {
                //   case compareAreaName:
                //       valid = true;
                //       break;
                case area:
                    valid = true;
                    break;
                case 'New Zealand':
                    valid = true;
                    break;
                default:
                    break;
            }

            return a.level === areaLevel || valid;
        });

        toCache[String(areaKey)] = areas;
    } else {
        areas = cache.get(indicator).areas[areaKey];
    }

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

    var xAxisHidden = gEnter.select('.axis--x--hidden');

    if (xAxisHidden.empty()) {
        xAxisHidden = gEnter.append("g")
            .attr("class", "axis axis--x-hidden")
            .attr("transform", "translate(0," + height + ")")
    }

    xAxisHidden.call(xAxisYears);

    var allTicks = svg.call(xAxisYears).selectAll(".tick"),
        lineXpos = 0;

    allTicks.each(function (data, i) {
            var tick = d3.select(this),
                date = new Date(data),
                fullYear = date.getFullYear(),
                targetYear = parseInt(year),
                transform = d3.transform(tick.attr("transform")).translate;

            if (targetYear === fullYear) {
                lineXpos = transform[0];
            }
        });

    if (lineXpos <= 0) {
        lineXpos = 1;
    }

    var yearLine = gEnter.select(".year-line");

    if (yearLine.empty()) {
        yearLine = gEnter.append('line');
    }

    yearLine
        .transition()
        .duration(500)
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


    var xAxisReal = gEnter.select('.axis--x');

    if (xAxisReal.empty()) {
        xAxisReal = gEnter.append("g")
        .attr("class", "axis axis--x")
        .attr("transform", "translate(0," + height + ")")
        .attr("z", 100)
    }

    xAxisReal
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

    svg.append("clipPath")
        .attr("id", "clipper")
        .append("rect")
        .attr("x", 0)
        .attr("y", 0)
        .attr("width", clipWidth)
        .attr("height", clipHeight);

    svg.selectAll(".axis--x g.tick text")
        .attr("data-bailiwick-year", function(d) {
            return (new Date(d)).getFullYear();
        });
        // .on("click", function (d) {
        //     d3.event.preventDefault();
        //     var year = (new Date(d)).getFullYear();
        //     console.log('year clicked', year, this);
        // });

    var path = gEnter.select('.areas').selectAll("path");

    if (path.empty()) {
        path = gEnter.append("g")
            .attr("class", "areas")
            .selectAll("path");
    }

    path = path.data(areas);

    path.enter()
        .append("path");

    path.attr("d", function (d) {
        d.line = this;
        return line(d.values);
    });

    // Data has not actually changed - no path is empty
    // but need need to update highlighing so grab everything.
    g.selectAll('g.areas').selectAll('path')
        .attr("class", function (d) {
            var classNames = {
                'New Zealand': 'new-zealand',
                'default': 'no-highlight',
            };
            classNames[area] = 'current-area';
            return (
                _.hasIn(classNames, d.name) ? 
                    classNames[d.name] : 
                    classNames['default']
            ) + ' area';
            // } else if (d.name === compareAreaName) {
            //     return "compare-area";
            // } else if (d.name === hover) {
            //     return "area--hover";
        }).attr("clip-path", "url(#clipper)");

    var focusElemEnter = gEnter.append("g")
        .attr("transform", "translate(-100,-100)")
        .attr("class", "focus");

    focusElemEnter.append("circle")
        .attr("r", 3.5);
    focusElemEnter.append("text")
        .attr("y", -10);

    var focusElem = g.selectAll('g.focus');

    var voronoiGroup = gEnter.append("g")
        .attr("class", "voronoi")
        .attr("clip-path", "url(#clipper)");

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
            return !present(d) ? "M" + d.join("L") + "Z" : "";
        })
        .attr("data-bailiwick-year", function(d) {
            return !present(d) ? (new Date(d['point'].date)).getFullYear() : "";
        })
        .attr("data-bailiwick-area", function(d) {
            return !present(d) ? d['point'].area.slug : "";
        })
        .datum(function (d) {
            return !present(d) ? d.point : null;
        })
        .on("click", function (d) {
            var year = (new Date(d.date)).getFullYear(),
                area = d.area.slug;
            // console.log(year, d);
            // console.log(d3.event)
            // let filter = _this.get('bailiwick.indicator').get('years').filter(function (y) {
            //     return y.get('name') === d[0];
            // });

            // if (filter.length === 1) {
            //     _this.set('bailiwick.year', filter[0]);
            // }

            // _this.transitionTo({ 'year': filter[0], 'area': d.area.dsArea });
        });

    if (!Modernizr.touch) {
        vg.on('mouseover', function mouseover(d, i) {
            if (none(d.area)) {
                return;
            }
            var xPos = x(d.date),
                yPos = y(d.v);
            d3.select(d.area.line).classed("area--hover", true);
            d.area.line.parentNode.appendChild(d.area.line);
    
    
            focusElem.attr("transform", "translate(" + xPos + "," + yPos + ")")
                .style("visibility", "visible");
            tooltipElem
                .style("top", (yPos - 90) + "px")
                .style("left", (xPos) + "px")
                .style("visibility", "inherit");
    
            var tooltipData = [d.area.name, d.d, d.date.getFullYear()],
                tooltip = tooltipElem.selectAll('p').data(tooltipData),
                tooltipEnter = tooltip.enter().append('p');
    
            tooltip.html(function (d) {
                return d;
            }).classed("number", function (d, i) {
                return i === 1;
            }).classed("local", function (d, i) {
                return i !== 0;
            }).classed("extra", function (d, i) {
                return i > 1;
            });
            tooltip.exit().remove();
        });
    
        vg.on('mouseout', function (d) {
            if (none(d.area)) {
                return;
            }
    
            d3.select(d.area.line).classed("area--hover", false);
            focusElem.attr("transform", "translate(-100,-100)").style("visibility", "hidden");
            tooltipElem.style("visibility", "hidden");
        });
    }

    // update data attributes.
    svg
      .attr('data-year', year)
      .attr('data-area', area)
      .attr('data-transform', transform)
      .attr('data-level', areaLevel)
      .attr('data-indicator', indicator);
    
    

    // ----
    // Only update the legend if the area has changed.
    // ----
    if (currentArea === area) {
        base.classed('svg-loading', false);
        return false;
    }

    var legendClasses = ["active", "other"];
    var legendLabels = ["New Zealand", "Other"];
    if (area !== "New Zealand") {
        legendLabels.push(area);
        legendClasses = ["nz", "other", "active"];
    }
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

    /// Update cache if necessary
    if (!_.isEmpty(toCache)) {
        let cachedIndicator = cache.get(indicator);

        // 1. Years
        if (!_.hasIn(cachedIndicator, "years") && _.hasIn(toCache, "years")) {
            cachedIndicator.years = toCache.years;
        }

        // 2. Areas
        if (_.hasIn(toCache, areaKey)) {
            let areasToCache = {};

            if (_.hasIn(cachedIndicator, "areas")) {
                areasToCache = cachedIndicator.areas;
            }

            areasToCache[areaKey] = toCache[areaKey];
            cachedIndicator.areas = areasToCache;
        }
    }

    base.classed('svg-loading', false);
}