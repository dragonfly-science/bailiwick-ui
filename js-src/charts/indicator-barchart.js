import _ from 'lodash';
import d3 from 'd3';

import { isEmpty } from '../utils/utils';
import chartSetup from '../utils/chart-setup';
import format from '../utils/formatting';

export default function (element, params) {
    //
    // Set up
    //
    let lmargin = 240;
    if (window.innerWidth < 400) {
      lmargin = 100;
    } else if (window.innerWidth < 600 && lmargin > 140) {
      lmargin = 180;
    }
    let margin = {top: 5, right: 25, bottom: 40, left: lmargin};

    let setup = chartSetup(element, params, margin, 'basic-barchart');

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
        chartCaption = setup.chartCaption,
        svg = setup.svg,
        base = setup.base,
        width = setup.width,
        height = setup.height;

    var legend = d3.select('.chart-inner .legend');
    legend.select('svg').remove();

    var tooltipElem;
    var y = d3.scale.ordinal();
    var x = d3.scale.linear();

    var maxLength = 35;

    if (window.innerWidth < 400) {
        maxLength = 11;
    } 
    else if (window.innerWidth < 600) {
        maxLength = 25;
    }

    var yAxis = d3.svg.axis()
        .scale(y)
        .orient("left")
        .outerTickSize(0)
        .tickFormat(function(d) {
            if (d === null) {
                return '';
            }

            if (d.length === 0) {
                return '';
            }

            let label = d;

            if (_.has(features, d)) {
                label = features[d];
            }
            
            label = label[0].toUpperCase() + label.slice(1);

            if (label.length > maxLength) {
                return label.substring(0, maxLength) + '…';
            }

            return label;
        });

    var barHeight = 20,
        barGap = 5;

    var xAxis = d3.svg.axis();


    // Find the current area in supplied data
    var currentAreaData = _.filter(data, function(o) {
        return o[0][1] === area;
    });

    if (isEmpty(currentAreaData)) {
        base.classed('svg-loading', false);
        return false;
    }

    currentAreaData = currentAreaData[0][0];
    let cacheKey = 'areas.' + area + '.' + year + '.' + feature + '-' + String(Math.random() * 100);

    // if (!_.hasIn(cache.get(indicator), cacheKey)) {
        let cachedInd = cache.get(indicator);
        let parents = currentAreaData[3];

        // No parents means NZ is selected - so we will end up showing all
        // available regions.
        if (
            (feature !== null && isEmpty(parents) && area !== 'New Zealand') ||
            (feature === null && isEmpty(parents))
        ) {
            parents = ['new-zealand'];
        }

        // Find all siblings that have one or more same parents.
        var siblingAreas = _.filter(data, function(o) {
            return _.intersection(o[0][3], parents).length > 0;
        });

        // This is assuming we are looking at NZ.
        if (_.isEmpty(siblingAreas)) {
            siblingAreas = data;
        }

        var siblingsFilteredByYear = _.reduce(siblingAreas, function(res, v, k) {
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

        ///
        /// If we have a feature, return area data sorted by feature (with
        /// keys renamed to appropriate values).
        if (feature !== null) {
            let areaData = _.groupBy(_.filter(siblingsFilteredByYear, function(o) {
                return o.name === area;
            }), 'feature');

            areaData = _.reduce(areaData, function(result, v, k) {
                let newData = {
                    name: v[0].feature,
                    slug: v[0].slug,
                    value: v[0].value,
                    year: v[0].year,
                    display: v[0].display
                }

                result.push(newData);

                return result;
            }, []);


            siblingsFilteredByYear = _.orderBy(areaData, 'value', 'desc');

            // current area data is now looking at the features.
            let newcurrentAreaData = _.filter(areaData, function(o) {
                return o.name === feature;
            });

            if (!_.isEmpty(newcurrentAreaData)) {
                currentAreaData[0] = newcurrentAreaData[0].name;
            }
        } else {
            siblingsFilteredByYear = _.sortBy(siblingsFilteredByYear, ['value']);
        }

        if (!_.hasIn(cachedInd, 'areas')) {
            cachedInd.areas = {};
        }

        if (!_.hasIn(cachedInd, 'areas.' + area)) {
            cachedInd.areas[area] = {};
        }
        cachedInd.areas[area][year] = siblingsFilteredByYear;
        data = siblingsFilteredByYear;
    // }

    // data = cache.get(indicator)['areas'][area][year];
    
    tooltipElem = d3.select(element).select(".tooltip");


    x = x.range([0, width]);

    let g = svg.selectAll("g").data([1]);
    var padding = barGap * (data.length + 1);
    var dataHeight = Math.min(barHeight * data.length + padding, height);
    var paddingRatio = padding / dataHeight;

    var svgEnter = g.enter().append("g");
    g
      .attr("width", width)
      .attr("height", height)
      .attr("transform", "translate(" + margin.left + "," + margin.top + ")");


    // Handle Units
    xAxis.scale(x)
        .orient("bottom")
        .ticks(window.innerWidth < 450 ? 2 : 5)
        .tickFormat(function (d) {
            if (chartData === null || chartData.length === 0) {
                return d;
            }
            
            var trans = _.filter(chartData.chartTransforms, function(i) {
                return i.transformName === transform;
            });

            var formatter = null; 

            if (trans.length > 0) {
                formatter = trans[0].transformFormatter;
            }
            
            return format(formatter, d);
        });
    xAxis.tickSize(-1 * dataHeight, 10)
    
    // x.domain([0, d3.max(data, function (d) { return d.value; })]);

    var xExtent = d3.extent(data, function(d) { return d.value; });
    x.domain([Math.min(0, xExtent[0]), Math.max(0, xExtent[1])]).nice();


    g.attr("height", dataHeight + margin.top + margin.bottom);
    y = y.rangeRoundBands([0, dataHeight], paddingRatio);
    y.domain(data.map(function (d) { return d.name; }));
    xAxis.tickSize(-1 * dataHeight, 10)

    var ySel = g.selectAll("g.y.axis").data([data]),
        ySelEnter = ySel.enter().append("g")
            .attr("class", "y axis");
    ySel
        .call(yAxis);
    ySel.exit().remove();

    var xSel = g.selectAll("g.x.axis").data([data]),
        xSelEnter = xSel.enter().append("g")
            .attr("class", "x axis");

    xSel
        .attr("transform", "translate(0," + (dataHeight + 3) + ")")
        .call(xAxis);
    xSel.exit().remove();

    var xSelCaption = svg.selectAll("text.caption").data([chartCaption])
        , xSelCaptionEnter = xSelCaption.enter()
            .append("text");
    xSelCaption
        .attr("class", "caption")
        .attr("transform", "translate(0," + (dataHeight + 50) + ")")
        .text(chartCaption);

    xSelCaption
        .attr("x", 
            base.node().getBoundingClientRect().width - 
            xSelCaption.node().getBBox().width - margin.right
        )

    var bar = g.selectAll(".bar").data(data),
        barEnter = bar.enter().append("rect")
            .attr("class", "bar")
            .attr("data-bailiwick-area", function (d) {
                return d.slug;
            })
            .attr("data-bailiwick-feature", function(d) {
                if (feature !== null) {
                    return d.name;
                }
                return '';
            });


    if (!Modernizr.touch) {
        barEnter
            .on("mouseover", function (d) {
                var name = d.name;

                if (_.has(features, d.name)) {
                    name = features[d.name];
                }

                name = name[0].toUpperCase() + name.slice(1);

                var tooltip = tooltipElem.selectAll('p').data([name, d.display, d.year]),
                    tooltipEnter = tooltip.enter().append('p');

                tooltip.text(function (d) {
                    return d;
                }).classed("number", function (d, i) {
                    return i === 1;
                }).classed("local", function (d, i) {
                    return i === 1;
                });

                tooltipElem.style("visibility", "visible")
                    .style("top", function () {
                        return (d3.event.offsetY) + "px";
                    })
                    .style("left", function () {
                        return (d3.event.offsetX) + "px";
                    });

            }).on("mouseout", function (d) {
                tooltipElem.style("visibility", "hidden");
            });
    }

    bar
        .attr("y", function (d) { return y(d.name); })
        .attr("height", Math.abs(y.rangeBand()))
        .attr("x", 1)
        .attr("width", function (d) { return x(d.value) + 1; })
        .classed("active", function (d) {
            if (feature !== null) {
                return d.name === currentAreaData[0];
            }
            return d.slug === currentAreaData[0];
        });

    bar.exit().remove();
    base.classed('svg-loading', false);
}
