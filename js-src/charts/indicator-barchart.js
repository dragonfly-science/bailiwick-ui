import d3 from 'd3';
import _ from 'lodash';
import chartSetup from '../utils/chart-setup';
import { isEmpty } from '../utils/utils';




export default function (element, params, feature) {
    //
    // Set up
    //
    // console.log(element, params, feature);
    let lmargin = 140;
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
        svg = setup.svg,
        base = setup.base,
        width = setup.width, 
        height = setup.height;

    var legend = d3.select('.chart-inner .legend');
    legend.select('svg').remove();

    var percentageFormatter = d3.format('.1f');
    var tooltipElem;
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


    // Find the current area in supplied data
    var currentAreaData = _.filter(data, function(o) {
        return o[0][1] === area;
    });

    if (isEmpty(currentAreaData)) {
        base.classed('svg-loading', false);
        return false;
    }

    currentAreaData = currentAreaData[0];
    let cacheKey = 'areas.' + area + '.' + year + '.' + feature + '-' + String(Math.random() * 100);

    if (!_.hasIn(cache.get(indicator), cacheKey)) {
        let cachedInd = cache.get(indicator);
        let parents = currentAreaData[0][3];

        // No parents means NZ is selected - so we will end up showing all
        // available regions.
        if (isEmpty(parents)) {
            parents = ['new-zealand'];
        }
        
        // Find all siblings that have one or more same parents.
        var siblingAreas = _.filter(data, function(o) {
            return _.intersection(o[0][3], parents).length > 0;
        });

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

            if (
                ((feature !== null && feature === values.feature) ||
                feature === null) &&
                !_.isEmpty(yearVal)
            ) {
                values.value = Number(yearVal[0][1]);
                values.display = (yearVal[0][3]).trim();
                res.push(values);
            }

            return res;
        }, []);

        siblingsFilteredByYear = _.sortBy(siblingsFilteredByYear, ['value']);
        
        if (!_.hasIn(cachedInd, 'areas')) {
            cachedInd.areas = {};
        }

        if (!_.hasIn(cachedInd, 'areas.' + area)) {
            cachedInd.areas[area] = {};
        }
        cachedInd.areas[area][year] = siblingsFilteredByYear;
        data = siblingsFilteredByYear;
    }

    data = cache.get(indicator)['areas'][area][year];

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
    // TODO: handle formatting
    // var formatter = this.getAttr("caption").get("formatter");
    xAxis.scale(x)
        .orient("bottom")
        .ticks(window.innerWidth < 450 ? 2 : 5)
        // .tickFormat(function (d) {
        //     return formatter(d);
        // });
    xAxis.tickSize(-1 * dataHeight, 10)
    // Set data
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


    // Update
    // var data = this.get('plotdata'),
    //     feature = this.getAttr('feature'),
    //     featureSlug = present(feature) ? feature.get("id") : null,
    //     _this = this;

    // var caption = this.getAttr("caption");
    // var data = siblingsFilteredByYear;
    // if (!(data && svg && data.length > 0 && present(data[0]))) {
    //     return;
    // }
    x.domain([0, d3.max(data, function (d) { return d.value; })]);

    
    g.attr("height", dataHeight + margin.top + margin.bottom);
    y = y.rangeRoundBands([0, dataHeight], paddingRatio);
    y.domain(data.map(function (d) { return d.name; }));
    xAxis.tickSize(-1 * dataHeight, 10)

    var ySel = g.selectAll("g.y.axis").data([data]),
        ySelEnter = ySel.enter().append("g")
            .attr("class", "y axis");
    ySel
        .call(yAxis)
        .selectAll("text")
        .style("text-anchor", "end");
    ySel.exit().remove();

    var xSel = g.selectAll("g.x.axis").data([data]),
        xSelEnter = xSel.enter().append("g")
            .attr("class", "x axis");
    xSel
        .attr("transform", "translate(0," + (dataHeight + 3) + ")")
        .call(xAxis);
    xSel.exit().remove();

    var xSelCaption = xSel.selectAll("text.caption").data(["Caption here..."])
        , xSelCaptionEnter = xSelCaption.enter()
            .append("text");
    xSelCaption
        .attr("class", "caption")
        .attr("y", 30)
        .attr("x", 0)
        .style("text-anchor", "start")
        .text("Caption here...");

    var bar = g.selectAll(".bar").data(data),
        barEnter = bar.enter().append("rect")
            .attr("class", "bar")
            .attr("data-bailiwick-area", function (d) {
                return d.slug;
            })
    if (!Modernizr.touch) {
        barEnter
            .on("mouseover", function (d) {
                var tooltip = tooltipElem.selectAll('p').data([d.name, d.display, d.year]),
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
        .attr("height", y.rangeBand())
        .attr("x", function (d) { return 1; })
        .attr("width", function (d) { return x(d.value) + 1; })
        .classed("active", function (d) {
            return d.slug === currentAreaData[0][0];
        });

    bar.exit().remove();

    base.classed('svg-loading', false);
}