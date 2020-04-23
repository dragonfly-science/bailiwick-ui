import d3 from 'd3'
import _ from 'lodash'

import { isEmpty } from '../utils/utils';

export default function(element, params, margin, chartType) {
    let base = d3.select(element),
        chartRect = d3.select(".indicator-chart")[0][0].getBoundingClientRect(),
        svg = null,
        width = parseInt(chartRect.width) - margin.left - margin.right,
        height = parseInt(base.style('height')) - margin.top - margin.bottom,
        data = params[0],
        chartInnerClasses = {
            'default-timeseries': false,
            'basic-barchart': false,
            'area-treemap': false,
            'overunder-barchart': false
        };


    chartInnerClasses = _.forEach(chartInnerClasses, function(value, key) {
        chartInnerClasses[key] = key === chartType;
    });

    svg = base.select('svg');
    base.classed('svg-loading', true);

    if (
        d3.select('.chart-inner').empty() ||
        (!d3.select('.chart-inner').empty() &&
        !d3.select('.chart-inner').classed(chartType))
        )
    {
        base.select('svg').remove();
        svg = base.append('svg');
    }

    // set chart specific classes.
    d3.select('.chart-inner').classed(chartInnerClasses);

    svg.attr("preserveAspectRatio", "xMinYMin meet");
    // svg.attr("viewBox", "0 0 481 474");

    var year = params[1].year;
    var indicator = null; // params[1].indicatorId;
    var transform = params[1].transform;
    var area = params[1].areaname;
    var areaLevel = params[1].areatype;
    var feature = params[1].featureId;
    var zoom = params[1].zoom;
    var features = params[1].features;
    var chartData = params[1].chartData;
    var chartCaption = params[1].chartCaption;
    var areas = params[1].areas;
    var compareArea = params[1].compareArea;
    var cache = window.MBIECacheStorage;

    /// Cached data
    if (isEmpty(cache.get(indicator))) {
        cache.put(indicator, {});
    }

    if (!isEmpty(features)) {
        var feats = {};

        _.forEach(features, function(f, i) {
            feats[f[0]] = f[1];
        })

        features = feats;
    }

    if (!_.isEmpty(chartCaption)) {
        chartCaption = chartCaption[0].toUpperCase() + chartCaption.slice(1);
    }

    return {
        data: data,
        year: year,
        indicator: indicator,
        transform: transform,
        area: area,
        areas: areas,
        areaLevel: areaLevel,
        feature: feature,
        features: features,
        chartData: chartData,
        chartCaption: chartCaption,
        compareArea: compareArea,
        svg: svg,
        base: base,
        width: width,
        height: height,
        zoom: zoom
    }
}
