import d3 from 'd3'
import _ from 'lodash'

import { isEmpty } from '../utils/utils';

export default function(element, params, margin, chartType) {
    let base = d3.select(element).select('.d3-attach'),
        svg = null,
        width = parseInt(base.style("width")) - margin.left - margin.right,
        height = parseInt(base.style("height")) - margin.top - margin.bottom,
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

    if (isEmpty(data) || isNaN(width) || isNaN(height)) {
        return null;
    }
    var year = params[1];
    var indicator = params[2].indicatorId;
    var transform = params[2].transform;
    var area = params[2].areaname;
    var areaLevel = params[2].areatype;
    var feature = params[2].featureId;
    var features = params[3];
    var chartData = params[4];
    var chartCaption = params[5];
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
        areaLevel: areaLevel,
        feature: feature,
        features: features,
        chartData: chartData,
        chartCaption: chartCaption,
        svg: svg,
        base: base,
        width: width,
        height: height
    }
}