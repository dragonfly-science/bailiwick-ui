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

    svg.attr("preserveAspectRatio", "xMinYMin meet")
        .attr("viewBox", "0 0 481 474");

    if (isEmpty(data) || isNaN(width) || isNaN(height)) {
        return null;
    }

    var year = params[1];
    var indicator = params[2];
    var transform = params[3];
    var area = params[4]
    var areaLevel = params[5];

    var cache = window.MBIECacheStorage;

    /// Cached data    
    if (isEmpty(cache.get(indicator))) {
        cache.put(indicator, {});
    }

    return {
        data: data,
        year: year,
        indicator: indicator,
        transform: transform,
        area: area,
        areaLevel: areaLevel,
        svg: svg,
        base: base,
        width: width,
        height: height
    }
}