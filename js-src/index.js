/* ///
 * Global Imports
 * /// */
import { updateIndicatorTimeSeries as up } from './charts/indicator-timeseries';
import { updateMapLegend as legend } from './charts/map-legend';
import updateTimeSeries from './charts/summary-timeseries';
import updateAreaBarchart from './charts/indicator-barchart';

/* 
 * Reflex functions
 * ----------------:
 * Any functions that need to interact with Reflex must be made global.
 */
window.updateIndicatorTimeSeries = up;
window.updateMapLegend = legend;
window.updateTimeSeries = updateTimeSeries;
window.updateAreaBarchart = updateAreaBarchart;
// window.computeTicks, window.none, window.isEmpty, window.present, window.getColours

// window._ = _;
// window.d3 = d3;

