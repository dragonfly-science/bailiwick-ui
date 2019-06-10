/* ///
 * Global Imports
 * /// */
import updateIndicatorTimeSeries from './charts/indicator-timeseries';
import updateMapLegend from './charts/map-legend';
import updateTimeSeries from './charts/summary-timeseries';
import updateAreaBarchart from './charts/indicator-barchart';

/* 
 * Reflex functions
 * ----------------:
 * Any functions that need to interact with Reflex must be made global.
 */
window.updateIndicatorTimeSeries = updateIndicatorTimeSeries;
window.updateMapLegend = updateMapLegend;
window.updateTimeSeries = updateTimeSeries;
window.updateAreaBarchart = updateAreaBarchart;