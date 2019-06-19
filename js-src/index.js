/* ///
 * Global Imports
 * /// */
import CacheStorage from './cache/cache-store';
window.MBIECacheStorage = CacheStorage.getInstance();

import updateIndicatorTimeSeries from './charts/indicator-timeseries';
import updateMapLegend from './charts/map-legend';
import updateTimeSeries from './charts/summary-timeseries';
import updateAreaBarchart from './charts/indicator-barchart';
import overUnderBarchart from './charts/indicator-over-under-barchart';
import areaTreeMap from './charts/indicator-area-treemap';

/* 
 * Reflex functions
 * ----------------:
 * Any functions that need to interact with Reflex must be made global.
 */
updateIndicatorTimeSeries = updateIndicatorTimeSeries;
updateMapLegend = updateMapLegend;
updateTimeSeries = updateTimeSeries;
updateAreaBarchart = updateAreaBarchart;
overUnderBarchart = overUnderBarchart;
areaTreeMap = areaTreeMap;