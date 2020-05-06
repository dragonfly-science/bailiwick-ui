/* ///
 * Global Imports
 * /// */
import CacheStorage from './cache/cache-store';
global.MBIECacheStorage = CacheStorage.getInstance();

import updateIndicatorTimeSeries from './charts/indicator-timeseries';
import { updateMapLegend, positiveScale } from './charts/map-legend';
import updateTimeSeries from './charts/summary-timeseries';
import updateAreaBarchart from './charts/indicator-barchart';
import overUnderBarchart from './charts/indicator-over-under-barchart';
import areaTreeMap from './charts/indicator-area-treemap';

/* 
 * Reflex functions
 * ----------------:
 * Any functions that need to interact with Reflex must be made global.
 */
global.updateIndicatorTimeSeries = updateIndicatorTimeSeries;
global.updateMapLegend = updateMapLegend;
global.positiveScale = positiveScale;
global.updateTimeSeries = updateTimeSeries;
global.updateAreaBarchart = updateAreaBarchart;
global.overUnderBarchart = overUnderBarchart;
global.areaTreeMap = areaTreeMap;

export default global;
