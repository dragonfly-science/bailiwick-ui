/* ///
 * Global Imports
 * /// */
// const _ = require('lodash');
// import _ from 'lodash';
// import d3 from 'd3';
import { computeTicks, none, isEmpty, present, getColours } from './utils/utils';
import { updateIndicatorTimeSeries as up } from './charts/indicator-timeseries';

/* 
 * Reflex functions
 * ----------------:
 * Any functions that need to interact with Reflex must be made global.
 */
window.updateIndicatorTimeSeries = up;
// window.computeTicks, window.none, window.isEmpty, window.present, window.getColours

// window._ = _;
// window.d3 = d3;

