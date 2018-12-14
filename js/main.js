import $ from 'webpack-zepto'

import { updateHomeTimeSeries } from './modules/time-series/home-time-series'
import { setupDefaultTimeSeries, updateDefaultTimeSeries } from './modules/time-series/default-time-series'


// Globally expose all the modules to reflex.
window.updateHomeTimeSeries = updateHomeTimeSeries
window.setupDefaultTimeSeries = setupDefaultTimeSeries
window.updateDefaultTimeSeries = updateDefaultTimeSeries