import Cache from 'cache';
import _ from 'lodash';

export default (function () {
    let instance;
 
    function createInstance() {
        let object = new Cache(5 * 60 * 1000);
        return object;
    }
 
    return {
        getInstance: function () {
            if (!instance) {
                instance = createInstance();
            }
            return instance;
        }
    };
})();