import _ from 'lodash'

/* 
 * computeTicks:
 * -------------
 * @param extent - d3 extent
 * @return array of ticks.
 * 
 * Given a d3 extent, will work out the ticks for a domain.
 */
let computeTicks = function (extent) {
    let nice = d3.scale.linear().domain(extent).nice().ticks();
    if (nice.length < 9) {
        return nice;
    }
    // else if (nice.length % 2 === 0) {
    //     let diff = nice[nice.length - 1] - nice[nice.length - 2];
    //     nice.push(nice[nice.length - 1] + diff);
    // }

    return nice.filter(function (d, i) {
        return i % 2 === 0;
    });
}

/*
 * getColours:
 * -----------
 * looks for elements in the page with a class of "colour" & builds 
 * a map of colournames -> hex.
 */
let getColours = function () {
    let output = {};
    let colours = document.getElementsByClassName("colour-palette");

    if (colours.length === 0) {
        return output;
    }

    let children = document.getElementsByClassName("colour");

    if (children.length === 0) {
        return output;
    }

    _.each(children, function (value, i) {
        let classNames = value.className,
            el = document.getElementsByClassName(classNames),
            bg = window.getComputedStyle(el[0], null).getPropertyValue('background-color');
        output[classNames.split(' ')[0]] = bg;
    });

    return output;

    // .each(function(i, el) {
    //     console.log(i, el)
    //     // let classNames = $(el).attr('class');
    //     // self.set(classNames.split(' ')[0], $(el).css('background-color'));
    // });
};

/*
 * General util functions. 
 */
let none = function (obj) {
        return obj === null || obj === undefined;
    },
    isEmpty = function (obj) {
        return _.isEmpty(obj);
    },
    present = function (obj) {
        return isEmpty(obj) || (typeof obj === 'string' && /\S/.test(obj) === false);
    };

// http://ryanmorr.com/using-mutation-observers-to-watch-for-element-availability/

var listeners = {},
    observer;

function ready_check() {
    // Check the DOM for elements matching a stored selector
    for (var selector in listeners) {
        // Query for elements matching the specified selector
        var elements = document.querySelectorAll(selector);
        for (var j = 0, jLen = elements.length, element; j < jLen; j++) {
            element = elements[j];
            // Make sure the callback isn't invoked with the
            // same element more than once
            if (!element.ready) {
                element.ready = true;
                // Invoke the callback with the element
                listeners[selector].fn.call(element, element);
            }
        }
    }
}

let ready = function(selector, fn) {
    // Store the selector and callback to be monitored
    listeners[selector] = {
        selector: selector,
        fn: fn
    };
    if (!observer) {
        // Watch for changes in the document
        observer = new MutationObserver(ready_check);
        observer.observe(document.documentElement, {
            childList: true,
            subtree: true
        });
    }
    // Check if the element is currently in the DOM
    ready_check();
}

export { computeTicks, none, isEmpty, present, getColours, ready }
