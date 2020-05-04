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

let whenLoaded = function(svg, target_selector, ancestor_selector, callback) {
    // Jumping through hoops here so we can wait until an svg has been
    // inserted before attempting to use its dimensions.
    if (svg.node().getRootNode().constructor.name === "DocumentFragment") {
        var target = document.querySelector(target_selector);
        var ancestor = svg.node().getRootNode().querySelector(ancestor_selector);
        var observer = new MutationObserver(function(rs) {
            for (let i = 0; i < rs.length; i++) {
                if (rs[i].target !== target) {
                    return;
                }
                for (let j = 0; j < rs[i].addedNodes.length; j++) {
                    if (rs[i].addedNodes[j] === ancestor) {
                        callback();
                        return;
                    }
                }
            }
        });
        observer.observe(target, {childList: true});
    } else {
        callback();
    }
}

export { computeTicks, none, isEmpty, present, getColours, whenLoaded }
