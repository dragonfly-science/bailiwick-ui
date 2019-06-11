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
    let colors = document.getElementsByClassName("colour-palette");
    let colorChildren = colors[0].getElementsByClassName("colour");

    let output = {};

    _.each(colorChildren, function (value, i) {
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

export { computeTicks, none, isEmpty, present, getColours }