import _ from 'lodash';
import chroma from 'chroma-js';
import d3 from 'd3';
import rgbHex from 'rgb-hex';

import chartSetup from '../utils/chart-setup';
import format from '../utils/formatting';
import { isEmpty, getColours, none, present } from '../utils/utils';


const margin = {top: 2, right: 2, bottom: 2, left: 2};


function wordwrap(d, i) {
//   if (!d.absolute) {
//     return;
//   }

  var t = d3.select(this),
  rectBB = t.select('rect').node().getBBox(),
  text = t.select('text'),
  textBB = text.node().getBBox();
  if (textBB.width >= rectBB.width - 1|| textBB.height >= rectBB.height - 1) {
    text.style('visibility', 'hidden');
  } else {
    text.style('visibility', 'inherit');
  }
}

export default function(element, params) {
    let colours = getColours();
    let treemap, legendElem, tooltipElem, labels;
    let first = true;

    var startColour = colours['background-rear-positive-light'],
        endColour = colours['background-rear-positive'];

    if (typeof startColour === 'undefined') {
        return false;
    }

    var scale = chroma.scale([rgbHex(startColour),rgbHex(endColour)]);
    var colour = d3.scale.ordinal().domain(["international","domestic"]).range([scale(0.2), scale(0.8)]);

    // Setup
    let setup = chartSetup(element, params, margin, 'area-treemap');

    if (setup === null) {
        return;
    }

    let cache = window.MBIECacheStorage,
        toCache = {},
        data = setup.data, 
        year = setup.year, 
        indicator = setup.indicator, 
        transform = setup.transform, 
        area = setup.area,
        areas = setup.areas,
        areaLevel = setup.areaLevel,
        feature = setup.feature,
        features = setup.features,
        svg = setup.svg,
        base = setup.base,
        width = setup.width, 
        height = setup.height;


    legendElem = d3.select(element).select(".legend");
    tooltipElem = d3.select(element).select(".tooltip");
    treemap = d3.layout.treemap()
      .size([width, height])
      .value(function(d) {
        var value = _.filter(d[1], function(item) {
            return item[0] === year;
        })

        if (value.length > 0) {
            return Number(value[0][1]);
        }

        return 0; 
      });

    svg.selectAll('g').remove();
    let g = svg.selectAll("g").data([1]);
    let svgEnter = g.enter()
      .append("g");

    svgEnter.append("rect")
      .attr("width", width + 2)
      .attr("height", height + 2)
      .attr("x", -1)
      .attr("y", -1)
      .attr("class", "border-rect");

    first = true;

    ///
    /// Data Manager
    ///
    // var dataTree = this.getAttr('data'),
    //   area = this.getAttr('area').get('id'),
    //   dataId = this.get('dataId');
    //   if (!dataTree) {
    //     return;
    //   }
    //   if (dataId.tree !== dataTree.get('id') || dataId.area !== area) {
    //     this.set("plotdata", dataTree.get('areas')[area]);
    //     this.set("dataId", {tree: dataTree.get('id'), area: area});
    //   }

    ///
    /// Draw Labels
    ///
    labels = ["International", "Domestic"];
    var legendWidth = 300;
    var legendHeight = 80;

    var legend = legendElem.selectAll("svg").data([labels]);
    var legendEnter = legend.enter().append("svg");
    legend.attr("width", legendWidth)
          .attr("height", legendHeight);

    var legendG = legend.selectAll("g.key")
                        .data(function(d) {
                          return [d]
                        })
      , legendGEnter = legendG.enter()
                              .append("g")
                              .attr("class", "key");
      legendG.attr("transform", "translate(" + margin.left + "," + legendHeight / 2 + ")");

    var legendRects = legendG.selectAll("rect")
                             .data(function(d) { return d; })
      , legendRectsEnter = legendRects.enter()
                                      .append("rect")
                                      .attr("height", 8)
                                      .attr("width", 55);
    legendRects.attr("x", function(d, i) {
      return i * 95;
    }).attr("fill", function(d) {
      return colour(d.toLowerCase());
    });
    var legendTexts = legendG.selectAll("text")
                             .data(function(d) { return d; })
      , legendTextsEnter = legendTexts.enter()
                                      .append("text")
                                      .attr("dy", "0.71em")
                                      .attr("y", 16);
    legendTexts.attr("x", function(d, i) {
        return i * 95;
      })
      .text(function(d) {
        return d;
      });
      legendTexts.exit().remove();



    /// 
    /// Update
    ///
    // var data = this.get("plotdata"),
    var duration = 1000;
    // feature = this.getAttr("feature"),
    // featureName = present(feature) ? feature.get("name") : null,
    // _this = this;

    let areaData = _.filter(data, function (i) {
        return i[0][1] === area;
      });
    
    // var root = d3.hierarchy(areaData).sum(function(d){
    //     console.log(d)
    //     return 0;
    // })

    var grouped = _.groupBy(areaData, function(item) {
        let key = _.last(item[0]);

        return _.indexOf(areas, key) === -1;
    });

    let domestic = grouped[false],
        international = grouped[true];

    // console.log(domestic, international)
    let treemapData = {
        name: 'top',
        children: [
            {
                name: 'domestic',
                children: domestic
            },
            {
                name: 'international',
                children: international
            }
        ]
    };

    var cell = svgEnter.data([treemapData]).selectAll("g").data(treemap.nodes),
        cellEnter = cell.enter().append("g").attr("class", "cell"),
        cellTrans = cell;

    if (!first) {
      cellTrans = cellTrans.transition()
        .duration(duration);
    }

    cellTrans.attr("transform", function(d) {
      return "translate(" + d.x + "," + d.y + ")";
    });

    var rect = cell.selectAll('rect')
            .data(function(d) { 
                return [d];
            }),
        rectEnter = rect.enter()
            .append("rect")
            .attr("data-bailiwick-feature", function(d) {
                var children = _.hasIn(d, 'children');
                return children ? null : d[0][4]
            });

    
    if (!Modernizr.touch) {
      rectEnter.on("mouseover", function(d, i) {
        var value = _.filter(d[1], function(item) {
            return item[0] === year;
        });

        if (value.length === 0) {
            return;
        }

        // var name = d[0][4][0].toUpperCase() + d[0][4].slice(1)
        let name = d[0][4]
        let _name = _.filter(features, function(feature, key) {
            return key === name;
        })

        if (_name.length !== 0) {
            name = _name[0]
        }
        
        value = Number(d.value)
        value = value.toFixed(1)
        value = '$' + format("million dollars", value);

        var tooltip = tooltipElem.selectAll('p').data([name, value]),
            tooltipEnter = tooltip.enter().append('p');

        tooltip.text(function(d) {
          return d;
        }).classed("number", function(d, i) {
          return i === 1;
        }).classed("local", function(d, i) {
          return i === 1;
        });
        tooltipElem.style("visibility", "visible")
          .style("top", function() {
            return (d3.event.offsetY) + "px";
          })
        .style("left", function() {
          return (d3.event.offsetX) + "px";
        });
      }).on("mouseout", function(d, i) {
        tooltipElem.style("visibility", "hidden");
      });
    }

    rect.style("fill", function(d) {
        var children = _.hasIn(d, 'children');
        return children ? colour(d.name) : null;
    }).attr("pointer-events", function(d) {
        var children = _.hasIn(d, 'children');
        return children ? "none" : "all";
    }).classed("active", function(d) {
        var children = _.hasIn(d, 'children');
        return children ? false : (d[0][4] === feature);
    });

    if (!first) {
      rect.transition()
        .duration(duration);
    }

    rect
        .attr("width", function(d) { return d.dx; })
        .attr("height", function(d) { return d.dy; });

    var text = cell.selectAll("text").data(function(d) { return [d]; }),
        textEnter = text.enter()
            .append('text')
            .style("pointer-events", "none");

    text
      .style("visibility", "hidden")
      .attr("x", function(d) { return d.dx / 2; })
      .attr("y", function(d) { return d.dy / 2; })
      .attr("dy", ".35em")
      .attr("text-anchor", "middle")
      .attr("data-bailiwick-feature", function(d) {
        var children = _.hasIn(d, 'children');
        return children ? null : d[0][4]
      })
      .text(function(d) {
        var children = _.hasIn(d, 'children');

        if (!children) {
            let name = d[0][4];
            let _name = _.filter(features, function(feature, key) {
                return key === name;
            })

            if (_name.length !== 0) {
                return _name[0];
            }
        }

        return children ? null : '';
      });

    if (first) {
      cellTrans.each(wordwrap);
      first = false;
    } else {
      cellTrans.each('end', wordwrap);
    }

    cell.exit().remove();

    base.classed('svg-loading', false);
}