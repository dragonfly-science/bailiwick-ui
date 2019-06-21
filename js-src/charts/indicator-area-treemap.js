import _ from 'lodash';
import chroma from 'chroma-js';
import d3 from 'd3';
import rgbHex from 'rgb-hex';

import chartSetup from '../utils/chart-setup';
import { isEmpty, getColours, none, present } from '../utils/utils';


var margin = {top: 2, right: 2, bottom: 2, left: 2};
var width, height, svg, treemap, legendElem, tooltipElem, labels;
var first = true;



function wordwrap(d, i) {
  if (!d.absolute) {
    return;
  }

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

    console.log('treemap', params);

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
        areaLevel = setup.areaLevel,
        feature = setup.feature,
        svg = setup.svg,
        base = setup.base,
        width = setup.width, 
        height = setup.height;

    legendElem = d3.select(element).select(".legend");
    tooltipElem = d3.select(element).select(".tooltip");
    treemap = d3.layout.treemap()
      .size([width, height])
      .value(function(d) { return d.absolute; });

    svg.append("rect")
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
    var labels = ["International", "Domestic"];
    var legendWidth = 300;
    var legendHeight = 80;

    var legend = legendElem.selectAll("svg")
      .data([labels]);
    var legendEnter = legend.enter()
      .append("svg");
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
      return color(d.toLowerCase());
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
    // duration = this.get("duration"),
    // feature = this.getAttr("feature"),
    // featureName = present(feature) ? feature.get("name") : null,
    // _this = this;

    // this.$().addClass("svg-loading");

    // if (!(svg && data && present(data.features) && data.features.children.length === 2)){
    //     this.$().removeClass("svg-loading");
    //   return;
    // }

    // var cell = svg.data([data.features]).selectAll("g")
    //   .data(treemap.nodes),
    // cellEnter = cell.enter().append("g")
    //   .attr("class", "cell"),
    // cellTrans = cell;

    // this.set("labels", cell.data()[0].children.map(function(d) {
    //   return d.name[0].toUpperCase() + d.name.substr(1);
    // }).reverse());

    // if (!first) {
    //   cellTrans = cellTrans.transition()
    //     .duration(duration);
    // }

    // cellTrans.attr("transform", function(d) {
    //   return "translate(" + d.x + "," + d.y + ")";
    // });

    // var rect = cell.selectAll('rect')
    //   .data(function(d) { return [d];}),
    // rectEnter = rect.enter()
    //   .append("rect")
    //   .on("click", function(d, i) {
    //     _this.sendAction('featureAction', d.slug);
    //   });
    // if (!Modernizr.touch) {
    //   rectEnter.on("mouseover", function(d, i) {
    //     var tooltip = tooltipElem.selectAll('p')
    //       .data([d.name, d.dispAbsolute]),
    //     tooltipEnter = tooltip.enter().append('p');


    //     tooltip.text(function(d) {
    //       return d;
    //     }).classed("number", function(d, i) {
    //       return i === 1;
    //     }).classed("local", function(d, i) {
    //       return i === 1;
    //     });
    //     tooltipElem.style("visibility", "visible")
    //       .style("top", function() {
    //         return (d3.event.offsetY) + "px";
    //       })
    //     .style("left", function() {
    //       return (d3.event.offsetX) + "px";
    //     });
    //   }).on("mouseout", function(d, i) {
    //     tooltipElem.style("visibility", "hidden");

    //   });
    // }

    // rect.style("fill", function(d) {
    //   return d.children ? color(d.name) : null;
    // }).attr("pointer-events", function(d) {
    //   return d.children ? "none" : "all";
    // }).classed("active", function(d) {
    //   return d.name === featureName;
    // });

    // if (!first) {
    //   rect.transition()
    //     .duration(duration);
    // }
    // rect.attr("width", function(d) { return d.dx; })
    //   .attr("height", function(d) { return d.dy; });

    // var text = cell.selectAll("text")
    //   .data(function(d) {
    //     return [
    //       d
    //     ];

    //   }),
    // textEnter = text.enter()
    //   .append('text')
    //   .style("pointer-events", "none");

    // text
    //   .style("visibility", "hidden")
    //   .attr("x", function(d) { return d.dx / 2; })
    //   .attr("y", function(d) { return d.dy / 2; })
    //   .attr("dy", ".35em")
    //   .attr("text-anchor", "middle")
    //   .text(function(d) {
    //     return d.children ? null : d.name[0].toUpperCase()
    //       + d.name.slice(1);
    //   });

    // if (first) {
    //   cellTrans.each(wordwrap);
    //   first = false;
    // } else {
    //   cellTrans.each('end', wordwrap);
    // }

    // cell.exit().remove();
    // this.$().removeClass("svg-loading");
    base.classed('svg-loading', false);
}