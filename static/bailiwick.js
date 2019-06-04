Math.trunc = Math.trunc || function(x) {
    return x < 0 ? Math.ceil(x) : Math.floor(x);
};
var none = function(obj) {
        return obj === null || obj === undefined;
    },
    isEmpty = function isEmpty(obj) {
        let none = isNone(obj);
        if (none) {
          return none;
        }
      
        if (typeof obj.size === 'number') {
          return !obj.size;
        }
      
        let objectType = typeof obj;
      
        if (objectType === 'object') {
          let size = get(obj, 'size');
          if (typeof size === 'number') {
            return !size;
          }
        }
      
        if (typeof obj.length === 'number' && objectType !== 'function') {
          return !obj.length;
        }
      
        if (objectType === 'object') {
          let length = get(obj, 'length');
          if (typeof length === 'number') {
            return !length;
          }
        }
      
        return false;
    },
    present = function(obj) {
        return isEmpty(obj) || (typeof obj === 'string' && /\S/.test(obj) === false);
    };


/**
 * Update the time series on the home page side bar - housePriceTimeSeries
 **/
var updateTimeSeries = function(element, labelledData, activeLabelName) {
    console.log('labelled', labelledData, activeLabelName)
    var base = d3.select(element).select('.d3-attach'),
        svg = base.select('svg').empty() ? base.append('svg') : base.select('svg'),
        yearFormat = d3.time.format('%Y').parse,
        margin = {
            top: 15,
            right: 0,
            bottom: 30,
            left: 48
        },
        baseW = 225,
        baseH = 120,
        width = baseW - margin.left - margin.right,
        height = baseH - margin.top - margin.bottom,
        _this = this;

    svg.attr('width', baseW);
    svg.attr('height', baseH);

    var data = labelledData.map(function(d) {
        return d[1];
    });

    var x = d3.time.scale()
        .range([0, width]);

    var y = d3.scale.linear()
        .range([height, 0]);

    var xAxis = d3.svg.axis()
        .scale(x)
        .ticks(5)
        .innerTickSize(3)
        .outerTickSize(0)
        .tickFormat(function(t) {
            return t.getFullYear().toString().replace(/^20/, "'");
        })
        .orient("bottom");

    var yAxis = d3.svg.axis()
        .scale(y)
        .ticks([4])
        .outerTickSize(0)
        .innerTickSize(-width)
        .tickFormat(function(t) {
            return '$' + t / 1000 + ' k';
        })
        .orient("left");

    var line = d3.svg.line()
        .x(function(d) {
            return x(yearFormat(d[0].toString()));
        })
        .y(function(d) {
            return y(d[1]);
        });

    var g = svg.selectAll('g').data([data]);
    var gEnter = g.enter()
        .append("g")
        .attr("transform", "translate(" + margin.left + "," + margin.top + ")");

    var yMax = Math.ceil(d3.max(d3.merge(data), function(d) {
        return d[1];
    }) / 100000) * 100000;
    x.domain(d3.extent(data[0], function(d) {
        return yearFormat(d[0].toString());
    }));
    y.domain([0, yMax]);

    gEnter.append("g")
        .attr("class", "x axis")
        .attr("transform", "translate(0," + height + ")")
        .append("text")
        .attr("x", 50)
        .attr("dy", "2.5em")
        .text("Year");

    g.selectAll("g.x")
        .call(xAxis);

    gEnter.append("g")
        .attr("class", "y axis")
        .attr("transform", "translate(-7,0)");

    g.selectAll("g.y")
        .call(yAxis);

    var linePlot = g.selectAll("path.line").data(labelledData);
    linePlot.enter()
        .append("path");

    linePlot.attr("d", function(d) {
        return line(d[1]);
    }).attr("class", function(d, i) {
        if (activeLabelName === d[0]) {
            return "line active";
        }
        return "line";
    });

    linePlot.exit().remove();
}

/**
 * Legend for the map
 **/
var updateMapLegend = function(width, height, scaledata) {
    var svg = d3.select(".legend").selectAll("svg")
      .data([scaledata])
      .enter()
      .append("svg")
      .attr("width", width)
      .attr("height", height);
    var step = (width-100)/100;

    var sd = Array.from(Array(100).keys(),
                 i => [i*step, scaledata[i][2]]);
    // console.log(sd);

    var g = svg.selectAll("g")
      .data([sd])
      .enter()
      .append("g")
      .attr("class", "key")
      .attr("transform", "translate(50," + height * 1 / 3 + ")");

    var legend = g.selectAll("rect")
      .data(sd)
      .enter()
      .append("rect")
      .attr("height", 8)
      .attr("x", function(d) { return d[0]; })
      .attr("width", step)
      .style("fill", function(d) { return d[1]; })
      .style("stroke", function(d) { return d[1]; });
}

/**
 * Initialise a bar chart - used on the indicator.
 **/
var updateAreaBarchart = function(element, data) {
    console.log('updateAreaBarchart', element, data);
    // var container = d3.select(element).select('svg.d3-attach')
    //   , tooltipElem = d3.select(element).select(".tooltip")
    //   , margin = {
    //       top: 5,
    //       right: 25,
    //       bottom: 40,
    //       left: 140
    //     };
    //
    // width = parseInt(container.style("width")) - margin.left - margin.right;
    // height = parseInt(container.style("height")) - margin.top - margin.bottom;
}

/**
 * Time series used on an indicator
 * * data: { year, selected area, areas }
 **/
var yearFormat = d3.time.format('%Y').parse;
var transforms = ["absolute", "indexed", "percapita"];
var x = d3.time.scale();

var y = d3.scale.linear();

var clipHeight = 393;
var clipWidth = 363;
var margin = {top: 40, right: 40, bottom: 40, left: 80};

var line = d3.svg.line()
  .x(function(d, i) { return x(d.date); })
  .y(function(d) { return y(d.v); });

// data: (data, year, indicator, transform)
var updateIndicatorTimeSeries = function(element, data) {
    // console.log('indicator time series', data);
    var transform = data[3];
    var indicator = data[2];
    var year = data[1];
    data = data[0];

    /// Setup
    var svg = d3.select(element).select('svg.d3-attach');

    width = parseInt(svg.style("width")) - margin.left - margin.right;
    height = parseInt(svg.style("height")) - margin.top - margin.bottom;

    x.range([0, width]);
    y.range([height, 0]);

    voronoi = d3.geom.voronoi()
      .x(function(d) { return x(d.date); })
      .y(function(d) { return y(d.v); })
      .clipExtent([[-margin.left, -margin.top], [width + margin.right, height + margin.bottom]]);

    /// Update
    // var _this = this
    //   , data = this.getAttr('data')
    //   , feature = this.getAttr('feature')
    //   , indicator = this.getAttr('indicator')
    //   , transform = this.get('bailiwick.transformType')
    //   , areaName = this.getAttr('area').get('name')
    //   , areaType = this.get('bailiwick.areaType')
    //   , compareAreaName = this.get('bailiwick.compareArea.name')
    //   , hover = this.get("hover")
    //   , areaLookup = this.getAttr("areaLookup")
    //   , caption = this.getAttr("caption")
    //   , formatter = caption.get("formatter")
    //   , yearEndMonth = indicator.get("yearEndMonth")
    //   , yearCaption = yearEndMonth ? "Year to " + yearEndMonth : "Year"
    //   , bailiwick = this.get('bailiwick')
    //   ;


    svg.selectAll('g').remove();
    var g = svg.selectAll('g').data([data])
      , gEnter = g.enter()
                    .append("g")
                    .attr("transform", "translate(" + margin.left + "," + margin.top + ")");
    
    var years = _.uniq(_.reduce(data, function(result, value, key) {
        result.push(value[0][1]);
        return result;
    }, [])).sort(function(a, b) {
        return d3.ascending(a, b);
    }).map(function(y) {
        return yearFormat(y + '');
    });

    var transformPos = transforms.indexOf(transform);
    var pos = transformPos === -1 ? 1 : transformPos + 1;
    var dispPos = transformPos === -1 ? 4 : transformPos + 4;

    var areas = data.map(function(a) {
        // console.log(a, "a");
        var areaInfo = a[0];
        var areaData = {
            info: a[0],
            vals: a[1]
        };

        console.log(a)

        // var area = { 
        //     slug: areaInfo[0],
        //     name: areaInfo[0], /// need to be able to look up by areaId
        //     dsArea: areaInfo[0],/// need to be able to look up by areaId
        //     values: areaData.map(function(y) {
        //         // y.date = yearFormat(y[0][1] + '');
        //         // y.v = y[pos];
        //         // y.d = y[dispPos];
        //         // console.log(y);
        //         return y;
        //     })
        // };

        // console.log(area)
  
        // area.values.forEach(function(o) {
        //   o.area = area;
        // });
        // return area;
        return [];
      });
    // console.log(areas);
    //   .filter(function(a) {
    //       var valid = false;
  
    //       switch (a.name) {
    //           case compareAreaName:
    //               valid = true;
    //               break;
    //           case areaName:
    //               valid = true;
    //               break;
    //           case 'New Zealand':
    //               valid = true;
    //               break;
    //           default:
    //               break;
    //       }
  
    //       var level = a.dsArea.get('level');
  
    //       return level === areaType || valid;
    //   });
}




