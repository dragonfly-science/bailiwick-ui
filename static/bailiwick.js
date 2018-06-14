var updateTimeSeries = function(element, labelledData, activeLabelName) {
  var base = d3.select(element).select('svg.d3-attach')
    , yearFormat = d3.time.format('%Y').parse
    ;
  var margin = {top: 15, right: 0, bottom: 30, left: 43}
    , width = base.attr('width') - margin.left - margin.right
    , height = base.attr('height') - margin.top - margin.bottom
    ;
  var _this = this;

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
    .x(function(d) { return x(yearFormat(d[0])); })
    .y(function(d) { return y(d[1]); });

  var g = base.selectAll('g').data([data]);
  var gEnter = g.enter()
    .append("g")
    .attr("transform", "translate(" + margin.left + "," + margin.top + ")");

  var yMax = Math.ceil(d3.max(d3.merge(data), function(d) { return d[1]; }) / 100000) * 100000;
  var yMin = Math.ceil(d3.min(d3.merge(data), function(d) { return d[1]; }) / 100000) * 100000;
  x.domain(d3.extent(data[0], function(d) { return yearFormat(d[0]); }));
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
    .attr("transform", "translate(-7,0)")
    .append("text")
    .attr("y", -1 * margin.left)
    .attr("x", -5)
    .attr("dy", function(d) {
      return d3.select(this).style('line-height');
    });

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