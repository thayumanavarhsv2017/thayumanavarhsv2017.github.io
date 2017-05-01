var timeFactor = 1; //number of minutes in real life to a second in the viz
$('.timeFactor').html(timeFactor); //Displays the timeFactor in the UI.
var tweenToggle = 0;

var tiles = L.tileLayer('http://{s}.tile.openstreetmap.fr/hot/{z}/{x}/{y}.png',{
  attribution: '&copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>'
});

var topLeft,bottomRight;

var time = moment();
var map = L.map('map',{ zoomControl:true })
.addLayer(tiles)
.setView([78.649755, 10.825148], 14);


var running = {
    "Pace":0,
    "BestPace":0,
    "AvgSpeed":0,
    "MaxSpeed":0,
    "Distance":0,
    "ElevationGain":0,
    "Calories":0
} ;

var svg = d3.select(map.getPanes().overlayPane).append("svg"),
g = svg.append("g").attr("class", "leaflet-zoom-hide");

//area chart
var margin = {top: 30, right: 20, bottom: 20, left: 40},
areaChartWidth = $(window).width() - margin.left - margin.right -40,
areaChartHeight = 140 - margin.top - margin.bottom;

var parseDate = d3.time.format("%d-%b-%y").parse;

var x = d3.scale.linear()
.range([0, areaChartWidth]);

var y = d3.scale.linear()
.range([areaChartHeight, 0]);

var xAxis = d3.svg.axis()
.scale(x)
.orient("bottom");

var yAxis = d3.svg.axis()
.scale(y)
.orient("left")
.ticks(4);

var area = d3.svg.area()
.x(function(d) { return x(d.time); })
.y0(areaChartHeight)
.y1(function(d) { return y(d.runningFare); });

var areaChartSvg = d3.select(".areaChartBox").append("svg")
.attr("width", areaChartWidth + margin.left + margin.right)
.attr("height", areaChartHeight + margin.top + margin.bottom)
.attr("class","areaChart")
.append("g")
.attr("transform", "translate(" + margin.left + "," + margin.top + ")");

var markerLine = areaChartSvg.append('line')
.attr('x1', 0)
.attr('y1', 0)
.attr('x2', 0)
.attr('y2', areaChartHeight )
.attr("class","markerLine");

var dummyData = [];



x.domain([0, 3]);
y.domain([0, 50]);

var chartPath = areaChartSvg.append("path")
.datum(dummyData)
.attr("class", "area");
//.attr("d", area);

areaChartSvg.append("g")
.attr("class", "x axis")
.attr("transform", "translate(0," + areaChartHeight + ")")
.call(xAxis)
.append("text")
.attr("y", 9)
.attr("x", 39)
.attr("dy", ".71em")
.style("text-anchor", "end")
.text("Hour");

areaChartSvg.append("g")
.attr("class", "y axis")
.call(yAxis)
.append("text")
.attr("transform", "rotate(-90)")
.attr("y", 6)
.attr("dy", ".71em")
.style("text-anchor", "end")
.text("Pace (km/h)");



//end area chart

//listeners

$('.slower').click(function(){
    if(timeFactor > 1){
        timeFactor -= 1;
    };

    $('.timeFactor').html(timeFactor);

});

$('.faster').click(function(){
    timeFactor += 1;
    $('.timeFactor').html(timeFactor);

});

$('.reload').click(function(){
    location.reload();
});

var transform = d3.geo.transform({
    point: projectPoint
}),
d3path = d3.geo.path().projection(transform);

var timer;

function updateTimer() {
    time.add('minutes',1);
    $('.readableTime').text(time.format('h:mm a'));
    $('.date').text(time.format('dddd, MMMM Do YYYY'));
    timer = setTimeout(function(){updateTimer()},(1000/timeFactor));
}

//get a random number between 0 and 11p
var number = Math.floor(Math.random() * 15)

d3.json('traces/ride1.geojson', function (data) {

//    console.log("Loaded data for run: " + data.features[0].properties.name);
  //  console.log("Data load for: " + data.features[0]);

    var feature = g.selectAll("path")
    .data(data.features)
    .enter().append("path")
    .attr("class", function (d) {
        return "trip0";
    })
    .attr("style", "opacity:0");
    var pointsArray = [];
    var points = g.selectAll(".point")
    .data(pointsArray);

var marker = g.append("circle");
marker.attr("r", 5)
.attr("id", "marker");
//.attr("transform", "translate(" + startPoint + ")");

//Get path start point for placing marker



//var string = JSON.stringify(j);


map.on("viewreset", reset);
map.on("zoomend", reset);
reset();

var i = 0;

function iterate() {

    var chartInterval = 0;

    var emptyData = [];

    var emptyPath = areaChartSvg.append("path")
    .datum(emptyData)
    .attr("class", "empty");



    var path = svg.select("path.trip0")
    .attr("style", "opacity:.7")
    .call(transition);



    function pathStartPoint(path) {ppp
        var d = path.attr('d');
        console.log("PathStartPoint "+d);
        dsplitted = d.split("L")[0].slice(1).split(",");
        var point = []
        point[0]=parseInt(dsplitted[0]);
        point[1]=parseInt(dsplitted[1]);
        console.log("PathStPoint "+ point[0] + "  " + point[1]);
        return point;
    }


    var startPoint = pathStartPoint(path);
    marker.attr("transform", "translate(" + startPoint[0] + "," + startPoint[1] + ")");


path.each(function(d){

//add the translation of the map's g element
startPoint[0] = startPoint[0]; //+ topLeft[0];
startPoint[1] = startPoint[1]; //+ topLeft[1];
var newLatLon = coordToLatLon(startPoint);
pointsArray.push([newLatLon.lng,newLatLon.lat,true]);


points = g.selectAll(".point")
.data(pointsArray)
.enter()
.append('circle')
.attr("r",5)
.attr("class",function(d){
    if(d[2]) {
        return "startPoint point";
    } else {
        return "endPoint point";
    }
})
.attr("transform",function(d){
    return translatePoint(d);
});

    marker
    .transition()
    .duration(500)
    .attr("r",5)
    .attr('style','opacity:1');
});




function transition(path) {

    g.selectAll

    path.transition()
    .duration(function(d){
        //calculate seconds
        var start = Date.parse(d.properties.time),
        finish = Date.parse("2017-04-22T03:28:39Z"),
        duration = finish - start;

        duration = duration/60000; //convert to minutes

        duration = duration * (1/timeFactor) * 1000;

        time = moment(d.properties.time.toString());
        $('.readableTime').text(time.format('h:mm a'));
         return (duration)
})
    .attrTween("stroke-dasharray", tweenDash)
    .each("end", function (d) {
            running.Pace += 0.0;
            running.BestPace += 0.0;
            running.AvgSpeed += 0.0;
            running.MaxSpeed += 0.0;
            running.Distance += 0.0;
            running.ElevationGain += 0;
            running.Calories += 0;
            updateRunning();

        i++;
        var nextPath = svg.select("path.trip" + i);
        if (nextPath[0][0]==null){
            clearTimeout(timer);
        } else {
            iterate();
        }
    });
}

function tweenDash(d) {

    var l = path.node().getTotalLength();
    var i = d3.interpolateString("0," + l, l + "," + l); // interpolation of stroke-dasharray style attr
return function (t) {
    var marker = d3.select("#marker");
    console.log(t);
    var p = path.node().getPointAtLength(t * l);
marker.attr("transform", "translate(" + p.x + "," + p.y + ")");//move marker


if (tweenToggle == 0) {
    tweenToggle = 1;
    var newCenter = map.layerPointToLatLng(new L.Point(p.x,p.y));

    map.panTo(newCenter, 13);
} else {
    tweenToggle = 0;
}


//update chart data every X frames
if(chartInterval == 5){
    chartInterval = 0;
    var decimalHour = parseInt(time.format('H')) + parseFloat(time.format('m')/60)
    if(isNaN(d.properties.fare)){
        d.properties.fare = 0; 
    }

    var incrementalFare = d.properties.fare*t;
    dummyData.push({
        "time": decimalHour,
        "runningFare": running.Pace + parseFloat(incrementalFare)
    });
chartPath.attr("d", area); //redraw area chart
if(d.properties.hasfare == false) { //draw purple area for nonfare time
    emptyData.push({
        "time": decimalHour,
        "runningFare": running.Pace + parseFloat(incrementalFare)
    });
    emptyPath.attr("d", area);
}

markerLine
.attr('x1', x(decimalHour))
.attr('x2', x(decimalHour));
} else {
    chartInterval++;
}
return i(t);
}
}
}

updateRunning();
    $('.overlay').fadeOut(250);
    $('.box').fadeIn(250);
    setTimeout(function(){
       updateTimer();
       iterate();
    },700);

/*$('#begin').click(function(){
    $('.overlay').fadeOut(250);
    $('.box').fadeIn(250);
    setTimeout(function(){
        updateTimer();
        iterate();
    },500);

});*/


function updateRunning() {
    $('.runningPace').text('$'+running.Pace.toFixed(2));
    $('.runningBestPace').text('$'+running.BestPace.toFixed(2));
    $('.runningAvgSpeed').text('$'+running.AvgSpeed.toFixed(2));
    $('.runningMaxSpeed').text('$'+running.MaxSpeed.toFixed(2));
    $('.runningDistance').text('$'+running.Distance.toFixed(2));
    $('.runningElevationGain').text('$'+running.ElevationGain.toFixed(2));
    $('.runningCalories').text(running.Calories);
}

// Reposition the SVG to cover the features.
function reset() {
    var bounds = d3path.bounds(data);
    topLeft = bounds[0],
    bottomRight = bounds[1];

    svg.attr("width", bottomRight[0] - topLeft[0] + 100)
    .attr("height", bottomRight[1] - topLeft[1] + 100)
    .style("left", topLeft[0]-50 + "px")
    .style("top", topLeft[1]-50 + "px");

    g.attr("transform", "translate(" + (-topLeft[0]+50) + "," + (-topLeft[1]+50)+ ")");

    feature.attr("d", d3path);

    g.selectAll(".point")
    .attr("transform",function(d){
        return translatePoint(d);
    });
}
});
// Use Leaflet to implement a D3 geometric transformation.
function projectPoint(x, y) {
    var point = map.latLngToLayerPoint(new L.LatLng(y, x));
    this.stream.point(point.x, point.y);
}

function translatePoint(d) {
    var point = map.latLngToLayerPoint(new L.LatLng(d[1],d[0]));

    return "translate(" + point.x + "," + point.y + ")";
}

function coordToLatLon(coord) {
var point = map.layerPointToLatLng(new L.Point(coord[0],coord[1]));
return point;
}

