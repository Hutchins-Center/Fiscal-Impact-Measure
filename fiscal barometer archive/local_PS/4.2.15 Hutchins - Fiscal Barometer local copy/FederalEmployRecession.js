	
var FederalEmploy = function() {
				
				$('#FederalEmployChart').remove();
				
				
var bigwidth = $('#FederalEmploy').width();
var bigheight = $('#FederalEmploy').height();			
var margin;

$('.tooltip').remove();	
$('#content_0_pnlInteractive').append('<div class="tooltip" id="newtooltip"></div>');	
	
	if (bigwidth == 400) {
margin  = {top: bigheight * 0.25, right: bigwidth * 0.05, bottom: bigheight * .15, left: bigwidth * .12};}
else  {
margin  = {top: bigheight * 0.16, right: bigwidth * 0.05, bottom: bigheight * .2, left: bigwidth * .08};
}
	
var	width = bigwidth - margin.left - margin.right,
    height = bigheight - margin.top - margin.bottom;
	
	
	function commaSeparateNumber(val){
    while (/(\d+)(\d{3})/.test(val.toString())){
      val = val.toString().replace(/(\d+)(\d{3})/, '$1'+','+'$2');
    }
    return val;
  }
	
	var tooltip = d3.selectAll("#newtooltip").style("opacity", 0);


//var parseDate = d3.time.format("%m/%d/%Y").parse;
//for new CSV with recession shading
var parseDate = d3.time.format("%m/%d/%Y").parse;


var color = d3.scale.ordinal()
.range(["#524A48","#A2C0DE","#133570","#A89D95", "#524A48","#A2C0DE","#133570","#A89D95",  ]);


var x = d3.time.scale()
    .range([0, width]);
		
var x2;
	

var y = d3.scale.linear()
   .range([height, 0]);	

var xAxis = d3.svg.axis()
    .scale(x)
    .orient("bottom");
	
var xAxis2 = d3.svg.axis()
    .scale(x)
    
var yAxis = d3.svg.axis()
    .scale(y)
    .orient("left")
	 .ticks(5);
	


var hourlyline = d3.svg.line()
.defined(function(d) { return d.MovingAverage})
//.defined(function(d) { return d.MovingAverage != ''; })
//.defined(function(d) { return d.MovingAverage != 'N/A' })
 //.interpolate("basis")
    .x(function(d) { return x(d.date); })
    .y(function(d) { return y(d.MovingAverage); });
	
/*	var hourlylinezero = d3.svg.line()
.defined(function(d) { return d.MovingAverage != null; })
 //.interpolate("basis")
    .x(function(d) { return x(d.date); })
    .y(function(d) { return y(0); });*/


var FederalEmployChart = d3.select("#FederalEmploy").append("svg")
   .attr('id', 'FederalEmployChart')
    .attr("width", width + margin.left + margin.right)
    .attr("height", height + margin.top + margin.bottom)
  .append("g")
    .attr("transform", "translate(" + margin.left + "," + margin.top + ")")

/*.on("mouseover",function() {
			tooltip.transition()									
				.duration(500)								
				.style("opacity", .9);	
})*/
.on("mouseout",function() {
			tooltip.transition()								
				.duration(500)									
				.style("opacity", .0);	
});




		var data = d3.csv("csv/jobs_fedFinal.csv", function(error, data) {


  color.domain(d3.keys(data[0]).filter(function(key) { return key !== "date"}));

/*data = data.filter(function(d, i) 
    { 

            if (d["MovingAverage"] != "N/A"  && d["MovingAverage"] != "" ) 
            { 
                return d; 
            }
			 else if (d["MovingAverage"] == "N/A"  ) 
            { 
                return  "N/A"; 
            }
			else
			{ return ''} 

        })
*/
	
	
  data.forEach(function(d) {
	  //tell it which column to get for date
    d.date = parseDate(d.date);
	

  });
  
    var thelines = color.domain().map(function(name) {
    return {
      name: name,
      values: data.map(function(d) {
		 
        {return {date: d.date, stuff: +d[name]}
		
		};
      })
    };
  })
  
/*  var thelines = color.domain().map(function(name) {
    return {
      name: name,
      values: data.map(function(d) {
		   if (d[name] != 'N/A' && d[name] != '')
        {return {date: d.date, stuff: +d[name]}}
		else if (d[name] == 'N/A')
		{return {date: d.date, stuff: 300}
		}
		else 
		{return {date: d.date, stuff: d[name]}
		};
      })
    };
  })*/

	


  x.domain(d3.extent(data, function(d) { return d.date; }));
/*  y.domain(d3.extent(data, function(d) { return d.MonthlyChange }))
  .nice();*/
  
  y.domain([
    d3.min(thelines, function(c) { return d3.min(c.values, function(v) { return v.stuff; }); }),
    d3.max(thelines, function(c) { return d3.max(c.values, function(v) { return v.stuff; }); })
  ])
  .nice();
  
  /*  y.domain([-25,25])
  .nice();
  */
  
   //new
   function Y0() {
  return y(0);
}
//size y proportional to data
 function Y(d) {
 return y(d.MonthlyChange);
}
//use time for x scale since dates
 
 x2 = d3.time.scale()
 //the 'data.length -1' is because heder row counts as one entry?
    .domain([d3.time.month(data[0].date), d3.time.month.offset((data[data.length - 1].date), 1)])
	//since bars, set x scale to discrete bands not contiunous
   // .rangeRound([0, width],40)
	.rangeRound([0, width])
	//nice ns up, pads, etc.
	.nice();
	//extent gets span from highest to lowest data vals
	

// new for SC- recession bars
FederalEmployChart.selectAll('g > onepath')
	  .data(data)
	  .enter()
		.append("rect")
		       // attach a rectangle
    .attr("x", function(d) { return x(d.date)})         // position the left of the rectangle
    .attr("y", 0)          // position the top of the rectangle
    .attr("height", function(d, i) {
		if (d.RecessionDummy == 1 && i !== (data.length-1)) {return height}
		else {return 0}})    // set the height
      //.attr("width", width / data.length +7)
	.attr("width", width / data.length *1.1)
	.attr("fill", "#EDEBE2"); 	
//end new for recession bars	 
	
	 

FederalEmployChart.insert("g", ".bars")         
        .attr("class", "grid horizontal")
        .call(d3.svg.axis().scale(y)
            .orient("left")
			.ticks(5)
            .tickSize(-(width), 0, 0)
            .tickFormat(""));
			
FederalEmployChart.insert("g", ".bars")         
        .attr("class", "grid vertical")
		 .attr("transform", "rotate(-90)")
        .call(d3.svg.axis().scale(x)
            .orient("left")
            .tickSize((height), 0, 0)
            .tickFormat(""));


FederalEmployChart.append("g")
      .attr("class", "x axis")
      .attr("transform", "translate(0," + height + ")")
      .call(xAxis);



	   FederalEmployChart.append("g")
      .attr("class", "y axis")
      .call(yAxis)
    .append("text")
      .attr("transform", "rotate(-90)")
      .attr("y", -30)
	  .attr("x", 0 - height/2)
	  .attr("class", "axislabel")
	.style("text-anchor", "middle")
    .text("Jobs");
	  
	  



  //new- barchart
	var barchart =   FederalEmployChart.append("g")
	.attr("class", "barchart")
	
	barchart.selectAll("#chartspace")
    .data(data)
  .enter().append('rect')
     .attr("class", function(d, i) { 
	
	 return d.MonthlyChange < 0 ? "bar negative" : "bar positive"; })
	 .attr("id", function(d,i) {return 'chartbar' + i})
    .attr('x', function(d) { return x(new Date(d.date)); })
  //  .attr('y', function(d) { return height - margin.top - margin.bottom - (height - margin.top - margin.bottom - y(d.GDPchange)) })
  .attr("y", function(d, i) { return (d.MonthlyChange) < 0 ? Y0() : Y(d); })
   .attr('width', (width/data.length -1))

    .attr("height", function(d, i) { return Math.abs( Y(d) - Y0() ); })
.attr("transform", "translate(" + -(width/data.length)*.5 + "," + 0 + ")")
	//.attr("height", 100)
	 .attr("opacity", 1);
	 

FederalEmployChart.append("g")
      .attr("class", "x axis zero")
      .call(xAxis2.tickFormat("").tickSize(0))
	 .attr("transform", "translate(0," + y(0) + ")")

	  	  
FederalEmployChart.append("path")
      .datum(data)
      .attr("class", "line")
      .attr("d", hourlyline)
	  .style('opacity', 1)
	 // .style('stroke', function(d) { return color(d.name) });
	  
	  
	  
FederalEmployChart.selectAll(".dotholder")
    .data(data.filter(function(d) { 
	
	return d.MovingAverage; }))
    .data(data)
	.enter().append('g')
	.attr('class','dotholder')
	
	//.attr('id', function(d, i)  { return 'holder' + i})
    //.enter()
	
	//hourly.selectAll(".dot")
	.append("circle")
    .attr("class", "dot")
	.attr("id", function(d,i) {return 'circle' + i})
	 // .attr("d", d.MovingAverage)
    .attr("cx", hourlyline.x())
    .attr("cy", hourlyline.y())
    .attr("r",2)
	 .style("display",function(d) { if (d.MovingAverage == ''  || d.MovingAverage == 'N/A' ) {return 'none'}})
	 .style("fill", "#524A48")
	.style('opacity', 1);	  
	  
//getting thos out of mouseover
		  var newid;
		  
		  
	        var datebars = FederalEmployChart.selectAll(".datebar")
                    .data(data)
                    .enter()
                    .append("g")
                    .attr('x', function (d) {
                        return x(d.date)
                    })
                    .attr('class', 'datebar')
					.attr("id", function(d,i) {return 'bar' + i})
					
                    .append('rect')
					 .attr("width", width / data.length)
                    .attr("height", height)
                 .style('fill', '#fff')
					// .style('stroke', 'green')
                  .style('opacity', .1)
				  .attr("title", function(d){
		   if (d.MonthlyChange != '') {return  parseFloat(d.MonthlyChange).toFixed(1) + "%"}
			else {return 'N/A'}
			})
                    .attr('x', function (d) {
                        return x(d.date)
                    })	  
   .on("mouseover", function(d) {
	   
	    var id = d3.select(this.parentNode).attr('id');
		newid = (id).replace("bar", "circle");
		  var tipformat = d3.time.format("%b %Y");	
	  var markerspot = d3.select(this).attr('x');
// var MonthlyText = d3.select(this).attr('title');
	
							 
							 
	  d3.select('#' + newid).transition()
	   .attr("r", 4.5)
    	.style("opacity", 1);	
	
	 
	 // var markerspot = d3.select(this).attr('cx');
								// when the mouse goes over a circle, do the following
			tooltip.transition()									// declare the transition properties to bring fade-in tooltip
				.duration(200)									// it shall take 200ms
				.style("opacity", function() {if (id == 'bar0' ){return 0 } else {return .9}});
							
			
			
			
	/*		tooltip.html('<span class= date>' + tipformat(d.date) + "</span>"  + "Monthly Change: <b>" + parseFloat(d.MonthlyChange).toFixed(1) + "%</b><br/>" + "Moving Average: <b>" +  parseFloat(d.MovingAverage).toFixed(1) + "%</b>")
			
		*/
		tooltip.html('<span class= date>&nbsp;&nbsp;&nbsp;&nbsp;' + tipformat(d.date) + '</span><span style ="color: #A2C0DE; font-size:2em;">&#9642; </span>Monthly Change: <b>' + commaSeparateNumber(d.MonthlyChange) + ' jobs</b><br/><span style ="color: #524A48; font-size:2em;">&#9642; </span>Moving Average: <b>' + commaSeparateNumber(d.MovingAverage)  + ' jobs</b>')
					
			/*
			
			
			
			
				// add the text of the tooltip as html 
				.style("left", function (){
					var xpos = d3.event.pageX;
					
					if (xpos < 625)
					{return (d3.event.pageX + 20) + "px"}
					else
					{return (d3.event.pageX -115 ) + "px"}
				})			// move it in the x direction 
				//.style("top", y(d.value) );
				.style("top", (d3.event.pageY - 28) + "px");
				*/
								
				if (d3.select('#FederalEmploy').attr("class" == "big")){
					
					  tooltip
					  .style("left", Math.max(0, d3.event.pageX+ 20) + "px")
                      .style("top", (d3.event.pageY + 20) + "px");
					
					};
  
		
		d3.select(".grid.horizontal")
		//hourly.append("line")
		.append("line")
      .attr("class", "markerline")
      .attr("x2", 0)
	  .attr("y2",  height)
	  //.attr ('stroke', 'red')
		.attr("transform", function() { 
		return  "translate(" + markerspot +"," + 0 + ")"
		});
		
    FederalEmployChart.selectAll(".markerline").sort(function () { // select the parent and sort the path's
       return 0;                             // a is the hovered element, bring "a" to the front
  });

					
			})													// 
		.on("mouseout", function(d) {							// when the mouse leaves a circle, do the following
		d3.selectAll(".markerline")
		.remove();
			 d3.select('#' + newid).transition()
	//.style("opacity", 0)	
	.attr("r", 2);	
	
			tooltip.transition()									// declare the transition properties to fade-out the div
				.duration(500)									// it shall take 500ms
				.style("opacity", 0);							// and go all the way to an opacity of nil
		});	
		
		
		
		//KEY STUFF
		
	
		
	var keyholder = d3.select("#FederalEmployChart g")
.append("g")
.attr('id', 'keyholder')
.attr("width",100)
.attr("height", 100)
//.attr("transform", "translate(height,width)")
.attr("transform", function() { return "translate( 0 ," + (height+40)  + ")"; })

var keyentry = keyholder.selectAll('.keyentry')
.data(thelines.filter(function(d){return d.name !== "RecessionDummy"}))	 
.enter().append("g")
      .attr("class", "keyentry")
	  .attr("id", function(d) { return (d.name).substring(0,3)})	
	  .attr("d", function(d) { return (d.name)}) 
	  
	 
	 // .attr("y2",  0)
	  //.attr("y2",  30)
	  .attr("transform", function(d,i) { 
	  var offset =  (i * 150);
	  return "translate(" + offset+ "," + 0 + ")"; }) 
     // .attr( 'stroke', function(d) { return color(d.name)}); 
	 
	 
	var keyline = keyentry.append("rect")
       .attr("width", function(d) { if (d.name =="MonthlyChange") {return 20} else {return 0}})
	   .attr("height", 10)
	   .attr('y', -5)
	  .attr("id",  function(d) { return color(d.name)})
//.style("fill", function(d) { return color(d.name); })
.style("fill", "#A2C0DE")
	
	  keyentry.append("line")
      .attr("class", "line")
	  .attr("id",  function(d) { return color(d.name)})
	   .attr("x2", function(d) { if (d.name =="MovingAverage") {return 30} else {return 0}})
	 // .style("stroke", function(d) { return color(d.name); })
	 .style("stroke", " #524A48");
	 
	 

keyentry.append("text")
.text(function (d) {if (d.name =="MovingAverage")
{ return "3-Month " + (d.name).replace(/([a-z])([A-Z])/g, '$1 $2')}
else 
{return (d.name).replace(/([a-z])([A-Z])/g, '$1 $2')}
	})
.attr("x", function(d) { if (d.name =="MovingAverage") {return 38} else {return 25}})		
.attr("y", 3);


var recessiontext = keyholder.append("text")
.text("Shaded areas indicate recession")
.attr("x", width)		
.attr("y", 3)
.attr("text-anchor", "end");
	
});//end csv stuff


if (bigwidth != 400) {
$('#explain').text("Monthly changes in the number of employees hired by the federal government.")
}




//swap this out for grabbing name of chart if time
$('#imagesave').attr("href", "http://www.brookings.edu/~/media/Multimedia/Interactives/2014/FiscalBarometer/images/Monthly Change in Fed Employment 9_30_14.jpg");
$('#source').html("Source: BLS; seasonally adjusted; excludes Postal Service and Census Workers");

}//end Federal employ