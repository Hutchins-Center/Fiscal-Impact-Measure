	
var Construction = function() {
				
				$('#ConstructionChart').remove();
				
				
				
var bigwidth = $('#Construction').width();
var bigheight = $('#Construction').height();			
var margin;

$('.tooltip').remove();	
$('#content_0_pnlInteractive').append('<div class="tooltip" id="newtooltip"></div>');	
	
	if (bigwidth == 400) {
margin  = {top: bigheight * 0.25, right: bigwidth * 0.05, bottom: bigheight * .15, left: bigwidth * .08};}
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


var parseDate = d3.time.format("%d-%b-%y").parse;



var color = d3.scale.ordinal()
/*.range(["#A2C0DE","#524A48","#133570","#A89D95", "#524A48","#A2C0DE","#133570","#A89D95",  ]);
*/
.range(["#A2C0DE"]);


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
.defined(function(d) { return d.Spending})
//.defined(function(d) { return d.value != null; })
 //.interpolate("basis")
    .x(function(d) { return x(d.date); })
    .y(function(d) { return y(d.Spending); });
	
/*	var hourlylinezero = d3.svg.line()
.defined(function(d) { return d.MovingAverage != null; })
 //.interpolate("basis")
    .x(function(d) { return x(d.date); })
    .y(function(d) { return y(0); });*/


var ConstructionChart = d3.select("#Construction").append("svg")
   .attr('id', 'ConstructionChart')
    .attr("width", width + margin.left + margin.right)
    .attr("height", height + margin.top + margin.bottom)
  .append("g")
    .attr("transform", "translate(" + margin.left + "," + margin.top + ")")
	.on("mouseover",function() {
			tooltip.transition()									
				.duration(500)								
				.style("opacity", .9);	
})
.on("mouseout",function() {
			tooltip.transition()								
				.duration(500)									
				.style("opacity", .0);	
});




//d3.csv("csv/Inflation3c.csv", function(error, data) {
		var data =	d3.csv("csv/jobs_structuresFinal.csv", function(error, data) {


color.domain(d3.keys(data[0]).filter(function(key) { return key !== "date"  && key !=="RecessionDummy"}));

/*data = data.filter(function(d, i) 
    { 

            if (d["MovingAverage"] != "N/A") 
            { 
                return d; 
            }
			else
			{ return ''} 

        })*/

	
	
  data.forEach(function(d) {
	  //tell it which column to get for date
    d.date = parseDate(d.date);
	

  });
  
  var thelines = color.domain().map(function(name) {
    return {
      name: name,
      values: data.map(function(d) {
        return {date: d.date, stuff: +d[name]};
      })
    };
  })


		
	


  x.domain(d3.extent(data, function(d) { return d.date; }));
/*  y.domain(d3.extent(data, function(d) { return d.MonthlyChange }))
  .nice();*/
  
  y.domain([
    d3.min(thelines, function(c) { return d3.min(c.values, function(v) { return v.stuff; }); }),
    d3.max(thelines, function(c) { return d3.max(c.values, function(v) { return v.stuff; }); })
  ])
  .nice();


// new for SC- recession bars
ConstructionChart.selectAll('g > onepath')
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

  


ConstructionChart.insert("g", ".bars")         
        .attr("class", "grid horizontal")
        .call(d3.svg.axis().scale(y)
            .orient("left")
			.ticks(5)
            .tickSize(-(width), 0, 0)
            .tickFormat(""));
			
ConstructionChart.insert("g", ".bars")         
        .attr("class", "grid vertical")
		 .attr("transform", "rotate(-90)")
        .call(d3.svg.axis().scale(x)
            .orient("left")
            .tickSize((height), 0, 0)
            .tickFormat(""));


ConstructionChart.append("g")
      .attr("class", "x axis")
      .attr("transform", "translate(0," + height + ")")
      .call(xAxis);



	   ConstructionChart.append("g")
      .attr("class", "y axis")
      .call(yAxis)
    .append("text")
      .attr("transform", "rotate(-90)")
      .attr("y", -34)
	  .attr("x", 0 - height/2)
	  .attr("class", "axislabel")
	.style("text-anchor", "middle")
   // .text("Dollars (billions)");
	  
	  





ConstructionChart.append("g")
      .attr("class", "x axis zero")
      .call(xAxis2.tickFormat("").tickSize(0))
	 .attr("transform", "translate(0," + y(0) + ")")

	  	  
ConstructionChart.append("path")
      .datum(data)
      .attr("class", "line")
      .attr("d", hourlyline)
	  .style('opacity', 1)
	  .style('stroke', function(d) { return color(d.name) })
	   .attr('stroke', function(d) { return color(d.name) });
	  
	  
	  
ConstructionChart.selectAll(".dotholder")
    .data(data.filter(function(d) { 

	return d.Spending; }))
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
    .attr("r",3)
	 .style("display",function(d) { if (d.Spending == '') {return 'none'}})
	 .style('fill', function(d) { return color(d.name) })
	.style('opacity', 1);	  
	  
//getting thos out of mouseover
		  var newid;
		  
		  
	        var datebars = ConstructionChart.selectAll(".datebar")
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
		   if (d.Spending != '') {return  parseFloat(d.Spending).toFixed(1) + "%"}
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
 var MovingText = d3.select(this).attr('title');


//change to make in SC
	
	var dateToQuarter;
		
		if(tipformat(d.date).substring(0, 3) == "Jan") 
	{dateToQuarter = tipformat(d.date).substring(3, 8) + "-Q1"}
	else if(tipformat(d.date).substring(0, 3) == "Apr") 
	{dateToQuarter = tipformat(d.date).substring(3, 8) + "-Q2"}
	else if(tipformat(d.date).substring(0, 3) == "Jul") 
	{dateToQuarter = tipformat(d.date).substring(3, 8) + "-Q3"}
	else if(tipformat(d.date).substring(0, 3) == "Oct") 
	{dateToQuarter = tipformat(d.date).substring(3, 8) + "-Q4"}
	else
	{dateToQuarter = tipformat(d.date)};
	
							 
							 
	  d3.select('#' + newid).transition()
	   .attr("r", 4.5)
    	.style("opacity", 1);	
	
	 
	 // var markerspot = d3.select(this).attr('cx');
		//console.log(markerspot);						// when the mouse goes over a circle, do the following
			tooltip.transition()									// declare the transition properties to bring fade-in tooltip
				.duration(200)									// it shall take 200ms
				.style("opacity", .9);							// and go all the way to an opacity of .9
			tooltip.html('<span class= date>' + dateToQuarter + "</span>"  + "Spending: <b>$" + (d.Spending) + " billion</b>");
			
			if (d3.select('#FiscalImpetus').attr("class" == "big")){
					
					  tooltip
					  .style("left", Math.max(0, d3.event.pageX+ 20) + "px")
                      .style("top", (d3.event.pageY + 20) + "px");
					  };
					  
			/*// add the text of the tooltip as html 
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
		
    ConstructionChart.selectAll(".markerline").sort(function () { // select the parent and sort the path's
       return 0;                             // a is the hovered element, bring "a" to the front
  });

					
			})													// 
		.on("mouseout", function(d) {							// when the mouse leaves a circle, do the following
		d3.selectAll(".markerline")
		.remove();
			 d3.select('#' + newid).transition()
	//.style("opacity", 0)	
	.attr("r",3);	
	
			tooltip.transition()									// declare the transition properties to fade-out the div
				.duration(500)									// it shall take 500ms
				.style("opacity", 0);							// and go all the way to an opacity of nil
		});	
		
		
		
		//KEY STUFF
	var keyholder = d3.select("#ConstructionChart g")
.append("g")
.attr('id', 'keyholder')
.attr("width",100)
.attr("height", 100)
//.attr("transform", "translate(height,width)")
.attr("transform", function() { return "translate( 0 ," + (height+40)  + ")"; })


var recessiontext = keyholder.append("text")
.text("Shaded areas indicate recession")
.attr("x", width)		
.attr("y", 3)
.attr("text-anchor", "end");


});//end csv stuff

if (bigwidth != 400) {
$('#explain').text("State and local government total investment in structures (such as highways and schools), adjusted for inflation.")
}
//swap this out for grabbing name of chart if time
$('#imagesave').attr("href", "http://www.brookings.edu/~/media/Multimedia/Interactives/2014/FiscalBarometer/images/State_Local_Spending_Structures9_30_14.png");
$('#source').html("Source: BEA");

}