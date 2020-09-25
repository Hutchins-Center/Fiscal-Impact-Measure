	
var FedSpend= function() {
				
				$('#FedSpendChart').remove();
			
				
var bigwidth = $('#FedSpend').width();
var bigheight = $('#FedSpend').height();			
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

var parseDate = d3.time.format("%m/%d/%Y").parse;



var color = d3.scale.ordinal()


//.range(["#A89D95","#133570","#A2C0DE","#524A48","#A89D95","#133570","#A2C0DE","#524A48",]);

.range(["#A89D95","#1A4796","#A2C0DE","#524A48","#A89D95","#1A4796","#A2C0DE","#524A48",]);

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
.defined(function(d) { return d.stuff})
//.defined(function(d) { return d.value != null; })
 //.interpolate("basis")
    .x(function(d) { return x(d.date); })
    .y(function(d) { return y(d.stuff); });
	
/*	var hourlylinezero = d3.svg.line()
.defined(function(d) { return d.MovingAverage != null; })
 //.interpolate("basis")
    .x(function(d) { return x(d.date); })
    .y(function(d) { return y(0); });*/


var FedSpendChart = d3.select("#FedSpend").append("svg")
   .attr('id', 'FedSpendChart')
    .attr("width", width + margin.left + margin.right)
    .attr("height", height + margin.top + margin.bottom)
  .append("g")
    .attr("transform", "translate(" + margin.left + "," + margin.top + ")")

.on("mouseover",function() {
			tooltip.transition()									// declare the transition properties to bring fade-in div
				.duration(500)									// it shall take 200ms
				.style("opacity", .9);	
})
.on("mouseout",function() {
			tooltip.transition()									// declare the transition properties to bring fade-in div
				.duration(500)									// it shall take 200ms
				.style("opacity", .0);	
});


	var data = {},
	duration= 500;
	
var data =	d3.csv("csv/long_categoryFinal.csv", function(error, data) {
 color.domain(d3.keys(data[0]).filter(function(key) { return key !== "date" && key !=="RecessionDummy"; }));

  data.forEach(function(d) {
    d.date = parseDate(d.date);
  });

  var thelines = color.domain().map(function(name) {	  
    return {
      name: name, 
      values: data.map(function(d) {
		 
        return {date: d.date, stuff: +d[name] };
      })
    };
  });

  x.domain(d3.extent(data, function(d) { return d.date; }));
  
// console.log(thelines);

  y.domain([
    d3.min(thelines, function(c) { return d3.min(c.values, function(v) { return v.stuff }); }),
    d3.max(thelines, function(c) { return d3.max(c.values, function(v) { return v.stuff; }); })
  ])
  .nice();
  
	// new for SC- recession bars
  FedSpendChart.selectAll('g > onepath')
	  .data(data)
	  .enter()
		.append("rect")
		       // attach a rectangle
    .attr("x", function(d) { return x(d.date)})         // position the left of the rectangle
    .attr("y", 0)          // position the top of the rectangle
    .attr("height", function(d, i) {
		if (d.RecessionDummy == 1 && i !== (data.length-1)) {return height}
		else {return 0}})    // set the height
    .attr("width", width / data.length *1.1)
	.attr("fill", "#EDEBE2"); 	
//end new for recession bars	

  
 FedSpendChart.insert("g", ".bars")         
        .attr("class", "grid horizontal")
        .call(d3.svg.axis().scale(y)
            .orient("left")
            .tickSize(-(width), 0, 0)
            .tickFormat(""));
			
FedSpendChart.insert("g", ".bars")         
        .attr("class", "grid vertical")
		 .attr("transform", "rotate(-90)")
        .call(d3.svg.axis().scale(x)
            .orient("left")
            .tickSize((height), 0, 0)
            .tickFormat(""));

FedSpendChart.append("g")
      .attr("class", "x axis")
      .attr("transform", "translate(0," + height + ")")
      .call(xAxis);

FedSpendChart.append("g")
      .attr("class", "y axis")
       .call(yAxis)
	   .append("text")
      .attr("transform", "rotate(-90)")
      .attr("y", -40)
	  .attr("x", 0 - height/2)
	  .attr("class", "axislabel")
	.style("text-anchor", "middle")
     // .text("% GDP");
	  
	  var passoffcolor;

  var oneline = FedSpendChart.selectAll(".oneline")
      .data(thelines)
      .enter().append("g")
      .attr("class", "oneline")
	  .attr("id", function(d) { return (d.name)})	
	  .attr("d", function(d) { return (d.name)})  
      .attr('stroke', function(d) { return color(d.name)});  

 var onepath = oneline.append("path")
      .attr("class", "line")
	  .attr("id",  function(d) { return (d.name)})
	  .attr("d", function(d) { return hourlyline(d.values)})
	 .style("stroke", function(d) { return color(d.name); })
	  .style("stroke-dasharray", function(d) { 
	  if (d.name == "healthproj" || d.name == "ssproj" || d.name == "interestproj" || d.name == "otherproj" )
	  {return 4;}
	  else
	  {return ''}
	   })

oneline.selectAll('g > onepath')
	  .data(function(d) {return d.values})
	  .enter()
	  .append('circle')
	  .attr('class', 'dot')
	  .attr("id", function(d,i) {return 'circle' + i})
	  /*.attr("id", function(d) { return (d.name)})*/	
	  .attr('cx', function(d) { return x(d.date)})
	  .attr('cy', function(d) { return y(d.stuff)})
	    .attr("r", function(d) { 
	  if (d3.select(this.parentNode).attr("id") == "healthproj" || d3.select(this.parentNode).attr("id") == "ssproj" || d3.select(this.parentNode).attr("id") == "interestproj" || d3.select(this.parentNode).attr("id") == "otherproj")
	  {return 0;}
	  else
	  {return 2}
	   })
	  	 .style("display",function(d) { if (d.stuff == '') {return 'none'}})
	  .style("opacity", 1)
		.attr('stroke', 'none') 
		 .style("fill", function(d) { 
		 
		 passoffcolor = d3.select(this.parentNode).attr("stroke")
		 {return passoffcolor }
		 }
		 )
		
		
	  
	  
  var newid;
  	var revswitch;
	
		  
		  
	        var datebars = FedSpendChart.selectAll(".datebar")
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
                    .style('opacity', 0)
                    .attr('x', function (d) {
                        return x(d.date)
                    })	  
	.on("mouseover", function(d) {
	   var tipformat = d3.time.format("%Y");	
       var name = d3.select(this.parentNode).attr("id");
	   var markerspot = d3.select(this).attr('x');
	   var color = d3.select(this).style('stroke');
	     var id = d3.select(this.parentNode).attr('id');
		newid = (id).replace("bar", "circle");
		
		
		
		
		//actuals
		var healthactj;
		  if(d.healthactj != '' )
	  { healthactj = '<span style ="color: #A89D95; font-size:2em;">&#9642;&nbsp;</span>Health: <b>' + d.healthactj + "%</b><br/>"}
	  else
	  {healthactj = ''};
	
	
	  //this was something to prevent double values on overlaps but too tired to remember
	  	var ssact;
		  if(d.ssact != '' )
	  { ssact = '<span style ="color: #133570; font-size:2em;">&#9642;&nbsp;</span>Social Security: <b>' + d.ssact + "%</b><br/>"}
	  else
	  {ssact= ''};
	  
		var interestact;
		  if(d.interestact != '' )
	  { interestact = '<span style ="color: #A2C0DE; font-size:2em;">&#9642;&nbsp;</span>Net Interest: <b>' + d.interestact + "%</b><br/>"}
	  else
	  {interestact = ''};
	
	
		var otheract;
		  if(d.otheract != '')
	  { otheract= '<span style ="color: #524A48; font-size:2em;">&#9642;&nbsp;</span>Everything Else: <b>' + d.otheract + "%</b><br/>"}
	  else
	  {otheract = ''};
	
	  
	  
	  //projecteds
	
	  
	  	var healthproj;
		  if(d.healthproj != '' &&  d.healthactj == '')
	  { healthproj= '<span style ="color: #A89D95; font-size:2em;">&#9642; </span>Health (projected): <b>' + d.healthproj + "%</b><br/>"}
	  else
	  {healthproj = ''};
	
	
	  //this was something to prevent double values on overlaps but too tired to remember
	  	var ssproj
		  if(d.ssproj != '' && d.ssact== '')
	  { ssproj= '<span style ="color: #133570; font-size:2em;">&#9642; </span>Social Security (projected): <b>' + d.ssproj + "%</b><br/>"}
	  else
	  {ssproj= ''};
	  
		var interestproj;
		  if(d.interestproj != '' && d.interestact == '')
	  { interestproj = '<span style ="color: #A2C0DE; font-size:2em;">&#9642; </span>Net Interest (projected): <b>' + d.interestproj + "%</b><br/>"}
	  else
	  {interestproj = ''};
	
	
		var otherproj
		  if(d.otherproj != '' && d.otheract == '')
	  { otherproj= '<span style ="color: #524A48; font-size:2em;">&#9642; </span>Everything Else (projected): <b>' + d.otherproj + "%</b><br/>"}
	  else
	  {otherproj = ''};
	
	  
	  
	
	
	  
	 d3.selectAll('#' + newid).transition()		
	     .style("opacity", 1)	
	     .attr("r", 4.5);	
	
	  				// when the mouse goes over a circle, do the following
			tooltip.transition()									// declare the transition properties to bring fade-in div
				.duration(200)									// it shall take 200ms
				.style("opacity", .9);							// and go all the way to an opacity of .9
	/*		div.html( tipformat(d.date) + "<br/>" +"thousands of units - " + name + ": <b>" + commaSeparateNumber(d.stuff) + "</b>")	// add the text of the tooltip as html 
			
			*/
		
				
				
					tooltip.html( '<span class="date">&nbsp;&nbsp;&nbsp;&nbsp;' + tipformat(d.date) + '</span>' + healthactj + healthproj  + ssact + ssproj + interestact + interestproj  + otheract + otherproj);
				
				
				
					if (d3.select('#FedSpend').attr("class" == "big")){
					
					  tooltip
					  .style("left", Math.max(0, d3.event.pageX+ 20) + "px")
                      .style("top", (d3.event.pageY + 20) + "px");
					
					};
				
	

				
				/*
				tooltip.style("left", function (){
					var xpos = d3.event.pageX;
				
					if (xpos < 525)
					{return (d3.event.pageX + 20) + "px"}
					else
					{return (d3.event.pageX -250 ) + "px"}
				})				// move it in the x direction 
				.style("top", (d3.event.pageY - 28) + "px");*/
				
				
				
				//.style("border-top", function() { return "2px solid" +  color});	// move it in the y direction
				
				d3.select(".grid.horizontal")
		.append("line")
      .attr("class", "markerline")
      .attr("x2", 0)
	  .attr("y2",  height)
	  .attr ('stroke', 'red')
		.attr("transform", function() { 
		return  "translate(" + markerspot +"," + 0 + ")"
		});
		
			})													// 
		.on("mouseout", function(d) {
			d3.selectAll(".markerline")
		.remove();
		
d3.selectAll('#' + newid).transition()	
	    .attr("r", function(d) { 
	  if (d3.select(this.parentNode).attr("id") == "healthproj" || d3.select(this.parentNode).attr("id") == "ssproj" || d3.select(this.parentNode).attr("id") == "interestproj" || d3.select(this.parentNode).attr("id") == "otherproj")
	  {return 0;}
	  else
	  {return 2}
	   })
	   
		});





		//KEY STUFF
		

	var keyholder = d3.select("#FedSpendChart g")
.append("g")
.attr('id', 'keyholder')
.attr("width",100)
.attr("height", 100)
//.attr("transform", "translate(height,width)")
.attr("transform", function() { return "translate( 0 ," + (height+40)  + ")"; })
	   

var keyentry = keyholder.selectAll('.keyentry')
.data([thelines[4],thelines[5],thelines[6],thelines[7]])
.enter().append("g")
      .attr("class", "keyentry")
	  .attr("id", function(d) { return (d.name).substring(0,3)})	
	  .attr("d", function(d) { return (d.name)}) 
	 // .attr("y2",  0)
	  //.attr("y2",  30)
	  .attr("transform", function(d,i) { 
	  var offset =  (i * 122);
	  return "translate(" + offset+ "," + 0 + ")"; }) 
	  	    .attr("opacity", function(d) { 
	  if (d.name == "healthproj" || d.name == "ssproj" || d.name == "interestproj" || d.name == "otherproj")
	  {return 0;}
	  else
	  {return 1}
	   });

     // .attr( 'stroke', function(d) { return color(d.name)}); 

	
	  keyentry.append("line")
      .attr("class", "line")
	  .attr("id",  function(d) { return color(d.name)})
	   .attr("x2", 30)
	  .style("stroke", function(d) { return color(d.name); })
	  	    .attr("opacity", function(d) { 
	   if (d.name == "healthproj" || d.name == "ssproj" || d.name == "interestproj" || d.name == "otherproj")
	  {return 0;}
	  else
	  {return 1}
	   });

	  
	  
keyentry.append("text")
.text(function (d) {if (d.name == 'healthactj'){return "Health"}
					else if(d.name == 'ssact') {return  'Social Security'}
					else if(d.name == 'interestact') {return  'Net Interest'}
					else if(d.name == 'otheract') {return  'Everything Else'}
 
else{return d.name}
})
.attr("x", 35 )
.attr("y", 3 );

//.attr("x",  function (d,i) {
//	return (35 +(i * 150))});		
	

	 
var recessiontext = keyholder.append("text")
.text("Shaded areas indicate recession")
.attr("x", width)		
.attr("y", 3)
.attr("text-anchor", "end");

});//end csv stuff


if (bigwidth != 400) {
$('#explain').text("Actual and projected federal spending by category, relative to the size of the economy.")
}



//swap this out for grabbing name of chart if time
$('#imagesave').attr("href", "http://www.brookings.edu/~/media/Multimedia/Interactives/2014/FiscalBarometer/images/Federal%20Spending%20by%20Catgeory%209_30_14");
$('#source').html("Source: CBO; dotted line indicates projected values");


}//end local employ