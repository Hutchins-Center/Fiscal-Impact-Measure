	var FiscalImpetus = function () {
		
				
var bigwidth = $('#FiscalImpetus').width();
var bigheight = $('#FiscalImpetus').height();

$('.tooltip').remove();	
$('#content_0_pnlInteractive').append('<div class="tooltip" id="newtooltip"></div>');
//CHANGE FOR SC!!
//$('body').append('<div class="tooltip" id="newtooltip"></div>');
	
var margin = {top: bigheight * 0.16, right: bigwidth * 0.05, bottom: bigheight * .2, left: bigwidth * .08},


   /* width = 750 - margin.left - margin.right,
    height = 450 - margin.top - margin.bottom;*/
	width = bigwidth - margin.left - margin.right,
    height = bigheight - margin.top - margin.bottom;
	
function commaSeparateNumber(val){
    while (/(\d+)(\d{3})/.test(val.toString())){
      val = val.toString().replace(/(\d+)(\d{3})/, '$1'+','+'$2');
    }
    return val;
  }

var tooltip = d3.selectAll("#newtooltip").style("opacity", 0);

var parseDate = d3.time.format("%m/%d/%y").parse;
//var formatTime = d3.time.format("%e %B");

var x = d3.time.scale()
    .range([0, width]);
	

var y = d3.scale.linear()
    .range([height, 0]);

 // change to make in SC-new for bar chart
   function Y0() {
  return y(0);
}
//size y proportional to data
 function Y(d) {
 return y(d.Fiscal_Impact_bars);
}


var color = d3.scale.ordinal()
.range(["#524A48","#A2C0DE","#133570","#A89D95", "#524A48","#A2C0DE","#133570","#A89D95",  ]);
 //end change to make in SC-new for bar chart



var xAxis = d3.svg.axis()
    .scale(x)
    .orient("bottom")
	 .ticks(20);
	 
var xAxis2 = d3.svg.axis()
    .scale(x)	

var yAxis = d3.svg.axis()
    .scale(y)
    .orient("left")
	 .tickFormat(function(d) { return d});


var line = d3.svg.line()
 //   .interpolate("basis")
    .x(function(d) { return x(d.date); })
    .y(function(d) { return y(d.stuff); });
	
var linezero = d3.svg.line()
    .interpolate("basis")
    .x(function(d) { return x(d.date); })
    .y(function(d) { return y(200); });
	
//var tooltip = d3.selectAll(".tooltip").style("opacity", 0);
var tooltip = d3.selectAll("#newtooltip").style("opacity", 0);
	



var FiscalImpetusChart = d3.select("#FiscalImpetus").append("svg")
    .attr("width", width + margin.left + margin.right)
    .attr("height", height + margin.top + margin.bottom)
	 .attr("id", "FiscalImpetusChart")
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


	
	
	var data = {},
	duration= 500;
	

var data =	d3.csv("csv/fiscal_iFinal.csv", function(error, data) {
//var data =	d3.csv("csv/fiscal_iFinal_Dec14.csv", function(error, data) {
  color.domain(d3.keys(data[0]).filter(function(key) { return key !== "date" && key!== "RecessionDummy"; }));


    var mynewdataset = 
   data.map(function(d) {
        return {date: d.date, FiscalIpmact: d.Fiscal_Impact};
      });

  
  
  data.forEach(function(d) {
    d.date = parseDate(d.date);
  });

  var thelines = color.domain().map(function(name) {
    return {
      name: name,
      values: data.map(function(d) {
        return {date: d.date, stuff: +d[name]};
      })
    };
  });
  

 //new for SC- shift grid over since centering bars and dots???
  x.domain([d3.time.month(data[0].date), d3.time.month.offset((data[data.length - 1].date), 1)])

  y.domain([
    d3.min(thelines, function(c) { return d3.min(c.values, function(v) { return v.stuff; }); }),
    d3.max(thelines, function(c) { return d3.max(c.values, function(v) { return v.stuff; }); })
  ])
  .nice();
  
  
  
 
    
  
  FiscalImpetusChart.selectAll('g > onepath')
	  .data(data)
	  .enter()
		.append("rect")		     
    .attr("x", function(d) { return x(d.date)})  
    .attr("y", 0)        
    .attr("height", function(d, i) {
		if (d.RecessionDummy == 1 && i !== (data.length-1)) {return height}
		else {return 0}})  
    .attr("width", width / data.length +.5)
	.attr("fill", "#EDEBE2"); 
  
FiscalImpetusChart.insert("g", ".bars")         
        .attr("class", "grid horizontal")
        .call(d3.svg.axis().scale(y)
            .orient("left")
            .tickSize(-(width), 0, 0)
            .tickFormat(""));
			
FiscalImpetusChart.insert("g", ".bars")         
        .attr("class", "grid vertical")
		 .attr("transform", "rotate(-90)")
        .call(d3.svg.axis().scale(x)
		.ticks(20)
            .orient("left")
            .tickSize((height), 0, 0)
            .tickFormat(""));

FiscalImpetusChart.append("g")
      .attr("class", "x axis")
      .attr("transform", "translate(0," + height + ")")
      .call(xAxis);
	  
	  FiscalImpetusChart.append("g")
      .attr("class", "x axis zero")
      .call(xAxis2.tickFormat("").tickSize(0))
	 .attr("transform", "translate(0," + y(0) + ")")


FiscalImpetusChart.append("g")
      .attr("class", "y axis")
       .call(yAxis)
	   .append("text")
      .attr("transform", "rotate(-90)")
      .attr("y", -40)
	  .attr("x", 0 - height/2)
	  .attr("class", "axislabel")
	.style("text-anchor", "middle")
      .text("Percentage Points");
	  


	  
  //change to make in SC new- barchart
	var barchart =   FiscalImpetusChart.append("g")
	.attr("class", "barchart")
	
	barchart.selectAll("#chartspace")
    .data(data)
  .enter().append('rect')
     .attr("class", function(d, i) { 
	 return d.Fiscal_Impact_bars < 0 ? "bar negative" : "bar positive"; })
	 .attr("id", function(d,i) {return 'chartbar' + i})
    .attr('x', function(d) { return x(new Date(d.date)); })
  .attr("y", function(d, i) { return (d.Fiscal_Impact_bars) < 0 ? Y0() : Y(d); })
   .attr('width', (width/data.length -1))

    .attr("height", function(d, i) { return Math.abs( Y(d) - Y0() ); })
	//new-Shift so bars align center with dots
	 .attr("transform", "translate(" + -(width/data.length)*.5 + "," + 0 + ")")
	 .attr("opacity", 1);
	 //end change to make in SC	 
//change to make in SC
 thelines= thelines.filter(function(d) {return d.name != 'Fiscal_Impact_bars'});
//change to make in SC




var oneline = FiscalImpetusChart.selectAll(".oneline")
      .data(thelines)
      .enter().append("g")
      .attr("class", "oneline")
	  .attr("id", function(d) { return (d.name)})	
	  .attr("d", function(d) { return (d.name)})  
      .style('stroke', function(d) { return color(d.name)}) 
	  .style('fill', function(d) { return color(d.name)});  

var onepath = oneline.append("path")
      .attr("class", "line")
	  .attr("id",  function(d) { return (d.name)})
	  .attr("d", function(d) { return line(d.values)})
	 .style("stroke", function(d) { return color(d.name); })


oneline.selectAll('g > onepath')
	  .data(function(d) {return d.values})
	  .enter()
	  .append('circle')
	  .attr('class', 'dot')
	  .attr("id", function(d,i) {return 'circle' + i})
	  /*.attr("id", function(d) { return (d.name)})*/	
	  .attr('cx', function(d) { return x(d.date)})
	  .attr('cy', function(d) { return y(d.stuff)})
	 // .style('fill', function(d) { 
	 // return color(d.name) })
	 // .style("fill", function(d){return d3.rgb(d.color).darker(1);})
	  .attr('r',2)
	  .style("opacity", 1)
	  
	  
var newid;
		  
		  
	        var datebars = FiscalImpetusChart.selectAll(".datebar")
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
	   var tipformat = d3.time.format("%b %Y");	
       var name = d3.select(this.parentNode).attr("id");
	   var markerspot = d3.select(this).attr('x');
	   var color = d3.select(this).style('stroke');
	     var id = d3.select(this.parentNode).attr('id');
		newid = (id).replace("bar", "circle");
		//var tipposer= d3.select(this.parentNode).attr('x');
		//console.log (markerspot);
	  
	 d3.selectAll('#' + newid).transition()		
	     .style("opacity", 1)	
	     .attr("r", 4.5);	

	
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


	
	tooltip.html('<span class= date>'+ dateToQuarter + '</span><span style ="color: #A2C0DE; font-size:1.5em;">&#9642; </span>Actual: <b>'+ (parseFloat(d.Fiscal_Impact_bars).toFixed(2)) + '%<br/><span style ="color:#524A48; font-size:1.5em;">&#9642; </span>Moving Average: <b>' + (parseFloat(d.Fiscal_Impact).toFixed(2)) + '%</b>');	
				
				
		

				
				
				
				if (d3.select('#FiscalImpetus').attr("class" == "big")){
					
					  tooltip
					  .style("left", Math.max(0, d3.event.pageX+ 20) + "px")
                      .style("top", (d3.event.pageY + 20) + "px");
					
					};
				

			/*tooltip
			
			
				.style("left", function (){
					
					var xpos = d3.event.pageX;
				//console.log(xpos);
				//console.log(markerspot);
					if (xpos < 525)
					
					//{return (d3.event.pageX + 15) + "px"}
					//{return (d3.select(this).attr('x') + 15) + "px"}
					{return (markerspot + 200)  + "px"}
					//else
					//{return (d3.event.pageX -100 ) + "px"}
				})				// move it in the x direction 
				.style("top", (d3.event.pageY) + "px");
				//.style("border-top", function() { return "2px solid" +  color});	// move it in the y direction
				
				
				d3.select('tooltip').attr('left', function(d) {
					//console.log(d3.select(this).attr('left'));
					});*/
				
				
d3.select(".grid.horizontal")
		.append("line")
      .attr("class", "markerline")
      .attr("x2", 0)
	  .attr("y2",  height)
	  .attr ('stroke', 'red')
		.attr("transform", function() { 
		return  "translate(" + markerspot +"," + 0 + ")"
		});
		
			})												




.on("mouseout", function(d) {
			d3.selectAll(".markerline")
		.remove();
		
d3.selectAll('#' + newid).transition()	
	.attr("r", 2);	
	
		tooltip.transition()									
				.duration(500)									
				.style("opacity", 0);							
			
		});		
  


var keyholder = d3.select("#FiscalImpetusChart g")
.append("g")
.attr('id', 'keyholder')
.attr("width",100)
.attr("height", 100)
.attr("transform", function() { return "translate( 0 ," + (height+40)  + ")"; })




keyholder.append("g") 
	 .attr("class", "keyentry")
	  .append("rect")
	  .attr("width", 20)
	   .attr("height", 10)
	   .attr("x", 0)
	   .attr("y",-6)
	   .style("fill", "#A2C0DE")
	   .append("text")
	   	.attr("y", 0);
		
		keyholder.append("g") 
	 .attr("class", "keyentry")
	  .append("rect")
	  .attr("width", 30)
	   .attr("height", 2)
	   .attr("x", 80)
	   .attr("y",-2)
	   .style("fill", "#524A48")
	   .append("text")
	   	.attr("y", 0);
		
		
keyholder.append("text")
.text('Actual')
.attr("x", 25)		
.attr("y", 3);


keyholder.append("text")
.text('Moving Average')
.attr("x", 115)		
.attr("y", 3);

var recessiontext = keyholder.append("text")
.text("  Shaded areas indicate recession")

.attr("x", width)		
.attr("y", 3)
.attr("text-anchor", "end");
});//end within csv




if (bigwidth != 400) {
$('#explain').html('The fiscal impact measure shows how much federal, state, and local government taxes and spending added to or subtracted from the overall pace of economic growth.  Between 2008 and 2011, fiscal impact was positive, indicating that government policy was stimulative; in recent years, it has been negative, indicating restraint. (For more detail on how this measure was constructed and how to interpret it, see <a style="font-weight: bold" href="http://www.brookings.edu/research/papers/2014/10/01-hutchins-center-fiscal-impact-measure-sheiner" target="_blank">our methodology</a>.)');
}


		


$('#imagesave').attr("href", "http://www.brookings.edu/~/media/Multimedia/Interactives/2014/FiscalBarometer/images/Fiscal_Impact9_30_14.png");
$('#source').html("Source: Hutchins Center calculations based on BEA data");

		
		
		
		}//end new fiscal Impetus