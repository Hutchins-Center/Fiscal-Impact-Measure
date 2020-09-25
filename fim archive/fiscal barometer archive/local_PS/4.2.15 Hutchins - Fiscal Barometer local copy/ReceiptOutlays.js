		 
			 
		var ReceiptOutlays = function() {
				$('#tableholderholder').remove();
					
			
			
		 $('#ReceiptOutlays').append('<div id="tableholderholder"><span id="tabletitle"></span><div id="tableholder"></div><span class="sources" id="sourcesleft"></span><span class="sources" id="sourcesright"></span><div style="clear:both"></div></div></div>')
					
					d3.csv("csv/taxes_cboFinal.csv", function(data) {
    // the columns you'd like to display
    var columns = ["a", "b", "c"];
	
	d3.select("#tableholder").html('');

    var table = d3.select("#tableholder")
	.append("table")
	.attr("id","FedRecTable"),
        thead = table.append("thead"),
        tbody = table.append("tbody");


d3.select("#tabletitle").text("Federal Receipts and Outlays: Rolling 12-months")
.style("opacity", 0);


d3.select("#sourcesleft").html("Source: Hutchins Center calculations based on CBO data<br/>*Adjusted for timing shifts<br/>**Medicare outlays are net of offsetting receipts<br/>***Includes unemployment insurance,  TARP, and GSE outlays")
d3.select("#sourcesright").html("")
 
 
    // append the header row
   /* thead.append("tr")
        .selectAll("th")
        .data(columns)
        .enter()
        .append("th")
            .text(function(column) { return column; });*/

    // create a row for each object in the data
    var rows = tbody.selectAll("tr")
        .data(data)
        .enter()
        .append("tr");
//console.log(data);
    // create a cell in each row for each column
    var cells = rows.selectAll("td")
        .data(function(row) {
            return columns.map(function(column) {
                return {column: column, value: row[column]};
            });
        })
        .enter()
        .append("td")
            .text(function(d,i) {if (i == 2 && d.value != "Year-Over-Year Percentage Change" &&  d.value!= ''){ return d.value + '%'; } 
			else{ return d.value; }})
	/*		.style("font-size", function(d) { 
			if (d.value == "Total Outlays")
			return "2em"; });*/
			.attr("class", function(d) { 
			if (d.value == "Total Outlays*" || d.value ==  "Total Receipts")
			{return "cathead";}
			if (d.value == "Past 12 Months, Billions of $" || d.value ==  "Year-Over-Year Percentage Change" || d.value ==  "")
			{return "colhead";}
		 });
			
			
});

var bigwidth = $('#ReceiptOutlays').width();					
if (bigwidth != 400) {

$('#explain').text("Federal revenues and spending by category over the past 12 months.")
}
			


//swap this out for grabbing name of chart if time
$('#imagesave').attr("href", "http://www.brookings.edu/~/media/Multimedia/Interactives/2014/FiscalBarometer/images/Federal%20Receipts%209_30_14");
$('#source').html("");
}//end receipts outlays




			// JavaScript Document