
$(window).load(function(){
        $('#welcomePage').modal('show');
        // $('#patientInfo').children().first().attr('class','table table-bordered table-hover');
	});

$(document).ready(function(){

  $("#star_5").click( function()
           {
             alert("5 STARS")
           }
  );

	//$('#patientInfo').find("table").attr('class','table table-bordered table-hover');
	// alert($('#patientInfo').find('.data'))
  $(function() {
      $("#subButton").click( function()
           {
             $('#welcomePage').modal('hide');
           }
      );
});
  $(function() {
      $("#new_patient_button").click( function()
           {
             $('#newPatient').modal('hide');
           }
      );
});

});



$(document).delegate("#patientInfo",'DOMSubtreeModified','DOMNodeInserted', function(event) {                                           
                    color()
 });

function color(){
  // $('#patientInfo').find("table").attr('class','table table-bordered table-hover');
  // $('#patientInfo').find("tr").first().css('background-color','#F5F5F5');

  var child = $('#patientInfo').find("table")
  child.attr('class','table table-bordered table-striped table-hover');
  child.find("tr").first().css('background-color','#d6d6d6');

  child.css('font-size','1.1em');
  child.css('font-weight','bold');

}


// var tooltipSpan = document.getElementById('hover_info');

window.onmousemove = function (e) {
    var x = e.clientX, y = e.clientY;
    Shiny.onInputChange("mouse_x", x+5);
    Shiny.onInputChange("mouse_y", y+5);
};

