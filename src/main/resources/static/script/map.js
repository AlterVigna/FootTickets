$( document ).ready(function() {

    // Associate event click "map creation" to the button. (ONLY for admin).
    $("#createMapButton").click(function () {
        var numRow = $("#numRow").val();
        var numCol = $("#numCol").val();

        //createMapInDOM(numRow,numCol,'admin');
        drawMapInDOM(numRow,numCol);
        associateEventToTD('admin',["0_0"]);
        if (numRow > 0 && numCol > 0) {
            $("#confirmMap").css("display", "");
        }
    });


    // In case is present an empty table with id=mainTable, look if is present the attr asyncLoad.
    // In that case it will try to load asynchronously  the table via AJAX REQUEST.
    if ( $("#mainTable").attr("asyncLoad")=='true'){

        var auth=$("#mainTable").attr('auth');
        if (auth==undefined || auth.trim()==""){
            auth='none';
        }
        $(".container").append("<span id='msgLoad'> Loading the map ...</span> ");
        AJAX_CALL_READ_MAP_STATUS(auth);
    }

});

// Basic function for drawing seats element.
function drawMapInDOM(numRow,numCol){

    var mainTable = $("#mainTable");
    mainTable.empty();

    for (var i = 0; i < numRow; i++) {
        var newRow = $("<tr></tr>");

        for (var j = 0; j < numCol; j++) {
            var place = (i + 1) + '-' + (j + 1);
            var newCol = $("<td title='Available' row=" + i + " column=" + j + " sel='false' selectable='true'><i id='cell" + i + "_" + j + "' class='fas fa-chair fa-2x' ></i><br>" + place + "</td>");
            newRow.append(newCol);
        }
        mainTable.append(newRow);
    }
}


// If a place is in the list of LockedSeats it could not be selected.
// If a place is in the list of selectedSeats, this means the user previously has selected "in the same session" that place.
// listOfSelectedSeats is a subset of listOfLockedSeats.
function modifyMapLayoutInDOM(listOfLockedSeats,listOfSelectedSeats){

    // TODO Maybe add a summarization on the right.

    $('#mainTable td').each(function( index ) {

        var numRow = $(this).attr("row");
        var numCol = $(this).attr("column");
        var research = numRow + "_" + numCol;

        if (listOfLockedSeats.includes(research) && !listOfSelectedSeats.includes(research)) {
            // Any event is associated.
            $(this).addClass("locked");
            $(this).children(":first").removeClass("fa-chair");
            $(this).children(":first").addClass("fa-lock");
            $(this).attr("title", "Reserved");
            $(this).attr("sel","false");
            $(this).attr("selectable","false");

        }

        if (listOfSelectedSeats.includes(research)){
            $(this).addClass("reserved");
            $(this).children(":first").removeClass("fa-chair");
            $(this).children(":first").addClass("fa-clipboard-check");
            $(this).attr("title","Your reservation");
            $(this).attr("sel","true");
        }
    });
}


// Function to associate user event to single place.
// Type indicate who has the authority to make update on map.
function associateEventToTD(type){

    var mainTable = $("#mainTable");

    $('#mainTable td').each(function( index ) {

        var numRow=$(this).attr("row");
        var numCol=$(this).attr("column");

        if ($(this).attr("selectable")=='true'){

            // Associate events
            $(this).click(function () {

                if(type=='buyer'){
                    addTdEventForBuyer($(this));
                }
                if (type=='admin'){
                    addTdEventForAdmin($(this));
                }
            });
        }

    });
}


function addTdEventForBuyer(elem){

        var nRow=$(elem).attr("row");
        var nCol=$(elem).attr("column");
        var research=nRow+"_"+nCol;

        if ($(elem).attr("sel") == "false") {
            var id = $(elem).children(":first").attr("id");
            $(elem).addClass("selected");
            $(elem).attr("title","");
            $("#" + id).attr("class", "fas fa-hourglass-start");
            // startEffect(id);

            AJAX_CALL_RESERVE_A_PLACE('buyer','select',research);
            $(this).attr("sel", "true");
            //addHiddenElement($(this));
        } else {

            $(elem).removeClass("selected");
            $(elem).attr("sel", "false");
            $(elem).attr("title","Available");

            AJAX_CALL_RESERVE_A_PLACE('buyer','deselect',research);

            var id = $(elem).children(":first").attr("id");
            //stopEffect(id);
            $("#" + id).attr("class", "fas fa-chair fa-2x");
        }

        //stopEffect(id); // Questo qui deve essere messo in fondo alla chiamata AJAX in ogni caso.
}


function addTdEventForAdmin(elem){

    if ($(elem).attr("sel") == "false") {
        id = $(elem).children(":first").attr("id");
        $(elem).addClass("selected");
        $(elem).attr("title","");
        $("#" + id).attr("class", "fas fa-clipboard-check");
        $(elem).attr("sel", "true");

        addHiddenElement($(elem));
    } else {
        $(elem).attr("title","Available");
        $(elem).removeClass("selected");
        $(elem).attr("sel", "false");
        id = $(elem).children(":first").attr("id");

        $("#" + id).attr("class", "fas fa-chair fa-2x");

        removeHiddenElement($(elem));
    }



}







// This 2 function are reserved to admin in case he wants to reserve preliminary some places.
// The selected places are sent to the controller by hidden fields.
function addHiddenElement(elem){
    var nRow=$(elem).attr("row");
    var nCol=$(elem).attr("column");
    var id=nRow+"_"+nCol;
    var newHidden=$("<input type='hidden' id="+id+" name='selectedPlaces' value='"+id+"'>");
    $(".mapForm").append(newHidden);
}

function removeHiddenElement(elem){
    var nRow=$(elem).attr("row");
    var nCol=$(elem).attr("column");
    var idRem=nRow+"_"+nCol;
    $(".mapForm").find("#"+idRem).remove();

}


//  AJAX CALL

    // Asking information about global map.
    function AJAX_CALL_READ_MAP_STATUS(auth){
        var path= window.location.origin;
        path+="/rest/map"
        var JSON_ANSWER=[];
        $.get( path, {  })
            .done(function( JSON_ANSWER ) {

                $("#msgLoad").remove();
                answer=JSON_ANSWER["answer"];
                if (answer!="1"){
                    alert(JSON_ANSWER["messageDescription"])
                    // alert(" Hey someone has reserved that seat before you. Please select an other one!")
                }
                numRows=JSON_ANSWER["numRows"];
                numCols=JSON_ANSWER["numCols"];
                price=JSON_ANSWER["price"];
                listLocked=JSON_ANSWER["lockedPlaces"];
                listCurrentSelectedPlaces=JSON_ANSWER["currentSelectedPlaces"];

                $(".numRows").text(numRows);
                $(".numCols").text(numCols);
                $(".price").text(price);

                drawMapInDOM(numRows,numCols);
                modifyMapLayoutInDOM(listLocked,listCurrentSelectedPlaces);
                associateEventToTD(auth,listLocked,listCurrentSelectedPlaces);
            });
        return "";
    }

    // The place should be in the form of 'row_column'
    function AJAX_CALL_RESERVE_A_PLACE(auth,operation,placeSelected){

        var path= window.location.origin;
        path+="/rest/reserveSeat"
        var JSON_ANSWER=[];

        $.post( path, {placeSelected:placeSelected,operation:operation})
            .done(function( JSON_ANSWER ) {
                answer=JSON_ANSWER["answer"];
                if (answer!="1"){
                    alert(JSON_ANSWER["messageDescription"])
                   // alert(" Hey someone has reserved that seat before you. Please select an other one!")
                }
                numRows=JSON_ANSWER["numRows"];
                numCols=JSON_ANSWER["numCols"];
                price=JSON_ANSWER["price"];
                listLocked=JSON_ANSWER["lockedPlaces"];
                listCurrentSelectedPlaces=JSON_ANSWER["currentSelectedPlaces"];

                $(".numRows").text(numRows);
                $(".numCols").text(numCols);
                $(".price").text(price);

                drawMapInDOM(numRows,numCols);
                modifyMapLayoutInDOM(listLocked,listCurrentSelectedPlaces);
                associateEventToTD(auth,listLocked,listCurrentSelectedPlaces);


            });
        return "";


    }







// Not used for now.
// Graphical Effects (evaluate to implement)

var arrayEffectsEvent = [];
var inter;
var timeOut;

function startEffect(idCell) {

    inter = setInterval(function () {
        timeOut = setTimeout(function () {
            $("#" + idCell).attr("class", "fas fa-hourglass-half");
            timeOut = setTimeout(function () {
                $("#" + idCell).attr("class", "fas fa-hourglass-end");

                timeOut = setTimeout(function () {
                    $("#" + idCell).attr("class", "fas fa-hourglass-start");
                    //startEffect(idCell);
                }, 1000);
            }, 1000);
        }, 1000);
    }, 3000);

    arrayEffectsEvent[idCell] = inter;
}

function stopEffect(id) {
    clearInterval(inter);
    clearTimeout(timeOut);
    //clearTimeout(arrayEffectsEvent[id]);
    //delete arrayEffectsEvent[id];
}
