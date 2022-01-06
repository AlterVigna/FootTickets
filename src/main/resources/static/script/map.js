$( document ).ready(function() {

    $("#createMapButton").click(function () {

        var numRow = $("#numRow").val();
        var numCol = $("#numCol").val();

        createMapInDOM(numRow,numCol);
        //startEffect();
    });
});

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


    function createMapInDOM(numRow, numCol) {

        var mainTable = $("#mainTable");

        mainTable.empty();

        for (var i = 0; i < numRow; i++) {
            var newRow = $("<tr></tr>");

            for (var j = 0; j < numCol; j++) {
                var place = (i + 1) + '-' + (j + 1);
                var newCol = $("<td row=" + i + " column=" + j + " sel='false'><i id='cell" + i + "_" + j + "' class='fas fa-chair fa-2x'></i><br>" + place + "</td>");

                newCol.click(function () {
                    if ($(this).attr("sel") == "false") {
                        var id = $(this).children(":first").attr("id");
                        $(this).addClass("selected");
                        $("#" + id).attr("class", "fas fa-hourglass-start");
                        startEffect(id);
                        $(this).attr("sel", "true");
                    } else {
                        $(this).removeClass("selected");
                        $(this).attr("sel", "false");
                        var id = $(this).children(":first").attr("id");
                        stopEffect(id);
                        $("#" + id).attr("class", "fas fa-chair fa-2x");
                    }
                });
                newRow.append(newCol);
            }
            mainTable.append(newRow);
        }
        if (numRow > 0 && numCol > 0) {
            $("#confirmMap").css("display", "");
        }
    }
