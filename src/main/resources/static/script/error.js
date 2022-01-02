var show=false;
$( document ).ready(function() {

    $(".buttonDetails").click(function(){
        if (show==false){
            show=true;
            $(".tableErrorDetails").css("display", "");
            $(".buttonDetails").removeClass("fa-plus-square");
            $(".buttonDetails").addClass("fa-minus-square");

        }
        else{
            show=false;
            $(".tableErrorDetails").css("display", "none");
            $(".buttonDetails").removeClass("fa-minus-square");
            $(".buttonDetails").addClass("fa-plus-square");
        }
    });

});

