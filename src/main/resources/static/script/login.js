$( document ).ready(function() {
    $('.message a').click(function(){
        $(".messInfo").remove();
        $('form').animate({height: "toggle", opacity: "toggle"}, "slow");
        if ($(this).attr("fun")=="reg"){
            $(".titleForm").text("Log In");
        }
        else {
            $(".titleForm").text("Sign In");
        }
    });
});
