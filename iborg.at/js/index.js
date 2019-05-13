const scrollTo = (element) => {
    const speed = $(element).offset().top / 2;

    $('html, body').animate({
        scrollTop: $(element).offset().top
    }, speed);
};

$(document).ready(() => {
    console.log("rdy");

    $("#info-button").click(() => {
        scrollTo('#info');
    });

    $("#faq-button").click(() => {
        scrollTo('#faq');
    });

    $("#skills-button").click(() => {
        scrollTo('#skills');
    });

    $("#scroll-home").click(() => {
        const speed = $(window).scrollTop() / 2;

        $('html, body').animate({
            scrollTop: '0px'
        }, speed);
    });

    $(window).scroll(() => {
        if ($(window).scrollTop() > 0) {
            $("#scroll-home").fadeIn(300);
        } else {
            $("#scroll-home").fadeOut(300);
        }
    });
});
