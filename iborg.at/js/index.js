const scrollTo = (element) => {
	const speed = $(element).offset().top / 2;
	
	$('html, body').animate({
		scrollTop: $(element).offset().top
	}, speed);
};

$(document).ready(() => {
	console.log("rdy");
	
	$("#b_info").click(() => {
		scrollTo('#info');
	});

	$("#b_faq").click(() => {
		scrollTo('#faq');
	});

	$("#b_skills").click(() => {
		scrollTo('#skills');
	});

	$("#scroll_home").click(() => {
		const speed = $(window).scrollTop() / 2;

		$('html, body').animate({
			scrollTop: '0px'
		}, speed);
	});

	$(window).scroll(() => {
		if ($(window).scrollTop() > 0) {
			$("#scroll_home").fadeIn(300);
		} else {
			$("#scroll_home").fadeOut(300);
		}
	});
});
