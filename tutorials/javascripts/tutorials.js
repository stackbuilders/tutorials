$(document).ready(function() {

  $(".tutorials_social_icons .facebook-share a").click(function(e) {
    e.preventDefault();
    window.open('https://www.facebook.com/sharer/sharer.php?u=' +
    $(this).attr('href'), 'Share', 'width=626,height=436');
  });
});

$(window).load(function() {
  var currentUrl = window.location.href;
  if (currentUrl.indexOf('tutorials') > 0 && currentUrl.indexOf('page') < 0) {
    $(".figure > img").attr('data-no-retina', '');
  }
});
