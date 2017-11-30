$(document).ready(function() {

  $(".tutorials_social_icons .facebook-share a").click(function(e) {
    e.preventDefault();
    window.open('https://www.facebook.com/sharer/sharer.php?u=' +
    $(this).attr('href'), 'Share', 'width=626,height=436');
  });

  (function() {
    var dsq       = document.createElement('script');
        dsq.type  = 'text/javascript';
        dsq.async = true;
        dsq.src   = '//stackbuilders.disqus.com/embed.js';
        dsq.setAttribute('data-timestamp', +new Date());

    (document.getElementsByTagName('head')[0] ||
        document.getElementsByTagName('body')[0]).appendChild(dsq);
  })();

});

$(window).load(function() {
  var currentUrl = window.location.href;
  if (currentUrl.indexOf('tutorials') > 0 && currentUrl.indexOf('page') < 0) {
    $(".figure > img").attr('data-no-retina', '');
  }
});
