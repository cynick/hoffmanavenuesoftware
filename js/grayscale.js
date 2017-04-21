/*!
 * Start Bootstrap - Grayscale Bootstrap Theme (http://startbootstrap.com)
 * Code licensed under the Apache License v2.0.
 * For details, see http://www.apache.org/licenses/LICENSE-2.0.
 */

var lib = (function () {

  // jQuery to collapse the navbar on scroll
  function collapseNavbar() {
    if ($(".navbar").offset().top > 50) {
      $(".navbar-fixed-top").addClass("top-nav-collapse")
    } else {
      $(".navbar-fixed-top").removeClass("top-nav-collapse")
    }
  }

  // jQuery for page scrolling feature - requires jQuery Easing plugin
  $(function() {
    $('a.page-scroll').bind('click', function(event) {
      var $anchor = $(this)
      $('html, body').stop().animate({
        scrollTop: $($anchor.attr('href')).offset().top
      }, 1500, 'easeInOutExpo')
      event.preventDefault()
    })
  })

  // Closes the Responsive Menu on Menu Item Click
  $('.navbar-collapse ul li a').click(function() {
    $(this).closest('.collapse').collapse('toggle')
  })

  var postIndex = 0

  function navigate (index) {
    console.log( "NAV")
    postIndex = index
    $(".post").hide()
    var post = $(".post").eq(postIndex)
    post.show()
    $('html, body').stop().animate({
      scrollTop: post.offset().top - 75
    }, 1500, 'easeInOutExpo')
  }

  function maybeNavigate(e) {
    var postCount = $(".post").length

    if (e.keyCode == '37') {
      navigate( (postIndex + 1) % postCount )
    } else if (e.keyCode == '39') {
      navigate( postIndex == 0 ? postCount -1 : postIndex -1 )
    }
  }

  function showLatestPost() {
    $(".post").hide()
    $(".post").eq(postIndex).show()
  }

  function init() {
    console.log( "INIT" )
    document.onkeydown = maybeNavigate
    $(window).scroll(collapseNavbar)
    $(document).ready(collapseNavbar)

    function initialNavigateIf () {
      var url = window.location.href
      var m = url.match(/.*\/(\S+)$/)
      var id = null;

      if ( m && m[1] ) {
        id = m[1];
      }

      if ( id && document.getElementById(id) ) {
        var els = document.getElementsByClassName("post")
        for ( var index = 0; index < els.length; index++ ) {
          if (els[index].getAttribute('id') == id) {
            postIndex = index
          }
        }
        navigate( postIndex )
      }
    }

    setTimeout(initialNavigateIf, 1500)

    showLatestPost()

  }

  return { init : init }
})()

$(document).ready(lib.init)
