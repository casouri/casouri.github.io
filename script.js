// load mousetrap for key bindings
var newScript = document.createElement('script')
newScript.type = 'text/javascript'
newScript.src = '/misc/mousetrap.js'
document.getElementsByTagName('head')[0].appendChild(newScript)

var newScript = document.createElement('script')
newScript.type = 'text/javascript'
newScript.innerHTML = 'setKey()'
document.getElementsByTagName('head')[0].appendChild(newScript)

// key bindings

function scrollToBottom() {
        window.scrollTo(0, document.body.scrollHeight || document.documentElement.scrollHeight)
}

function scrollToTop() {
        window.scrollTo(0, 0)
}

function flipDown() {
        scrollBy(0, window.screen.height / 2)
}

function flipUp() {
        scrollBy(0, -window.screen.height / 2)
}

function setKey() {
        // scroll to bottom
        Mousetrap.bind('option+>', scrollToBottom)
        Mousetrap.bind('G', scrollToBottom)
        // scroll to top
        Mousetrap.bind('option+<', scrollToTop)
        Mousetrap.bind('g g', scrollToTop)
        // scroll down
        Mousetrap.bind('control+v', flipDown)
        Mousetrap.bind('control+d', flipDown)
        // scroll up
        Mousetrap.bind('option+v', flipUp)
        Mousetrap.bind('control+u', flipUp)
}


document.onkeydown = function(evt) {
    evt = evt || window.event;
    if (evt.keyCode == 27) {
      window.location.href = "./home.html";
    }
};
