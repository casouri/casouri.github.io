// Copyright: some dude on SO.
function replaceVerticalScrollByHorizontal(event) {
    if (Math.abs(event.deltaY) != 0 && Math.abs(event.deltaX) <= 1) {
        // console.log(event.deltaY);
        // Manually scroll horizontally instead.
        window.scroll(window.scrollX + event.deltaY, window.scrollY);
        // Prevent vertical scroll.
        event.preventDefault();
    }
    return;
}

window.addEventListener('wheel', replaceVerticalScrollByHorizontal,
                        {passive: false});
