function myremove(lst, elt) {
  var index = lst.indexOf(elt)
  if (index > -1) {
    lst.splice(index, 1)
  }
}

// tag filtering


window.onload = setupTagList

var includeTagList = []

function setupTagList() {
  includeTagList = []
  for (var tag of document.getElementById('taglist').children) {
    tag.className = 'include'
    includeTagList.push(tag.innerHTML)
  }
}

function showAll() {
  setupTagList()
  filterHeaders()
}

function showTag(thisTag) {
  for (var tag of document.getElementById('taglist').children) {
    tag.className = 'noselect'
  }
  thisTag.className = 'include'
  includeTagList = [thisTag.innerHTML]
  filterHeaders()
}

function filterHeaders() {
  for (var header of document.getElementById('headers').children) {
    if (header.className === "outline-2") {
      
      const taglist = header.getElementsByClassName('tag')
      if (taglist !== []) { // Header has tags.

        var show = false
        for (var tag of taglist[0].children) {
          if (includeTagList.includes(tag.innerHTML)) {
            show = true
          }
        }
        if (show) {
          header.style.display = 'block'
        } else {
          header.style.display = 'none'
        }
      }
    }
  }
}
