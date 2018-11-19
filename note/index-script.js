function myremove(lst, elt) {
  var index = lst.indexOf(elt)
  if (index > -1) {
    lst.splice(index, 1)
  }
}

// tag filtering


window.onload = setupTagList

var excludeTagList = []
var includeTagList = []
var allTagList = []

function setupTagList() {
  for (var tag of document.getElementById('taglist').children) {
    tag.className = 'include'
    includeTagList.push(tag.innerHTML)
    allTagList.push(tag)
  }
}

function toggleAll() {
  toggleTag(document.getElementById('tagAll'))
  for (tag of allTagList) {
    while (tag.className !== tagAll.className) {
      toggleTag(tag)
    }
  }
}

function toggleTag(tag) {
  switch (tag.className) {
    case 'include':
      var nextState = 'noselect'
      myremove(includeTagList, tag.innerHTML)
      break
    case 'noselect':
      var nextState = 'exclude'
      // excludeTagList.push(tag.innerHTML)
      includeTagList.push(tag.innerHTML)
      break
    // case 'exclude':
    //   var nextState = 'include'
    //   myremove(excludeTagList, tag.innerHTML)
    //   includeTagList.push(tag.innerHTML)
    //   break
  }
  tag.className = nextState
  filterHeaders()
}

function filterHeaders() {
  for (var header of document.getElementById('content').children) {
    if (header.className === "outline-2") {
      for (var tag of header.getElementsByClassName('tag')[0].children) {
        if (includeTagList.includes(tag.innerHTML)) {
          header.style.display = 'block'
        } else {
          header.style.display = 'none'
        }
      }
      // exclude list overrides includ list
      for (var tag of header.getElementsByClassName('tag')[0].children) {
        if (excludeTagList.includes(header.tagName)) {
          header.style.display = 'none'
        }
      }
    }
  }
}
