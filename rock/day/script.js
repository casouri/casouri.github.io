// toggle display of titles between "Day X" and music title
function switchTitle() {
  const dayCount = document.getElementById('dayCount')
  const title = document.getElementById('title')
  if (dayCount.className === 'show') {
    dayCount.className = 'hide'
    title.className = 'show'
  } else {
    dayCount.className = 'show'
    title.className = 'hide'
  }
}
