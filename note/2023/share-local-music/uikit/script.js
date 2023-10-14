
async function fetchMeta() {
  const response = await fetch("./info.json");
  const {cover, artist, album, title} = await response.json();

  document.getElementById("cover").src = cover;
  document.getElementById("artist").textContent = artist;
  document.getElementById("album").textContent = album;
  document.getElementById("title").textContent = title;

  console.log(cover, artist, album, title);
}

fetchMeta().then();
setInterval(() => {
  fetchMeta().then();
}, 5000);

window.onload = () => {
  const audio = document.getElementById("audio")
  audio.src = "SERVER_ADDRESS:PORT/xxx";
  const button = document.getElementById("playpause");
  button.innerText = "Play";
  button.onclick = () => {
    if (button.innerText === "Play") {
      audio.play();
      button.innerText = "Pause";
    } else {
      audio.pause();
      button.innerText = "Play";
    }
  }
}
