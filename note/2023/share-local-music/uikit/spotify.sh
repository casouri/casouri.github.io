#!/opt/local/bin/fish

while true

set title (osascript spotifyTitle)
set album (osascript spotifyAlbum)
set artist (osascript spotifyArtist)
set cover (osascript spotifyCover)

echo '{"title":"'$title'","album":"'$album'","artist":"'$artist'","cover":"'$cover'"}' > info.json

rsync info.json YOUR_SERVER:DIRECTORY_OF_THE_HTML

echo (date) (cat info.json)
sleep 10
end
