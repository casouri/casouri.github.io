#!/opt/local/bin/fish

while true

set title (osascript getTitle)
set album (osascript getAlbum)
set artist (osascript getArtist)
set cover "$artist"-"$album".jpg
osascript getCover
mv cover.jpg "$artist"-"$album".jpg

echo '{"title":"'$title'","album":"'$album'","artist":"'$artist'","cover":"'$cover'"}' > info.json

rsync $cover linode:blog/stream
rsync info.json linode:blog/stream

echo (date) (cat info.json)
sleep 10
end
