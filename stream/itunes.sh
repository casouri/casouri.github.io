#!/opt/local/bin/fish

while true

set title (osascript itunesTitle)
set album (osascript itunesAlbum)
set artist (osascript itunesArtist)
set cover "$artist"-"$album".jpg

osascript itunesCover
mv ./cover/cover.jpg ./cover/"$artist"-"$album".jpg

echo '{"title":"'$title'","album":"'$album'","artist":"'$artist'","cover":"'./cover/$cover'"}' > info.json

rsync ./cover/"$cover" linode:blog/stream
rsync info.json linode:blog/stream

echo (date) (cat info.json)
sleep 10
end
