#!/opt/local/bin/fish

while true

set title (osascript itunesTitle)
set album (osascript itunesAlbum)
set artist (osascript itunesArtist)
set cover "$artist"-"$album".jpg

osascript itunesCover
if test -e cover.png
    set cover "$artist"-"$album".png
    mv cover.png cover/$cover
else
    mv cover.jpg cover/$cover
end

echo '{"title":"'$title'","album":"'$album'","artist":"'$artist'","cover":"'./$cover'"}' > info.json

rsync ./cover/"$cover" YOUR_SERVER:DIRECTORY_OF_THE_HTML
rsync info.json YOUR_SERVER:DIRECTORY_OF_THE_HTML

echo (date) (cat info.json)
sleep 10
end
