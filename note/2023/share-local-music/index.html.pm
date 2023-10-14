#lang pollen

◊define-meta[date]{<2023-10-13 Fri 19:21>}
◊define-meta[uuid]{5ad0d3dc-6a38-11ee-897a-077e7c7be042}
◊define-meta[tags]{Misc}
◊define-meta[lang]{en}

◊meta{
  ◊title{Sharing Local Music with Friends}
}

The third decade of twenty-first century is strange—people figured out how to make matrices write stories in the style of King James Bible, but ◊fnref["send-file"]{there’s no easy way to send a large file to your friend}.

◊fndef["send-file"]{Yeah, magic-wormhole, etc, etc, but I should be able to right click on a file and select “send to Ian”. ◊link["https://xkcd.com/949/"]{xkcd ◊om{949}} isn’t going away any time soon.}

Another thing you can’t easily do is to listen to your local music with your friend over the internet. Your only options are either some janky Discord bot that distorts the sound and can’t find any non-popular song, or two subscription on some streaming service that has the song you want to play.


But this is ◊om{2023}, of course there’s some open source program, that will work brilliantly with just eight hours of fiddling. Here’s how to fiddle said program and a bit more in ◊om{30} minutes.

◊section{Architecture}

You play music on your computer, the audio stream is captured by a program, that program sends the audio stream to a server. Your friend opens a web page which contains a ◊sc{html} ◊code{audio} tag, which streams music from that server.

◊section{Server}

First, you need a server, ie, any computer that has an publicly accessible ◊sc{ip}. On that server, install ◊link["https://icecast.org"]{icecast2}. On debian, it’s as simple as

◊bcode{
  apt install icecast2
}

Then, change the default password and listening port, the config file is at ◊code{/etc/icecast2/icecast.xml}. You don’t need to touch anything else.

Now you can start icecast2 and forget about it, the command on Debian is

◊bcode{
  systemctl start icecast2
}

◊section{Local streamer}

Now, install ◊link["https://danielnoethen.de/butt/"]{butt}. If you use Mac, you can also use ◊link["https://apps.apple.com/us/app/ladiocast/id411213048?mt=12"]{LadioCast}.

In butt, open Settings—main—Server Settings—Add, and configure according to your server:
◊ul{
  ◊li{Address: your server’s address}
  ◊li{Type: icecast}
  ◊li{Port and Password: the port and password you just set for your server}
  ◊li{Icecast mountpoint: anything you like. if it’s “xxx”, the streaming ◊sc{url} will be ◊code{<server address>:<port>/xxx}}
}

◊section{Feed audio output}

Now you need to feed ◊sc{os} audio output into your local streamer. Follow this article: ◊link["https://danielnoethen.de/butt/howtos/broadcast_audio_files_on_macOS.html"]{How to broadcast music files with butt on macOS}. Then butt will stream it to icecast2, and whoever listening on ◊code{<server address>:<port>/xxx} will be able to hear what you are playing locally.

◊section{A web page}

Your friend needs a music player that supports streaming mp3. Or, better yet, you can host a web page that has a ◊sc{html} ◊code{audio} tag that streams audio on ◊code{<server address>:<port>/xxx}. Then your friend only need to visit that web page. I’ve written a web page like that, you can find the kit here: ◊link["./uikit.zip"]{uikit}. You need to change the title in ◊code{index.html}, and change the address in ◊code{script.js} to your server’s address.

◊figure{
  ◊image["./uikit.png"]{A screenshot of the web page}
  ◊figcap{The web page looks like this}
}

Additionally, I wrote two scripts that uploads the meta data of the currently playing song to your server, so the web page can show the title, artist, album cover, etc. There are two scripts, ◊code{itunes.sh} and ◊code{spotify.sh}, for each app. Change the server address in them and run them locally when you play the music. The scripts are incredibly dumb, and they only work on Mac, but they work ;-)