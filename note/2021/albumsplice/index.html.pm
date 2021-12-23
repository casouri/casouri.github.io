#lang pollen

◊define-meta[date]{<2021-12-23 Thu>}
◊define-meta[uuid]{08d3bbb4-63cb-11ec-abd1-1b63bee8032d}
◊define-meta[tags]{Misc}
◊define-meta[lang]{zh}

◊meta{
  ◊title{自动把视频裁切成专辑的工具}
}

油管上经常有一整张专辑做成的视频，虽然我一般尽量买 CD 然后录到电脑里，但是有些专辑根本买不到，这时候就得把视频下下来手工制作成音频文件。当然，手工是不可能手工的，只能写脚本自动化这样子。我写了个简单的脚本，可以自动把视频裁切成一个个音频文件，加上专辑名、音乐家之类的元数据，还有截图视频的第一帧用来当封面。元数据用的是 iTunes 的标签。

这个脚本需要 Python 3 和 ffmpeg。使用方式如下：

1. 新建一个文件夹 ◊code{tmp}
2. 在 ◊code{tmp} 里新建两个文件，◊code{time} 和 ◊code{meta.json}，一个文件夹 ◊code{out}，生成的音频文件会放在 ◊code{out} 里。

◊code{time}里写上每首歌开始的时间和标题，比如

◊bcode{
  00:00 Prologue
  03:00 Rainbow One
  10:27 Rainbow Two
  18:02 Rainbow Three
  21:31 Rainbow Four
  27:45 Rainbow Five
  32:11 Rainbow Six
  39:51 Rainbow Seven
  50:42 Epilogue
}

时间的格式为 ◊code{mm:ss} 或者 ◊code{hh:mm:ss} ，时间和标题之间空几格都可以。

◊code{meta.json}里写专辑的元数据，格式为

◊bcode{
  {
    "artist": "Neil Ardley",
    "album": "Kaleidoscope of Rainbows",
    "year": "1976",
    "genre": "Jazz"
  }
}

3. 在 ◊code{tmp} 里运行脚本 ◊code{albumsplice}。加上 ◊code{--dry}的话，脚本不会运行任何命令，只是打印出来。加上 ◊code{--help} 的话，也不会运行，而是打印说明书。

最后，脚本在这里：◊link["./albumsplice"]{◊code{albumsplice}}。
