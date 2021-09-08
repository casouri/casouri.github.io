#lang pollen

◊define-meta[date]{<2021-09-03 Fri 13:15>}
◊define-meta[uuid]{05c234e6-d6b3-11eb-b625-f744d8291272}
◊define-meta[tags]{中文 Type}
◊define-meta[lang]{zh}

◊(require txexpr)
◊(define (ovl . elm-list)
  (txexpr 'span '((class "overline"))
    elm-list))

◊meta{
  ◊title{自动处理网页里的全角引号和标点挤压}
}

◊section{全角引号}

在 Unicode 里，问号、叹号、各种括号都有全角半角两种版本，各自有独立的编码；但因为莫名的原因，最常用的引号却不在此列。中英混排的时候想要正确显示直角和半角的引号就很头疼；搞不好的话，中文里显示半角引号还不算太违和，英文里蹦出来一个全角引号就太丑了。

CSS 没法自动区别什么时候用全角引号、什么时候用半角，只能靠标记。好在还没复杂到需要手工标记的地步，只要用程序检查引号前后的字是中文还是英文，以此标记全角还是半角，就基本不会出错。我现在的办法是这样，默认字体还是英文先中文后：

◊bcode{
  body {
    font-family: Charter, Source Han Serif CN, serif;
  }
}

需要全角的引号用 ◊code{span} 标签包起来：

◊bcode{
  <span class="full-width-quote">“</span>
}

然后用 CSS 指定中文字体：
◊bcode{
  span.full-width-quote {
    font-family: Srouce Han Serif CN, serif;
  }
}

怎么区别一个引号应该全角还是半角呢？我用了一个简单的判断方法：如果前或后紧挨着中文字符，就全角；如果前后都不是中文字符，就半角。我目前还没发现这个简单判断不够用的情况。这样一来还需要判断一个字符是不是中文，最简单的办法是检查字符的 Unicode codepoint 在不在中文区间内。常用汉字和标点符号在 ◊code{0x4E00}–◊code{0x9FFF} 和 ◊code{0x3000}–◊code{0x303F} 两个区间里，检查这两个就够了，其他的区间里都是生僻字。

◊section{标点挤压}

全角引号搞好了，又会贪心标点挤压。没有标点挤压的时候，几个标点排在一起确实不大好看：

◊fig{
  ◊image["./例子1.png" #:class "half300"]{没有标点挤压的样子}
  ◊figcap{◊link["https://archive.casouri.cat/rock/day/day-48/index.html"]{余日摇滚第48期}}
}


挤压以后就不那么空了：

◊fig{
  ◊image["./例子2.png" #:class "half300"]{有标点挤压的样子}
  ◊figcap{◊link["https://archive.casouri.cat/rock/day/day-48/index.html"]{余日摇滚第48期}}  
}

原理是设置 CSS 属性 ◊code{font-feature-settings: "halt"}，启用 OpenType 的 ◊code{halt} 特性。和全角引号一样，用程序自动识别需要挤压的标点，包在 ◊code{span} 标签里。要注意的是，你用的字体要有 ◊code{halt} 这个特性才行，我用的思源宋体是有的。

具体怎么挤压标点符号，我没找到现成的标准或者算法，下面是我的方法。这个方法并不完整，只处理比较常见的情况，但对我来说够用了。如果读者知道更好的算法，请一定告诉我。

首先，能挤压的标点符号可以分为三类：靠左，靠右，居中：

◊fig{
  ◊image["./各类符号.png"]{各种类型的标点符号}
  ◊figcap{◊link["https://www.w3.org/TR/2020/WD-clreq-20201101/"]{《中文排版需求》，W3C Working Draft 01 November 2020，3.1.6 标点符号的宽度调整，有修改}}
}

我们不考虑居中的符号，因为简体中文普遍不用，而我以简体中文写作。程序从头到尾遍历每个字符，决定每个字符要不要挤压。挤不挤压取决于这个字符和其前后的字符，以伪码表达为：

◊bcode{
遍历 字符：
  如果 此字符为靠左标点 且 后一字符为标点：
    挤压此字符
  如果 此字符为靠右标点 且 前一字符为靠右标点：
    挤压此字符
}

这个算法运行的结果是这样：（（文字）），（文）「字」。

◊fnref["subset"]{如果你用 ◊code{pyftsubset} 压缩过字体文件}，注意它默认会把 ◊code{halt} 这样的 OTF 特性扔掉，这样一来即使加上挤压标签也没有效果。压缩的时候加上 ◊code{--layout-features='*'} 这个选项就可以保留所有 OTF 特性了。也可以用 ◊code{--layout-features='halt'} 只保留 ◊code{halt} 特性。

◊fndef["subset"]{
  参见 ◊link["../../2019/reduce-font-loading-time-in-my-blog/index.html"]{◊em{Reduce Font Loading Time in My Blog}}。
}

◊section{破折号}

我还发现破折号有时会显示成 em dash（因为破折号在 Unicode 里其实就是 em dash）。解决方法和全角引号一样，包上全角的 ◊code{span} 标签就可以了——这样就能正确显示破折号。
