#lang pollen

◊define-meta[date]{<2021-10-23 Sat 00:11>}
◊define-meta[uuid]{97279de0-28c5-11ec-b9e1-e3d1f57bb295}
◊define-meta[tags]{Type}
◊define-meta[lang]{en}

◊meta{
  ◊title{Code Page 437}
  ◊subtitle{Interesting faces, ep3}
}

So I was installing a new OS on my desktop machine, and for some technical reasons I need to install the OS manually. That means typing in a console. I couldn’t help but wonder: what the font is it showing?

◊fig{
  ◊image["console.jpeg"]{Screenshot of the console}
  ◊figcap{I was typing in this}
}

Turns out the typeface isn’t even a typeface. It is a encoding that extends ASCII. It maps 8-bit patterns to characters. For example, ◊code{10000110} corresponds to “å”. According to Wikipedia, It is the“standard character set of the original IBM PC”, and it “remains the primary set in the core of any EGA and VGA-compatible graphic cards”. Basically this is the most basic font on a personal computer, stored directly in hardware.

This character set is supposed to contain many characters including fancy ones like “⌠”, “☺”, “§”, etc. But my graphic card is missing most of the non-basic characters. (How disappointing!)

◊fig{
  ◊image["specimen.jpeg"]{A screenshot specimen}
  ◊figcap{Many characters are missing}
}

I don’t think this font is pretty or anything. What makes it so interesting to me is that it is such ubiquitous yet most people never notice it. Next time ◊fnref["PC"]{when your PC starts up or crashes}, see if you can spot any message printed in this font.

You can even download the font file for this font: ◊link["https://cp437.github.io"]{◊em{Code Page 437}}.

PS. this makes me wonder if Mac has something similar, and sure enough, there is. ◊link["https://apple.stackexchange.com/questions/157038/what-font-is-used-during-verbose-boot-mode"]{Someone asked about it on StackExchange}. I bet even less people know about this one. I for one have never seen it despite using a MacBook for years (That’s probably a good thing, as one only see it when something goes hopelessly wrong.)

PPS. On Linux, you can drop yourself into a console by typing Ctrl+Alt+F1/F2/etc. Usually that screen is printed in ◊link["http://terminus-font.sourceforge.net"]{Terminus}.

◊fndef["PC"]{You still use a PC, do you?}
