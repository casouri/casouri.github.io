#lang pollen

◊define-meta[date]{<2021-09-13 Mon>}
◊define-meta[uuid]{c80dec02-151a-11ec-8dbf-7b9a8f37728d}
◊define-meta[tags]{Blog}
◊define-meta[lang]{en}

◊meta{
  ◊title{New link style for this blog}
}

◊section{Hyperlink}

◊(define blue-link
  (span '((style "color: rgb(0, 112, 201);")) "like this"))

◊(define underline-link
   (span '((style "text-decoration: underline;
text-decoration-style: dotted;
        text-underline-offset: 0.15em;")) "like this"))

◊(define star
   (span '((style "color: #933;
font-size: 14px;
vertical-align: top;")) "✶"))

◊(define red-link
   (span '((style "background: #fbeaea; border-radius: 8px;"))
          "like this"
          star))

Links in this blog has seen quite some change. In the past they have been marked by blue color, ◊|blue-link|, and by dashed underline, ◊|underline-link|. Now, a link is marked by a red star at the end, like this◊|star|, and when hovered, the link is marked with red background, ◊|red-link|.

I copied this style from Matthew Butterick’s ◊link["https://practicaltypography.com"]{◊em{Practical Typography}}. I think its neat. He used a degree sign °, I changed that to a darker star sign ✶. I tried to use plain asterisk at first, but had difficulties styling it.

The size of the star should be uniform across different sizes of text. If the size of the star is relative to the size of the text surround it, the star would be too small in small text. With a uniform size, the star looks like superscript in body text, and inline symbol in small text:

◊image["./small-text.png" #:class "twothird400"]{A piece of small text containing a link}

◊section{Footnote}

Hyperlinks in this blog look like footnotes, and in turn, footnotes in this blog look like hyperlinks—when hovered, the text referenced by the footnote is marked by red background. The only difference is in the symbol, hyperlinks are marked with ✶, and footnotes with numbers. A footnote looks something like

◊center{
  ◊image["./footnote.png" #:class "half300"]{A piece of text containing a footnote}
}

I like the uniformity: everything that takes you away is marked by a red superscript thingy (except for the obvious ones like menus and buttons).

◊section{Appendix: CSS for hyperlinks}

The CSS styling I used for hyperlinks is

◊bcode{
a { text-decoration: none; }

a:hover {
    background: #fbeaea;
    transition-property: background;
    transition-duration: 0.2s;
    border-radius: 8px;
}

a:after {
    content: "\FEFF✶";
    color: #933;
    /* We fix the size of the ✶ symbol. If we use relative
    font size, it becomes too small in small text. */
    font-size: 14px;
    /* Align to top so it looks like superscript when the
       body text is large. And when the body text is small,
       the symbol looks like inline and doesn’t heighten
       the line. */
    vertical-align: top;
    font-weight: bold;
}
}
