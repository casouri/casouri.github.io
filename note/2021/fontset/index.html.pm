#lang pollen

◊define-meta[date]{<2021-11-24 Wed 17:01>}
◊define-meta[uuid]{336b8c2c-4d8b-11ec-967b-17e83717b0eb}
◊define-meta[tags]{Emacs}
◊define-meta[lang]{en}

◊meta{
  ◊title{Using Fontsets in Emacs}
}

◊section{Fontset?}

Fontset is a feature of Emacs that allows you to bundle together multiple fonts and use them as a single font, such that it covers more characters than a single font could have. For example, you can combine a Latin font, a Greek font and a Chinese font together.

With fontsets, we can use different Unicode fonts for different faces. For example, serif Latin and Chinese font for a “serif” face, and sans serif Latin and Chinese font for a “sans” face. Without fontsets, we can only set different Latin fonts to faces and use a single fall-back Chinese font.

◊image["./fonts & faces.svg" #:class "half"]{A graph showing different fonts with different faces}

◊section{Create a fontset}

A fontset is recognized by its name. Each fontset has two names, one short and one long. The short name looks like ◊code{fontset-xxx}. The long name is a ◊link["https://wiki.archlinux.org/title/X_Logical_Font_Description"]{X Logical Font Description} with last two fields being ◊code{fontset} and ◊code{xxx}. For example,

◊bcode{
  -*-ibm plex mono-medium-*-*-*-13-*-*-*-*-*-fontset-my fontset
}

Emacs come with three fontsets by default: ◊code{fontset-startup}, ◊code{fontset-standard} and ◊code{fontset-default}. We only care about ◊code{fontset-default}; it is the ultimate fall-back when Emacs cannot find a font to display a character. But more on that later.

To create a fontset, you can use ◊code{create-fontset-from-fontset-spec} and pass it a bunch of X Logical Font Descriptions, each for a font you want to include. I find that tedious. Instead, I like to create a fontset with a single ASCII font and use ◊code{set-fontset-font} to add other fonts later, like this:

◊bcode{
  (create-fontset-from-fontset-spec
   (font-xlfd-name
    (font-spec :family "IBM Plex Mono"
               :size 13
               :registry "fontset-my fontset")))
}

Make sure you put the short fontset name under the ◊code{:registry} spec. The code above creates the fontset, and returns its long name,

◊bcode{
  -*-ibm plex mono-*-*-*-*-13-*-*-*-*-*-fontset-my fontset
}

Now we can add a Chinese font and a Greek font:

◊bcode{
(set-fontset-font
 "fontset-my fontset"
 'han (font-spec :family "Source Han Serif" :size 12))
(set-fontset-font
 "fontset-my fontset"
 'greek (font-spec :family "Academica"))
}

If you are not familiar with ◊code{set-fontset-font}, ◊link["http://idiocy.org/emacs-fonts-and-fontsets.html"]{◊em{Emacs, fonts and fontsets}} is a good read.

◊section{Apply a fonset}

Although the manual says we can use a fontset wherever a font is appropriate, it is not entirely true. If you pass your fontset through the ◊code{:font} attribute in ◊code{set-face-attribute}, ◊fnref["takes-ascii"]{Emacs takes the ASCII font from the fontset and only uses the ASCII font for the face}. The real way to do it is to use the undocumented ◊code{:fontset} attribute:

◊bcode{
(set-face-attribute
 'some-face nil :fontset "fontset-my fontset")
}

That’s not all. While the above code works for most faces, setting ◊code{:fontset} for ◊code{default} will not work as you expected, because Emacs again ◊fnref["default"]{only takes the ASCII font, even if you use the ◊code{fontset} attribute}. So don’t set the fontset for the ◊code{default} face; instead, just modify ◊code{fontset-default} (it’s the ultimate fall-back fontset we mentioned earlier) for Unicode fonts, and use whatever method you like for ASCII font. If you read ◊link["http://idiocy.org/emacs-fonts-and-fontsets.html"]{◊em{Emacs, fonts and fontsets}}, you’ll know we can modify ◊code{fontset-default} by either

◊bcode{
  (set-fontset-font "fontset-default" ...)
}

or

◊bcode{
  (set-fontset-font t ...)
}

Technically you could set the ◊code{font} attribute of a frame to a fontset by ◊code{set-frame-font} and it works fine. But as soon as you change any font-related attributes in ◊code{default} face, like font size, your fontset in the frame attribute will be overwritten by the font derived from ◊code{default} face. So the best way is still to just modify ◊code{fontset-default}.

◊fndef["takes-ascii"]{
  According to ◊link["https://github.com/emacs-mirror/emacs/blob/11e5c7d8ca58cc946930048b5c88c8f582d4d5d8/src/xfaces.c#L3391"]{the source}.
}

◊fndef["default"]{
  Basically, if the face is ◊code{default}, ◊code{set-face-attribute} calls ◊code{set_font_frame_param} (◊link["https://github.com/emacs-mirror/emacs/blob/11e5c7d8ca58cc946930048b5c88c8f582d4d5d8/src/xfaces.c#L3514"]{source}), which only looks at the ◊code{:font} attribute (◊link["https://github.com/emacs-mirror/emacs/blob/11e5c7d8ca58cc946930048b5c88c8f582d4d5d8/src/xfaces.c#L3685"]{source}).
}

◊section{Further reading}

◊ul{
  ◊li{Command ◊code{list-fontsets} lists all the defined fontsets.}
  ◊li{Command ◊code{describe-fontset} shows which font is each character assigned to in a fontset.}
  ◊li{Manual page: ◊link["https://www.gnu.org/software/emacs/manual/html_node/emacs/Fontsets.html"]{◊em{Fontsets, Emacs User Manual}}}
  ◊li{Another manual page: ◊link["https://www.gnu.org/software/emacs/manual/html_node/elisp/Fontsets.html"]{◊em{Fontsets, Emacs Lisp Manual}}}
}
