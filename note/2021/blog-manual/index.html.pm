#lang pollen

◊define-meta[date]{<2021-09-03 Fri 13:16>}
◊define-meta[uuid]{b368e5a6-0c76-11ec-9a65-7b5b7ac199fc}
◊define-meta[tags]{Blog}
◊define-meta[lang]{en}

◊meta{
  ◊title{Markup Reference for This Blog}
}

This Blog consists of two parts: an old section written in Org Mode markup, and a new section written in Pollen. The code for generating content from the Org source is in ◊code{/elisp}, and we don’t talk about it. This reference describes the markup language I implemented on top of Pollen.

This reference assumes basic understanding of Pollen, one should read its manual, ◊link["https://docs.racket-lang.org/pollen/index.html"]{◊em{Pollen: the book is a program}}, before reading this reference.

The source of most of the tag functions described in this page can be found in ◊code{/pollen.rkt}, under the project root.

This page describes some common markups.

◊section{Emphasizes}

Bold and italic are the same as in HTML.

◊bcode{
  ◊|lozenge|b{bold}, ◊|lozenge|em{italic}.
}

For Chinese, use ◊emph{着重号} instead of italic.

◊bcode{
  ◊|lozenge|emph{着重号}。
}

Center a block with

◊bcode{
  ◊|lozenge|center{
    content
  }
}

◊section{headings}

A blog post (or a single page of text) rarely needs headings beyond the second level, so ◊em{This Blog} only provides first and second level headings:

◊bcode{
  ◊|lozenge|section{First level heading}
  ◊|lozenge|subsection{Second level heading}
}

Headings written in this way are collected in the table of contents and is numbered. To produce a heading that is not in the TOC, nor numbered, use

◊bcode{
  ◊|lozenge|h2{First level heading}
  ◊|lozenge|h3{Second level heading}
}

◊section{Hyperlinks and images}

Hyperlinks are produced by

◊bcode{
  ◊|lozenge|link["url"]{description}
}

For example,

◊bcode{
  ◊|lozenge|link["https://www.gnu.org/software/emacs/"]{◊|lozenge|em{GNU Emacs}}
}

produces a link to GNU Emacs’s homepage: ◊link["https://www.gnu.org/software/emacs/"]{◊em{GNU Emacs}}.

Similarly, images are produced by

◊bcode{
  ◊|lozenge|image["url" #:style "CSS style"]{Image description}
}

The description is mandatory, for accessibility purpose. The ◊code{#:style} argument is optional.

If one wish to include a caption under an image, they can write

◊bcode{
  ◊|lozenge|fig{
    ◊|lozenge|image["url" #:style "width: 30%"]{Image description}
    ◊|lozenge|figcap{caption}
  }
}

It would look something like

◊fig{
  ◊image["../../../casouri.gif" #:style "width: 30%"]{A cat}
  ◊figcap{A cat that looks quite serious}
}

◊section{Code and quote}

Monospaced text is marked with

◊bcode{
  ◊|lozenge|mono{content}
}

Inline code can be produced by

◊bcode{
  ◊|lozenge|code{content}
}

Block code are produced by

◊bcode{
  ◊|lozenge|bcode{
    content
  }
}

As for now, code blocks are not colored.

Similar to a block code, a block quote is produced by

◊bcode{
  ◊|lozenge|bquote{
    My name is Ozymandias, King of Kings;
    Look on my Works, ye Mighty, and despair!
  }
}

◊section{Lists}

We use the same tags as in HTML for creating lists:

◊ul{
  ◊li{◊code{ol} for ordered list,}
  ◊li{◊code{ul} for unordered list, and}
  ◊li{◊code{li} for list items.}
}

For example,

◊bcode{
  ◊|lozenge|ol{
    ◊|lozenge|li{Love}
    ◊|lozenge|li{Peace}
    ◊|lozenge|li{War}
  }
}

and

◊bcode{
  ◊|lozenge|ul{
    ◊|lozenge|li{Streetcar}
    ◊|lozenge|li{Desire}
  }
}

◊section{Tables}

An easy way to produce a table is to use ◊code{quick-table}. In a quick-table, each line defines a row, each cell is separated by “|”. And the first row is considered the header.

◊bcode{
  ◊|lozenge|quick-table{
    Name | Love Power | Exp Level
    Caroline | Strong | 1
    Kat | Medium | 3
    Annie | Strong | 2
  }
}

The complicated way, and perhaps more powerful way, is to use HTML tags:

◊center{
  ◊quick-table{
    Tag | Role
    ◊code{tr} | table row
    ◊code{th} | table header
    ◊code{td} | table data cell
    ◊code{thead} | table head
    ◊code{tbody} | table body
    ◊code{tfoot} | table foot
  } 
}

Note that the table above is wrapped in ◊code{center} tag, a table by itself is not centered.

◊section{Footnote}

A reference to a footnote is produced by

◊bcode{
  ◊|lozenge|fnref["id"]{referenced text}
}

And the definition of the footnote is produced by

◊bcode{
  ◊|lozenge|fndef["id"]{explaination}
}

For example,

◊bcode{
  Today’s guest has 130 years of work experience in the
  ◊|lozenge|fnref["federation"]{federation}.

  ...

  ◊|lozenge|fndef["federation"]{Galactic Federation of Homo Sapiens}
}

It is recommended to put the footnote definition ◊fnref["atend"]{at the end of a section or subsection instead of the end of the whole
page}, because a page on the web could stretch quite long.

◊fndef["atend"]{Like this.}

◊section{Metas}

Before the body starts, we declare some meta information used to produce the final page. The meta for this particular page looks like

◊bcode{
  ◊|lozenge|define-meta[date]{<2021-09-03 Fri 00:14>}
  ◊|lozenge|define-meta[uuid]{b368e5a6-0c76-11ec-9a65-7b5b7ac199fc}
  ◊|lozenge|define-meta[tags]{Type}
  ◊|lozenge|define-meta[lang]{en}

  ◊|lozenge|meta{
    ◊|lozenge|title{Markup Reference}
  }
}

The ◊code{date} meta contains an ◊fnref["timestamp"]{Org Mode time stamp} that’s down to either day or minutes. The ◊code{uuid} meta contains a uuid for this page. The ◊code{tags} meta contains a space-separated list of categories. For now there are ◊code{Type}, ◊code{Emacs}, ◊code{Emacs_pkgs}, ◊code{Programming}, ◊code{Tech}, ◊code{Blog}. The ◊code{lang} meta contains the ◊link["https://en.wikipedia.org/wiki/List_of_ISO_639-1_codes"]{ISO-639 language code} of this page.

Since ◊code{define-meta} can only store plain text, This Blog provides the ◊code{meta} tag to store annotated metas. For the purpose of ◊em{Notes}, we only need to declare the title for the page. ◊link["../../../rock/day/index.html"]{《余日摇滚》} make more elaborate use of this feature.

◊fndef["timestamp"]{
  An Org Mode timestamp looks like ◊code{<2021-09-08 Wed>} or ◊code{<2021-09-08 Wed 09:42>}. See ◊link["https://orgmode.org/manual/Creating-Timestamps.html"]{◊em{Creating Timestamps}}.
}

◊section{余日摇滚}

余日摇滚在上面的基础上增加了一些自己的标记。

博客默认用中文字体显示汉字，因此日文需要手动标记。标记日文用

◊bcode{
  ◊|lozenge|jpns{inline content}
  ◊|lozenge|bjpns{
    Block content
  }
}

余日摇滚的 meta 用下面的格式：

◊bcode{
  ◊|lozenge|define-meta[date]{<2021-09-02 Thu 21:08>}
  ◊|lozenge|define-meta[uuid]{98e190b6-0c6c-11ec-ae06-dfe062c79019}

  ◊|lozenge|meta{
    ◊|lozenge|cover{◊|lozenge|cover-img{first-impression--supernatural.jpg}}
    ◊|lozenge|artist{First impression}
    ◊|lozenge|title{Title of the single}
    ◊|lozenge|album{Title of the album}
    ◊|lozenge|year{1996}
  }
}

一般单曲或是专辑的标题会加上链接：

◊bcode{
  ◊|lozenge|album{◊|lozenge|link["https://youtu.be/G8smivxP7oo"]{supernatural}}
}

◊code{cover-img} 这个标签里填专辑图片的文件名，博客会自动去 ◊code{/rock/day/album} 里找。

最终的结果就是每篇余日摇滚开头的专辑封面和信息，比如这个例子里的◊link["../../../rock/day/day-72/index.html"]{《余日摇滚第72日》}。
