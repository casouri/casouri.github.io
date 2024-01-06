#lang pollen

◊define-meta[template]{index-template.html.p}

◊meta{
  ◊title{Notes}
}

Welcome, this is the place where I record some of the things that I learned. I hope these notes can be useful to Internet strangers and to myself in the future. I categorized my posts into topics. Under each topic, you can see a few highlighted posts, and you can click “more” to view the rest.

◊(require pollen/core
          pollen/cache
          pollen/file)
◊(define (easy-title path)
   (let ([abs-path (build-path root-path "note" path "index.html.pm")])
     (txexpr 'div '((class "obviously-a-link"))
              (list (title-link (->output-path abs-path)
                                (select 'title (cached-doc abs-path)))))))

◊(define (old-title path title)
   (txexpr 'div '((class "obviously-a-link"))
            (list
             (title-link
              (build-path root-path "note" path "index.html")
              title))))

◊(require txexpr)
◊(define (more-link path)
   (txexpr 'div '((class "obviously-a-link"))
            (list (attr-set (link path "More...")
                            'class "more-link"))))

◊h2{Type}

◊easy-title{2021/code-page-437}
◊easy-title{2021/academica}
◊easy-title{2021/full-width-quote}
◊more-link{./topics/type.html}

◊h2{Programming and Computer Science}

◊easy-title{2024/cse221}
◊easy-title{2023/bonjour}
◊easy-title{2022/nat-what-do-they-do}
◊more-link{./topics/programming.html}

◊h2{Using technology}

◊easy-title{2023/alacritty-modifier}
◊old-title["2019/rime输入法完全指南"]{rime输入法完全指南}
◊old-title["2018/科学上网"]{科学上网}
◊more-link{./topics/tech.html}

◊h2{Emacs}

◊;easy-title{2023/tree-sitter-in-emacs-29}
◊;easy-title{2023/tree-sitter-starter-guide}
◊easy-title{2021/fontset}
◊old-title["2021/visual-undo-tree"]{Construct an Undo Tree From a Linear Undo History}
◊old-title["2020/contributing-to-emacs"]{Contributing to Emacs}
◊;old-title["2020/painless-transition-to-portable-dumper"]{Painless Transition to Portable Dumper}
◊more-link{./topics/emacs.html}

◊h2{Writing this blog}

◊easy-title{2022/domain-change}
◊old-title["2019/reduce-font-loading-time-in-my-blog"]{Reduce Font Loading Time in My Blog}
◊more-link{./topics/blog.html}

◊h2{Minor things}

◊easy-title{2023/share-local-music}
◊easy-title{2021/albumsplice}
◊easy-title{2021/secure-pin-backing}
◊more-link{./topics/misc.html}

Finally, here is a ◊link["./topics/index.html"]{complete index of all posts}.

