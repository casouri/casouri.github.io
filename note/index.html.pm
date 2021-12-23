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

◊easy-title{2021/academica}
◊easy-title{2021/full-width-quote}
◊old-title["2018/mathematics-penmanship"]{Mathematics Penmanship}
◊more-link{./topics/type.html}

◊h2{Using technology}

◊old-title["2019/rime输入法完全指南"]{rime输入法完全指南}
◊old-title["2018/科学上网"]{科学上网}
◊more-link{./topics/tech.html}

◊h2{Emacs}

◊easy-title{2021/fontset}
◊old-title["2021/visual-undo-tree"]{Construct an Undo Tree From a Linear Undo History}
◊old-title["2020/contributing-to-emacs"]{Contributing to Emacs}
◊old-title["2020/painless-transition-to-portable-dumper"]{Painless Transition to Portable Dumper}
◊more-link{./topics/emacs.html}

◊h2{Writing this blog}

◊old-title["2021/like-button"]{Adding a Like Button to My Static Blog}
◊old-title["2019/reduce-font-loading-time-in-my-blog"]{Reduce Font Loading Time in My Blog}
◊more-link{./topics/blog.html}

◊h2{Minor things}

◊easy-title{2021/secure-pin-backing}
◊old-title["2021/disappearing-image"]{Schrödinger’s Image: a File That Both Exists and Not}
◊more-link{./topics/misc.html}

◊br{}
Finally, here is a ◊link["./topics/index.html"]{complete index of all posts}.

