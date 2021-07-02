#lang racket

(provide (all-from-out "../pollen.rkt")
         homepage-titles
         topics)

(require
 pollen/decode
 pollen/file
 pollen/tag
 pollen/core
 pollen/template
 pollen/pagetree
 pollen/setup
 pollen/cache
 txexpr
 "../pollen.rkt")

;;; Functions

(define (titles-in-year year [tag #f])
  (let* ([current-dir (here-path)]
         [year-dir (build-path root-path "note" (number->string year))]
         ;; Get a list of /note/year/title/index.html.pm.
         [post-file-list
          (filter file-exists?
                  (map (lambda (dir)
                         (build-path dir "index.html.pm"))
                       (directory-list year-dir #:build? #t)))]
         ;; Sort by date authored.
         [sorted-file-list
          (sort post-file-list
                (lambda (path1 path2)
                  (string<?
                   (select-from-metas 'date (cached-metas path1))
                   (select-from-metas 'date (cached-metas path2)))))])
    (append-map
     (lambda (path)
       (let ([rel-html-path
              ;; This link is used on both homepage and topics page,
              ;; the relative path are different between the two.
              (path->string (->output-path (rel-path path current-dir)))]
             [tags (string-split (select-from-metas
                                  'tags (cached-metas path)))])
         ;; If a tag is specified and the post doesn’t contain
         ;; it, exclude the post.
         (if (and tag (not (member tag tags)))
             empty
             (list (txexpr 'div '((class "index-heading"))
                           (list
                            (txexpr
                             'div empty
                             (list (link rel-html-path
                                         (select 'title
                                                 (cached-doc path)))))
                            (post-tags tags)))))))
     sorted-file-list)))

(define (new-titles from to [tag #f])
  (filter
   ;; Filter out the years that doesn’t have any post. (The first
   ;; element is <h2>year</h2>, the rest are the titles in that year.)
   (lambda (year-tx)
     (lambda (year-tx)
       (> (length (get-elements year-tx)) 1)))
   (map (lambda (year)
          (txexpr 'div empty
                  (cons
                   (txexpr 'h2 empty (list (number->string year)))
                   (titles-in-year year tag))))
        (range to (sub1 from) -1))))

(define (post-tags tags)
  (txexpr 'div '((class "index-tag"))
          (map (lambda (tag) (txexpr 'span empty (list tag)))
               tags)))

(define (old-titles [tag #f])
  (filter
   ;; Filter out the years that doesn’t have any post. (The first
   ;; element is <h2>year</h2>, the rest are the titles in that year.)
   (lambda (year-tx)
     (> (length (get-elements year-tx)) 1))
   (map (lambda (year)
          (txexpr
           'div empty
           (cons
            ;; Year as a heading.
            (txexpr 'h2 empty (list (format "~a (old)" year)))
            ;; After the heading are the posts in that year.
            (append-map
             (lambda (entry)
               (let* ([tags (string-split (dict-ref entry 'tags))]
                      [current-dir (here-path)]
                      ;; This link is used on both homepage and
                      ;; topics page, the relative path are different
                      ;; between the two.
                      [rel-html-path
                       (rel-path
                        (build-path root-path "note"
                                    (dict-ref entry 'path))
                        current-dir)])
                 ;; If a tag is specified and the post doesn’t
                 ;; contain it, exclude the post.
                 (if (and tag (not (member tag tags)))
                     empty
                     (list (txexpr
                            'div
                            '((class "index-heading"))
                            (list
                             ;; Link to the post.
                             (txexpr 'div empty
                                     (list
                                      (link rel-html-path
                                            (dict-ref entry 'title))))
                             ;; Tags of that page. Each tag is in a
                             ;; span.
                             (post-tags tags)))))))
             (filter (lambda (entry)
                       (eq? year (dict-ref entry 'year)))
                     old-posts)))))
        (range 2021 2017 -1))))

(define (homepage-titles [tag #f])
  (txexpr 'nav '((id "headings")
                 (class "obviously-a-link"))
          (append
           (new-titles 2021 2021 tag)
           (old-titles tag))))

(define (topics)
  (let ([tags '("Emacs" "Web" "Tech" "Non-tech" "中文")])
    
    (txexpr 'nav '((id "toc")
                   (class "obviously-a-link"))
            (list
             (txexpr 'h2 empty (list "Topics"))
             (txexpr 'ul empty
                     (for/list ([tag tags])
                       (let* ([file (format "~a.html"
                                            (string-downcase tag))]
                              [path (path->string
                                     (build-path
                                      (rel-path "note/topics" (here-path))
                                      file))])
                         (txexpr 'li empty (list (link path tag))))))))))

;;; Variables

(define old-posts
  (map
   (lambda (lst)
     `((path . ,(car lst))
       (title . ,(cadr lst))
       (tags . ,(caddr lst))
       (year . ,(string->number (car (string-split (car lst) "/"))))))
   '(("2021/visual-undo-tree/index.html"
      "Construct an Undo Tree From a Linear Undo History"
      "Emacs")
     ("2021/like-button/index.html"
      "Adding a Like Button to My Static Blog"
      "Web")
     ("2021/disappearing-image/index.html"
      "Schrödinger’s Image: a File That Both Exists and Not"
      "Web Programming")
     ("2020/home-brew-define-key/index.html"
      "Home-brew define-key"
      "Emacs")
     ("2020/emacs-theme-utility/index.html"
      "Emacs Theme Utility"
      "Emacs")
     ("2020/embed-images-in-text-files/index.html"
      "Embed Images in Text Files"
      "Emacs")
     ("2020/home-brew-use-package/index.html"
      "Home-brew use-package"
      "Emacs")
     ("2020/simple-(back)-links-in-any-file/index.html"
      "Simple (Back) Links in Any File"
      "Emacs")
     ("2020/atomic-buffer/index.html"
      "Atomic Buffer"
      "Emacs")
     ("2020/org-html-export:-permanent-section-link/index.html"
      "Org HTML Export: Permanent Section Link"
      "Emacs Web")
     ("2020/contributing-to-emacs/index.html"
      "Contributing to Emacs"
      "Emacs")
     ("2020/safari’s-new-clipper/index.html"
      "Safari’s New Clipper  "
      "Tech")
     ("2020/insert-math-symbol-in-emacs/index.html"
      "Insert Math Symbol in Emacs"
      "Emacs")
     ("2020/painless-transition-to-portable-dumper/index.html"
      "Painless Transition to Portable Dumper   "
      "Emacs")
     ("2019/debug-in-emacs/index.html"
      "Debug in Emacs"
      "Emacs")
     ("2019/use-command-bindings-in-iterm-for-emacs/index.html"
      "Use Command bindings in iTerm for Emacs   "
      "Emacs")
     ("2019/display-console-in-emacs/index.html"
      "Display console in Emacs"
      "Emacs")
     ("2019/reduce-font-loading-time-in-my-blog/index.html"
      "Reduce Font Loading Time in My Blog"
      "Web")
     ("2019/emacs-字体与字体集/index.html"
      "Emacs，字体与字符集"
      "Emacs")
     ("2019/rime输入法完全指南/index.html"
      "Rime输入法完全指南   "
      "Tech 中文")
     ("2018/mathematics-penmanship/index.html"
      "Mathematics Penmanship"
      "Non-tech")
     ("2018/prettify-google-docs/index.html"
      "Prettify Google Docs"
      "Non-tech")
     ("2018/科学上网/index.html"
      "科学上网"
      "Tech 中文")
     ("2018/easy-bindings-when-region-is-active/index.html"
      "Easy Bindings when Region Is Active"
      "Emacs")
     ("2018/emacs-keymap-precedence/index.html"
      "Emacs Keymap Precedence"
      "Emacs")
     ("2018/who-called-my-function/index.html"
      "Who Called My Function?"
      "Emacs")
     ("2018/emacs-gateway-drug/index.html"
      "Emacs Gateway Drug"
      "Emacs")
     ("2018/retro-terminal-blog-style/index.html"
      "Retro Terminal Blog Style"
      "Web")
     ("2018/join-chat-on-irc/index.html"
      "Join chat on IRC"
      "Tech")
     ("2018/wanderlust/index.html"
      "Wanderlust"
      "Tech Emacs"))))
