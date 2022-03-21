#lang racket

(provide (all-from-out "../pollen.rkt")
         homepage-titles
         title-link
         note-feed-entry)

(require
 pollen/decode
 pollen/file
 pollen/tag
 pollen/core
 pollen/template
 pollen/pagetree
 pollen/setup
 pollen/cache
 pollen/render
 txexpr
 "../pollen.rkt")

;;; Functions

;; Generate the tags following each title index.
(define (post-tags tags)
  (txexpr 'div '((class "index-tag"))
          (map (lambda (tag) (txexpr 'span empty (list tag)))
               tags)))

;; Title link of the post at PATH. PATH is the absolute path.
(define (title-link path title [tags empty])
  (let* ([current-dir (here-path)]
         [rel-html-path
          ;; This link is used on both homepage and topics page,
          ;; the relative path are different between the two.
          (path->string
           (->output-path
            (rel-path path current-dir)))])
    (txexpr 'div '((class "title-link"))
            (list
             (txexpr 'div empty (list (link rel-html-path title)))
             ;; (post-tags tags)
             ))))

;; Return an index of posts authored in YEAR. Only include posts with
;; TAG if TAG isn’t #f.
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
                  ;; Only look at yyyy-mm-dd in <yyyy-mm-dd ...>.
                  (string>? ; Later posts come first.
                   (substring
                    (select-from-metas 'date (cached-metas path1))
                    1 11)
                   (substring
                    (select-from-metas 'date (cached-metas path2))
                    1 11))))])
    (append-map
     (lambda (path)
       (let ([tags (string-split (select-from-metas
                                  'tags (cached-metas path)))])
         ;; If a tag is specified and the post doesn’t contain
         ;; it, exclude the post.
         (if (and tag (not (member tag tags)))
             empty
             (list (title-link
                    path (select 'title (cached-doc path)) tags)))))
     sorted-file-list)))

;; Return a list of new (Pollen) posts from year FROM to TO. TAG is
;; the same as in titles-in-year.
(define (new-titles from to [tag #f])
  (filter
   ;; Filter out the years that doesn’t have any post. (The first
   ;; element is <h2>year</h2>, the rest are the titles in that year.)
   (lambda (year-tx)
     (> (length (get-elements year-tx)) 1))
   (map (lambda (year)
          (txexpr 'div empty
                  (cons
                   (txexpr 'h2 empty (list (number->string year)))
                   (titles-in-year year tag))))
        (range from (sub1 to) -1))))

;; Return an index of all the old posts, filtered by TAG.
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
            (txexpr 'h2 empty (list (format "~a" year)))
            ;; After the heading are the posts in that year.
            (append-map
             (lambda (entry)
               (let* ([tags (string-split (dict-ref entry 'tags))]
                      [current-dir (here-path)]
                      ;; This link is used on both homepage and
                      ;; topics page, the relative path are different
                      ;; between the two.
                      [path (build-path root-path "note"
                                        (dict-ref entry 'path))])
                 ;; If a tag is specified and the post doesn’t
                 ;; contain it, exclude the post.
                 (if (and tag (not (member tag tags)))
                     empty
                     (list
                      (title-link path (dict-ref entry 'title) tags)))))
             (filter (lambda (entry)
                       (eq? year (dict-ref entry 'year)))
                     old-posts)))))
        (range 2021 2017 -1))))

;; Generate an index of all titles for the homepage.
(define (homepage-titles [tag #f])
  (txexpr 'nav '((id "headings")
                 (class "obviously-a-link"))
          (append
           (new-titles 2022 2021 tag)
           (old-titles tag))))

;; Obsolete
;;
;; Generate a TOC, but for each topic, e.g.:
;;
;; Topics:
;; - Emacs
;; - Web
(define (topics topic-list)
  (txexpr 'nav '((id "toc")
                 (class "obviously-a-link"))
          (list
           (txexpr 'h2 empty (list "Topics"))
           (txexpr 'ul empty
                   (cons
                    ;; “All” links to the homepage.
                    (txexpr 'li empty
                            (list (link (rel-path "note/index.html"
                                                  (here-path))
                                        "All")))
                    ;; Each topics.
                    (for/list ([tag topic-list])
                      (let ([path (rel-path
                                   (format "note/topics/~a.html"
                                           (string-downcase tag))
                                   (here-path))])
                        (txexpr 'li empty (list (link path tag))))))))))

;; Obsolete
;;
;; Generate each topic index page and save them to file under
;; /note/topics.
(define (generate-topic-pages topic-list)
  (map (lambda (topic)
         (let* ([template
                 (file->string
                  (build-path root-path
                              "note/topics/topic-template.html.p"))]
                [out-path (build-path root-path "note/topics/"
                                      (format "~a.html.pm" topic))]
                [out (open-output-file out-path
                                       #:mode 'text
                                       #:exists 'replace)])
           (display (regexp-replace* "TOPIC" template topic) out)
           (close-output-port out)))
       topic-list)
  "\n")

;;; Feed

;; PATH is like year/dir.
(define (note-feed-entry path)
  (let* ([abs-path (build-path root-path "note" path "index.html.pm")]
         [doc (absolutize-url (cached-doc abs-path) abs-path)]
         [uuid (select-from-metas 'uuid (cached-metas abs-path))])
    (when (false? uuid)
      (error
       "Couldn't find uuid meta in page, insert ◊define-meta[uuid]{...}"))
    (txexpr 'entry empty
            (list
             (txexpr 'title empty (list (select 'title doc)))
             (txexpr 'link `((href ,(path->string
                                     (build-path root-url "note" path)))))
             (txexpr 'id empty (list (string-append "urn:uuid:" uuid)))
             (txexpr 'updated empty (list (rss-updated abs-path)))
             (txexpr 'content '((type "html"))
                     ;; Include the HTML content as string.
                     (list (doc->html doc)))))))


;;; Variables

(define topic-list '("Emacs" "Web" "Tech" "Non-tech" "中文"))

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
      "Blog")
     ("2021/disappearing-image/index.html"
      "Schrödinger’s Image: a File That Both Exists and Not"
      "Misc")
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
      "Blog")
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
      "Blog")
     ("2019/emacs-字体与字体集/index.html"
      "Emacs，字体与字符集"
      "Emacs")
     ("2019/rime输入法完全指南/index.html"
      "Rime输入法完全指南   "
      "Tech 中文")
     ("2018/mathematics-penmanship/index.html"
      "Mathematics Penmanship"
      "Type")
     ("2018/prettify-google-docs/index.html"
      "Prettify Google Docs"
      "Type")
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
      "Blog")
     ("2018/join-chat-on-irc/index.html"
      "Join chat on IRC"
      "Misc")
     ("2018/wanderlust/index.html"
      "Wanderlust"
      "Tech Emacs"))))
