#lang racket

(provide (all-from-out "../../pollen.rkt")
         maybe-custom-palate
         day-title
         day-link
         cover-img
         piece-info
         synthesis-body
         lyrics
         jpns
         bjpns
         trad
         hcon
         output-files
         home-page-titles
         index-page-titles
         rock-day-feed-entries)

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
 "../../pollen.rkt")

;;; Rock/day template

(define (maybe-custom-palate)
  (let ([metas (current-metas)])
    (if (select-from-metas 'highlight-color metas)
        (format "<style>
    html {
       --piece-info-foreground: ~a;
       --foreground: ~a;
       --background: ~a;
    }
  </style>"
                (select-from-metas 'highlight-color metas)
                (select-from-metas 'foreground-color metas)
                (select-from-metas 'background-color metas))
        "")))

(define (day-from-path path)
  (string->number
   (list-ref (regexp-match #rx"day-([0-9]+)" path) 1)))

(define (day-title path)
  (format "Day ~a" (day-from-path path)))

(define (day-link path)
  (let* ([day (day-from-path path)]
         [yesterday (link (format "../day-~a/index.html" (- day 1))
                          "Yesterday ☜")]
         [tomorrow (link (format "../day-~a/index.html" (+ day 1))
                         "☞ Tomorrow")])
    (txexpr 'div '((class "day-link obviously-a-link"))
            (list (txexpr 'div empty (list yesterday))
                  (txexpr 'div empty (list tomorrow))))))

;; Expands NAME to ../album/NAME and wraps it in a image tag.
;; Returns an image txexpr.
(define (cover-img name)
  (image (format "../album/~a" name)
         "Giant gorgeous album cover"))

;; Looks like Artist - Title - Album - Year.
;; Any of them can be omitted, e.g. Artist - Album - Year.
;; Returns a list of txexpr-element.
(define (piece-info doc)
  (let* ([artist (select* 'artist doc)]
         [title (select* 'title doc)]
         [album (select* 'album doc)]
         [year (select* 'year doc)]
         [info (remove* '(#f) (list artist title album year))])
    (apply append
           (list-join info
                      (list (txexpr 'span
                                    '((class "piece-info-separator"))
                                    (list " ◆ ")))))))

(define (synthesis-body doc path #:day-link? [include-day-link #t])
  (append
   (list (txexpr 'h1 '((class "title"))
                 (list (day-title path)))
         (if include-day-link (day-link path) "")
         (txexpr 'p empty (list (select 'cover doc)))
         (txexpr 'p '((class "piece-info obviously-a-link"))
                 (piece-info doc)))
   (get-elements doc)))

;;; Rock/day markup

(define (lyrics . text)
  (txexpr 'div empty
          (list ;; (txexpr 'hr empty)
                (txexpr 'div '((class "lyrics")) text))))

;; Mark inline-text as Japanese. Lang tag for accessibility.
(define jpns (default-tag-function 'span #:class "jpns" #:lang "jp"))

;; Mark a block as Japanese.
(define bjpns (default-tag-function 'div #:class "jpns" #:lang "jp"))

;; Mark inline-text as tradition Chinese.
(define trad (default-tag-function 'span #:class "trad" #:lang "zh"))

;; A horizontal container.
(define hcon (default-tag-function 'div #:class "hcontainer"))

;;; Rock/day homepage/index template

(define (day-files)
  (let* ([src (build-path root-path "rock/day/src")])
    (sort (filter (lambda (p) (regexp-match #rx".pm" (path->string p)))
                  (directory-list src #:build? #t))
          < #:key day-from-path)))

(define (home-page-titles)
  (txexpr 'nav '((id "headings")
                 (class "obviously-a-link"))
          (append
           (map (lambda (n)
                  (txexpr
                   'div empty
                   (list (link (format "./day-~a/index.html" (+ n 1))
                               (format "~a" (+ n 1))))))
                (reverse (range (length (day-files)))))
           (list (txexpr
                  'div empty
                  (list (attr-set (link "./index/index.html" "目录")
                                  'id "index-header")))))))

(define (index-page-titles)
  (txexpr
   'nav '((class "index-table obviously-a-link"))
   (map (lambda (path)
          (let* ([doc (cached-doc path)]
                 [day (day-from-path path)])
            (txexpr
             'div '((class "index-row"))
             (list
              (txexpr 'div empty
                      (list (link (format "../day-~a/index.html" day)
                                  (format "Day ~a" day))))
              (txexpr 'div empty
                      (piece-info doc))))))
        (day-files))))

;;; Pagetree

(define (make-pagetree)
  (list 'pagetree-root
        (cons 'index.html
              (map (lambda (day)
                     (string->symbol (format "day-~a/index.html" day)))
                   (reverse (range (length (day-files))))))))

;;; RSS

(define (output-files)
  (map (lambda (day)
         (build-path root-path
                     (format "rock/day/day-~a/index.html.pm" day)))
       (reverse (range 1 (add1 (length (day-files)))))))

(define (rock-day-feed-entry page)
  (let ([doc (absolutize-url (cached-doc page) page)]
        [uuid (select-from-metas 'uuid (cached-metas page))])
    (when (false? uuid)
      (error
       "Couldn't find uuid meta in page, insert ◊define-meta[uuid]{...}"))
    (txexpr 'entry empty
            (list
             (txexpr 'title empty (list (day-title page)))
             (txexpr 'link `((href ,(path->string
                                     (path-replace-extension
                                      (build-path root-url
                                                  (find-relative-path
                                                   root-path
                                                   page))
                                      "")))))
             (txexpr 'id empty (list (string-append "urn:uuid:" uuid)))
             (txexpr 'updated empty (list (rss-updated page)))
             (txexpr 'content '((type "html"))
                     ;; Include the HTML content as string.
                     (list
                      (doc->html
                       (synthesis-body doc page #:day-link? #f))))))))

(define (rock-day-feed-entries posts)
  (txexpr 'div empty (map rock-day-feed-entry posts)))
