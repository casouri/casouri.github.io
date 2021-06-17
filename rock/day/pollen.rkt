#lang racket

(provide (all-from-out "../../pollen.rkt")
         day-title
         day-link
         cover-img
         piece-info
         synthesis-body
         lyrics
         jpns
         bjpns
         hcon
         day-files
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
    (txexpr 'div '((class "day-link"))
            (list (txexpr 'div empty (list yesterday))
                  (txexpr 'div empty (list tomorrow))))))

;; Expands NAME to ../album/NAME and wraps it in a image tag.
;; Returns an image txexpr.
(define (cover-img name)
  (image (format "../album/~a" name)))

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
         (txexpr 'p '((class "piece-info"))
                 (piece-info doc)))
   (select* 'body doc)))

;;; Rock/day markup

(define (lyrics . text)
  (txexpr 'div empty
          (list (txexpr 'hr empty)
                (txexpr 'div '((class "lyrics")) text))))

;; Mark inline-text as Japanese.
(define jpns (default-tag-function 'span #:class "jpns"))

;; Mark a block as Japanese.
(define bjpns (default-tag-function 'div #:class "jpns"))

;; A horizontal container.
(define hcon (default-tag-function 'div #:class "hcontainer"))

;;; Rock/day homepage/index template

(define (day-files)
  (let* ([root (current-project-root)]
         [src (build-path root "rock/day/src")])
    (sort (filter (lambda (p) (regexp-match #rx".pm" (path->string p)))
                  (directory-list src #:build? #t))
          < #:key day-from-path)))

(define (home-page-titles)
  (txexpr 'div '((id "headings")
                 (class "obviously-a-link"))
          (append
           (map (lambda (n)
                  (txexpr
                   'p empty
                   (list (link (format "./day-~a/index.html" (+ n 1))
                               (format "~a" (+ n 1))))))
                (reverse (range (length (day-files)))))
           (list (txexpr 'a '((href "./index/index.html")
                              (id "index-header"))
                         (list "目录"))))))

(define (index-page-titles)
  (txexpr
   'table empty
   (map (lambda (path)
          (let* ([doc (cached-doc path)]
                 [day (day-from-path path)])
            (txexpr
             'tr empty
             (list
              (txexpr 'td empty
                      (list (link (format "../day-~a/index.html" day)
                                  (format "Day ~a" day))))
              (txexpr 'td empty
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

(define (rss-updated page)
  (let ([updated (select-from-metas 'updated (cached-metas page))]
        [date (select-from-metas 'date (cached-metas page))])
    (rfc3339 (or updated date))))

(define (rock-day-feed-entry page)
  (let ([doc (absolutize-url (cached-doc page) page)])
    (txexpr 'entry empty
            (list
             (txexpr 'title empty (list (day-title page)))
             (txexpr 'link `((href ,(path->string
                                     (build-path root-url
                                                 (find-relative-path
                                                  (current-project-root)
                                                  page))))))
             (txexpr 'id empty (list (string-append
                                      "urn:uuid:"
                                      (select-from-metas
                                       'uuid (cached-metas page)))))
             (txexpr 'updated empty (list (rss-updated page)))
             (txexpr 'content '((type "html"))
                     ;; Include the HTML content as string.
                     (list
                      (->html
                       (synthesis-body doc page #:day-link? #f))))))))

(define (absolutize-url doc path)
  (let ([absolutize (lambda (rel-path)
                      (string-append
                       root-url
                       (path->string
                        (find-relative-path
                         (current-project-root)
                         (simple-form-path
                          (build-path path rel-path))))))])
    (decode doc
            #:txexpr-proc
            (lambda (tx)
            (cond
              [(and (attrs-have-key? tx 'href)
                    (relative-path? (attr-ref tx 'href)))
               (attr-set tx 'href (absolutize (attr-ref tx 'href)))]
              [(and (attrs-have-key? tx 'src)
                    (relative-path? (attr-ref tx 'src)))
               (attr-set tx 'src (absolutize (attr-ref tx 'src)))]
              [else tx])))))

(define (rock-day-feed-entries posts)
  (txexpr 'div empty (map rock-day-feed-entry posts)))
