#lang racket

(provide rel-path
         here-path
         essential-html-meta
         link
         image
         fig
         figcap
         quick-table
         ul
         ol
         li
         bquote
         mono
         code
         bcode
         rt
         breadcrumb
         header-info
         header-line
         footer
         like-button
         toc
         section
         subsection
         article-title
         remove-meta
         post-proc
         doc->html
         doc->html*
         ;; helper
         list-join
         rfc3339
         get-language
         ;; vars
         author-en
         author-zh
         root-url
         root-path
         lozenge)

(require
 pollen/decode
 pollen/file
 pollen/tag
 pollen/core
 pollen/template
 pollen/pagetree
 pollen/setup
 pollen/cache
 pollen-count
 txexpr
 srfi/19
 net/url)

;;; Variable

(define author-zh "付禹安")
(define author-en "Yuan Fu")
(define root-url "https://archive.casouri.cat/")
(define root-path (string->path "/Users/yuan/p/casouri/"))

(define lozenge "◊")

;;; Helper

(define (list-join lst sep)
  (case (length lst)
    [(0) '()]
    [(1) lst]
    [else (append (list (car lst) sep)
                  (list-join (cdr lst) sep))]))

(define (sanitize-url url)
  (url->string (string->url (string-normalize-nfc url))))

(define (get-language [fallback #f])
  (or (select-from-metas 'lang (current-metas))
      fallback))

(define (txexpr->string tx)
  (foldr string-append "" (findf*-txexpr tx string?)))

(define (rel-path root-rel-path here-path)
  (let ([rel (find-relative-path
              (path-only here-path)
              (build-path root-path root-rel-path))])
    ;; Technically ‘find-relative-path’ should return ./
    ;; when two paths are the same, but in reality it doesn't.
    ;; So we need to fix that ourselves.
    (path->string
     (if (equal? rel (build-path root-path root-rel-path))
         (build-path 'same)
         (build-path 'same rel)))))

(define (here-path)
  (select-from-metas 'here-path (current-metas)))

(define (essential-html-meta stylesheet-rel-path)
  (list
   (txexpr 'meta '((charset "utf-8")))
   ;; This meta is needed by CSS media queries that detects
   ;; mobile/desktop/etc.
   (txexpr 'meta '((name "viewport")
                   (content
                    "width=device-width, initial-scale=1")))
   (txexpr 'link `((rel "stylesheet")
                   (type "text/css")
                   (href ,(rel-path stylesheet-rel-path
                                    (here-path)))))
   (txexpr 'link `((rel "icon")
                   (type "image/png")
                   (href ,(rel-path "favicon.png" (here-path)))))))

;;; Common markup

;;;; Link
;; A URL link. ◊link[url]{text}. If TEXT is omited, use URL as text.
(define (link url . tx-elements)
  (let* ([url (sanitize-url url)]
         [tx-elements (if (empty? tx-elements)
                          (list url)
                          tx-elements)]
         [link-tx (txexpr 'a empty tx-elements)]
         [link-tx (attr-set link-tx 'href url)])
    link-tx))

;; An image. SRC is the path to the image.
(define (image src #:style [style #f] alt)
  (txexpr 'img (append `((src ,(sanitize-url src))
                         (alt ,alt))
                       (if style (list (list 'style style)) empty))
          empty))

;;;; Footnote

(define (build-footnote-id-list doc)
  (select* 'fnref doc))

(define footnote-id-list empty)

;; Footnote reference, ID can be either number or string. The footnote
;; reference shown on page isn't ID, but a automatically generated
;; number.
(define (decode-fnref tx)
  (if (not (eq? (get-tag tx) 'fnref))
      tx
      (let* ([id (car (get-elements tx))]
             [ref-id (format "footref:~a" id)]
             [def-id (format "footdef:~a" id)]
             [display-number (add1 (index-of footnote-id-list id))]
             [id-display (format "~a" display-number)])
        (txexpr 'span '((class "footnote"))
                (list (attr-set* (link (string-append "#" def-id)
                                       id-display)
                         'id ref-id
                         'class "inline-footref"
                         'aria-label "Jump to footnote"))))))

;; Footnote definition. ID should match a previous footnote reference.
(define (decode-fndef tx)
  (if (not (eq? (get-tag tx) 'fndef))
      tx
      (let* ([id (car (get-elements tx))]
             [text (cdr (get-elements tx))]
             [ref-id (format "footref:~a" id)]
             [def-id (format "footdef:~a" id)]
             [display-number (add1 (index-of footnote-id-list id))]
             [id-display (format "~a" display-number)])
        (txexpr 'div `((id ,def-id)
                       (class "footdef"))
                (list
                 (txexpr 'div '((class "ref-footref obviously-a-link"))
                         (list (attr-set (link (string-append "#" ref-id)
                                               id-display)
                                    'aria-label "Jump back to main text")))
                 (txexpr 'div '((class "def-footdef")) text))))))

;;;; Table

(define tr (default-tag-function 'tr))
(define td (default-tag-function 'td))
(define th (default-tag-function 'th))

;; Picks out separators SEP from strings in ELM-LIST.
;; Elements in ELM-LIST could be non-string.
;; E.g. ("a|b" "|c" d) -> ("a" "|" "b" "|" "c" d)
(define (string-pick-out elm-list sep)
  (append-map (lambda (elm)
                (if (string? elm)
                    (list-join (string-split elm sep #:trim? #f) sep)
                    (list elm)))
              elm-list))

;; Split ELM-LIST by SEP.
;; E.g. ("a" "|" "b" "|" "c" d) -> (("a") ("b") ("c" d))
(define (split-list elm-list sep [intermediate empty])
  (if (empty? elm-list)
      (list intermediate)
      (if (equal? (car elm-list) sep)
          ;; Current elm is a separator, save INTERMEDIATE to return
          ;; list.
          (cons intermediate
                (split-list (cdr elm-list) sep empty))
          ;; Current elm is not a separator, save elm to INTERMEDIATE.
          (split-list (cdr elm-list) sep
                      (append intermediate (list (car elm-list)))))))

;; Define a table where rows are separated by newlines, columns are
;; separated by “|”. The first row is considered the header row.
(define (quick-table . elm-list)
  ;; “Pick out” all the bars and newlines.
  (let* ([maybe-trim (lambda (elm)
                       (if (string? elm) (string-trim elm) elm))]
         [trimmed-list (map maybe-trim elm-list)]
         [seped-list (string-pick-out
                      (string-pick-out elm-list "|") "\n")]
         ;; Now we have a 3d matrix (each cell is a list)
         [matrix (map (lambda (row) (split-list row "|"))
                      (split-list seped-list "\n"))]
         ;; Turns a list of elements into an row txexpr (th/td).
         [process-row
          (lambda (row cell-tag)
            (txexpr 'tr empty
                    (map (lambda (cell)
                           (txexpr cell-tag empty
                                   (map maybe-trim cell)))
                         row)))])
    (txexpr 'table empty
            (cons
             (process-row (car matrix) 'th)
             (map (lambda (row) (process-row row 'td))
                  (cdr matrix))))))

;;;; Misc

(define ul (default-tag-function 'ul))
(define ol (default-tag-function 'ol))
(define li (default-tag-function 'li))

(define bquote (default-tag-function 'blockquote))

(define mono (default-tag-function 'span #:class "mono"))
(define code (default-tag-function 'code))
(define bcode (default-tag-function 'pre #:class "code-block"))

(define fig (default-tag-function 'figure))
(define figcap (default-tag-function 'figcaption))

;; Ruby.
(define (rt . text)
  (@ (txexpr 'rp empty '("("))
     (txexpr 'rt empty text)
     (txexpr 'rp empty '(")"))))

;;; Common template

;; Returns the parent directory of PATH. Doesn’t matter if PATH is a
;; directory path or file path.
(define (path-parent path)
  (simple-form-path
   (build-path (path-only path) 'up)))

;; A breadcrumb that looks like Home / sub-site / sub-sub-site.
;; Returns a list of txexpr-element.
(define (breadcrumb [path #f] [rel-link #f])
  ;; First, elevate DIR and REL-LINK to point to parent directory.
  (let* ([dir (path-parent
               (or path
                   (simple-form-path
                    (select-from-metas 'here-path (current-metas)))))]
         [rel-link (build-path 'up (or rel-link "index.html"))]
         [root (simple-form-path (current-project-root))]
         [index-page (build-path dir "index.html.pm")])
    (if (string>? (path->string dir)
                  (path->string root))
        ;; Recursion case. 
        (if (file-exists? index-page)
            (let ([dir-title (or (select 'title (cached-doc index-page))
                                 (select 'h1 (cached-doc index-page)))])
              (append (breadcrumb dir rel-link)
                      (list (link (path->string rel-link) dir-title)
                            " ∕ ")))
            (breadcrumb dir rel-link))
        ;; Final case, link to home.
        (list (link (path->string rel-link) "Home") " ∕ "))))

;; Header information that looks like RSS | Source | License. Returns
;; a list of txexpr-element.
(define (header-info [rss-link #f])
  (let ([rss-link (or (select-from-metas 'rss-link (current-metas))
                      rss-link)])
    (list-join
     (remove
      #f
      (list
       (if rss-link (link rss-link "RSS") #f)
       (link "https://github.com/casouri/casouri.github.io" "Source")
       (link "https://creativecommons.org/licenses/by-sa/4.0/"
             "License")))
     "│")))

(define (header-line #:rss [rss-link #f])
  (txexpr 'header '((id "header")
                    (class "obviously-a-link"))
          (list (txexpr 'nav empty (breadcrumb))
                (txexpr 'div empty (header-info rss-link)))))

;; A footer that displays author, written date, and comment. Returns a
;; txexpr. LANG can be either "zh" or "en".
(define (footer lang)
  (let* ([author author-zh]
         [timestamp (select-from-metas 'date (current-metas))]
         [timestamp (list-ref (regexp-match #rx"<(.+)>" timestamp) 1)]
         [zh-en (lambda (zh en) (if (equal? lang "zh") zh en))])
    (txexpr
     'div empty
     (list (txexpr 'p empty
                   (list (string-append (zh-en "作者 " "Author ")
                                        author)))
           (txexpr 'p empty
                   (list (string-append (zh-en "写于 " "Published on")
                                        timestamp)))
           (txexpr
            'p empty
            (list
             (zh-en "评论 发邮件给 " "Comment send a message to ")
             (link "mailto:archive.casouri.cat@gmail.com"
                   "archive.casouri.cat@gmail.com")))))))

(define (like-button)
  (let* ([rel-path (find-relative-path
                    (current-project-root)
                    ;; ->output-path removes .pm extension.
                    (->output-path
                     (select-from-metas 'here-path (current-metas))))]
         ;; Add initial "/" to keep compatible with the old blog.
         [path-string (path->string (build-path "/" rel-path))])
    (txexpr 'div '((class "like-button"))
            (list (txexpr 'form '((action "/like")
                                  (method "post"))
                          (list (txexpr 'input
                                        `((name "path")
                                          (type "hidden")
                                          (value ,path-string))
                                        '())
                                (txexpr 'button '((class "like")
                                                  (type "submit"))
                                        (list "❤ Like"))))))))

(define (remove-meta doc)
  (let-values ([(rest _)
                (splitf-txexpr
                 doc
                 (lambda (tx)
                   (and (txexpr? tx) (eq? (get-tag tx) 'meta))))])
    rest))

;;; Post processing

(define (post-proc doc)
  (set! footnote-id-list (build-footnote-id-list doc))
  (set! doc (remove-meta doc))
  (set! doc (decode
             doc
             #:txexpr-elements-proc ignore-indent
             #:exclude-tags '(bcode)))
  (set! doc (decode
             doc
             #:txexpr-proc (compose1 decode-fndef
                                     decode-fnref)
             #:string-proc process-punc))
  (set! doc (decode
             doc
             #:txexpr-elements-proc decode-paragraphs
             #:exclude-tags '(figure)))
  doc)

(define (doc->html doc)
  (->html (post-proc doc) #:splice? #t))

(define (doc->html* elm-list)
  (->html (post-proc (txexpr 'root empty elm-list)) #:splice? #t))

;;;; TOC, header, title

(define (collect-headline tx)
  (if (txexpr? tx)
      (if (eq? (get-tag tx) 'h2)
          (list (txexpr
                 'li empty
                 ;; Link to the headline.
                 (list (apply link (string-append "#" (txexpr->string tx))
                              (get-elements tx)))))
          (append-map collect-headline (get-elements tx)))
      empty))

(define (toc doc)
  (let* ([lang (get-language "en")]
         [zh-en (lambda (zh en) (if (equal? lang "zh") zh en))])
    (txexpr 'nav '((id "toc")
                   (class "obviously-a-link"))
            (list (txexpr 'h2 empty (list (zh-en "目录" "TOC")))
                  (txexpr 'ol empty (collect-headline doc))))))

(define (section . elm-list)
  (let ([tx (txexpr 'h2 empty elm-list)])
    (attr-set* tx 'id (txexpr->string tx)
               'class "section")))

(define (subsection . elm-list)
  (let ([tx (txexpr 'h3 empty elm-list)])
    (attr-set* tx 'id (txexpr->string tx)
               'class "subsection")))

(define (article-title doc)
  (txexpr 'h1 '((class "title"))
          (select* 'title doc)))

;;;; Ignore indents

(define (ignore-indent elm-list)
  (map (lambda (elm)
         (if (string? elm)
             ;; Annihilate any space after a newline.
             (regexp-replace #rx"\n +" elm "\n")
             elm))
       elm-list))

;;;; Squeeze quotation marks

;; If CHAR is a CJK character, return #t. “CJK character” is narrowly
;; defined as lying in 4E00-9FFF and 3000-303F.
(define (cjk? char)
  (let ([code (char->integer char)])
    (or (<= #x4E00 code #x9FFF) ; CJK Unified Ideographs
        (<= #x3000 code #x303F) ; CJK Symbols and Punctuation
        )))

(define squeezed-marks-left (string->list "，。、：；？！”》）』」〗】"))
(define squeezed-marks-right (string->list "“《（『「〖【"))
(define squeezed-marks (append squeezed-marks-left squeezed-marks-right))

;; Add full-width quotation marks and squeezes other full-width
;; punctuation marks. For two consecutive full-width punctuation
;; characters: wrap them in <span class"squeeze full-width"> tags. For
;; quotation marks that ought to be full-width: wrap them in <span
;; class="full-width-mark"> tags. A quotation mark should be full-width if
;; it is next to a CJK character, as determined by CJK? (that function
;; only detects common Chinese characters and punctuation, other East
;; Asian languages don’t use curly quote anyway).
;;
;; Pro tip: if there are three (or more) consecutive punctuation
;; marks, control squeezing which two by inserting ZERO WIDTH SPACE
;; between the ones that you don’t want to squeeze.
(define (process-punc text [beg 0] [point 0])
  ;; It is possible for POINT to be greater than the string’s length,
  ;; because sometimes we add two to POINT.
  (if (>= point (string-length text))
      ;; Termination clause.
      (let ([text-before-including-point
             (substring text beg (min point (string-length text)))])
        (list text-before-including-point))
      ;; Recursion clause. In this clause it is guaranteed that
      ;; POINT < TEXT-LEN.
      (let* ([char (string-ref text point)]
             [text-len (string-length text)]
             [next-char (if (< point (sub1 text-len))
                            (string-ref text (add1 point))
                            #f)]
             [make-squeeze (lambda (mark)
                             (txexpr 'span
                                     '((class "squeeze full-width-mark"))
                                     (list mark)))]
             [make-full (lambda (mark)
                          (txexpr 'span
                                  '((class "full-width-mark"))
                                  (list mark)))])
        (cond
          ;; If this char is a LEFT cjk mark and the next one is also
          ;; a cjk mark, squeeze the left one.
          [(and (memq char squeezed-marks-left)
                ;; The test above must come first.
                (memq next-char squeezed-marks))
           (let* ([text-before-span (substring text beg point)]
                  [left-mark (make-squeeze (list->string (list char)))]
                  ;; Technically we don’t need to make marks other
                  ;; than curly quotes full-width, but making them all
                  ;; full-width is convenient.
                  [right-mark (make-full (list->string (list next-char)))])
             ;; TEXT-BEFORE-SPAN <span>LEFT-MARK</span> RIGHT-MARK REST
             (append (list text-before-span left-mark right-mark)
                     (process-punc text (+ 2 point) (+ 2 point))))]
          ;; If the next char is RIGHT cjk mark, and this char is a
          ;; cjk mark, squeeze the right one.
          [(and (memq next-char squeezed-marks-right)
                ;; The test above must come first.
                (memq char squeezed-marks))
           ;; TEXT-BEFORE-SPAN <span>RIGHT-MARK</span> REST
           (let* ([text-before-span (substring text beg point)]
                  ;; Same as above, we don’t need to make all marks
                  ;; full, but this is convenient.
                  [left-mark (make-full (list->string (list char)))]
                  [right-mark (make-squeeze
                               (list->string (list next-char)))])
             (append (list text-before-span left-mark right-mark)
                     (process-punc text (+ 2 point) (+ 2 point))))]
          ;; If the next char is not a cjk punctuation mark,
          ;; but this char is a curly quote that should be
          ;; full-width, we still need to annotate it.
          [(and (memq char (string->list "“”‘’"))
                ;; Previous or next char is CJK?
                (or (cjk? (string-ref text (max (sub1 point) 0)))
                    (and (< (add1 point) text-len)
                         (cjk? (string-ref text (add1 point))))))
           (let* ([text-before-span (substring text beg point)]
                  [mark (make-full (list->string (list char)))])
             (append (list text-before-span mark)
                     (process-punc text (add1 point) (add1 point))))]
          ;; Else just increment POINT.
          [else (process-punc text beg (add1 point))]))))

;;; RSS

(define (decode-org-timestamp timestamp)
  (string->date timestamp (if (eq? (string-length timestamp) 16)
                              "<~Y-~m-~d ~a>"
                              "<~Y-~m-~d ~a ~H:~M>")))

(define (rfc3339 timestamp)
  (date->string (decode-org-timestamp timestamp)
                "~Y-~m-~dT~H:~M:00.00-05:00"))
