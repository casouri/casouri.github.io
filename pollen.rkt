#lang racket

(provide link
         image
         bquote
         mono
         rt
         fnref
         fndef
         breadcrumb
         header-info
         header-line
         zh-footer
         like-button
         root
         ;; helper
         list-join
         rfc3339
         ;; vars
         author-en
         author-zh
         root-url)

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
 srfi/19
 net/uri-codec)

;;; Variable

(define author-zh "付禹安")
(define author-en "Yuan Fu")
(define root-url "https://archive.casouri.cat/")

;;; Helper

(define (list-join lst sep)
  (case (length lst)
    [(0) '()]
    [(1) lst]
    [else (append (list (car lst) sep)
                  (list-join (cdr lst) sep))]))

;; I give up on encoding url.
(define (normalize-uri uri)
  (string-normalize-nfc uri))

;;; Common markup

;; A URL link. ◊link[url]{text}. If TEXT is omited, use URL as text.
(define (link url . tx-elements)
  (let* ([url (normalize-uri url)]
         [tx-elements (if (empty? tx-elements)
                          (list url)
                          tx-elements)]
         [link-tx (txexpr 'a empty tx-elements)]
         [link-tx (attr-set link-tx 'href url)])
    link-tx))

;; An image. SRC is the path to the image.
(define (image src alt)
  (txexpr 'img `((src ,(normalize-uri src))
                 (alt ,alt))
          empty))

(define bquote (default-tag-function 'blockquote))

(define mono (default-tag-function 'span #:class "mono"))

(define code (default-tag-function 'span #:class "code"))

(define bcode (default-tag-function 'div #:class "code-block"))

(define (rt . text)
  (@ (txexpr 'rp empty '("("))
     (txexpr 'rt empty text)
     (txexpr 'rp empty '(")"))))

;; Footnote reference, ID can be either number or string. The footnote
;; reference shown on page isn't ID, but a automatically generated
;; number.
(define (fnref id)
  (let* ([ref-id (format "footref.~a" id)]
         [def-id (format "footdef.~a" id)]
         [counter (or (select-from-metas
                       'footnote-counter (current-metas))
                      1)]
         [id-display (format "~a" counter)])
    (define-meta 'footnote-counter (+ 1 footnote-counter))
    (txexpr 'span '((class "footnote"))
            (list (attr-set* (link (string-append "#" def-id) id-display)
                             'id ref-id
                             'class "inline-footref"
                             'aria-label "Jump to footnote")))))

;; Footnote definition. ID should match a previous footnote reference.
(define (fndef id . text)
  (let ([ref-id (format "footref.~a" id)]
        [def-id (format "footdef.~a" id)])
    (txexpr 'div `((class "footdef")
                   (id ,def-id))
            (list (txexpr 'div '((class "ref-footref"))
                          (list (attr-set
                                 (link (string-append "#" ref-id) "⭡")
                                 'aria-label "Jump back to main text")))
                  (txexpr 'div '((class "def-footdef"))
                          text)))))


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
            (let ([dir-title (select 'h1 (cached-doc index-page))])
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
;; txexpr.
(define (zh-footer)
  (let* ([author author-zh]
         [timestamp (select-from-metas 'date (current-metas))]
         [timestamp (list-ref (regexp-match #rx"<(.+)>" timestamp) 1)])
    (txexpr
     'div empty
     (list (txexpr 'p empty
                   (list (string-append "作者 " author)))
           (txexpr 'p empty
                   (list (string-append "写于 " timestamp)))
           (txexpr
            'p empty
            (list
             "评论 发邮件给 "
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

;;; Post processing

(define (root . elements)
  (txexpr 'root empty
          (decode-elements
           elements
           #:txexpr-elements-proc decode-paragraphs
           #:string-proc process-punc)))

;; If CHAR is a CJK character, return #t. “CJK character” is narrowly
;; defined as lying in 4E00-9FFF and 3000-303F.
(define (cjk? char)
  (let ([code (char->integer char)])
    (or (<= #x4E00 code #x9FFF) ; CJK Unified Ideographs
        (<= #x3000 code #x303F) ; CJK Symbols and Punctuation
        )))

(define squeezed-marks-left (string->list "，。、：；？！“《（『「〖【"))
(define squeezed-marks-right (string->list "”》）』」〗】"))
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
          ;; If this char is RIGHT cjk mark, and the next is a cjk
          ;; mark, squeeze the right one.
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
